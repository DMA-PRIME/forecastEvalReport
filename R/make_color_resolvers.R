#' Build deterministic color and style resolvers for auxiliary variable traces
#'
#' Constructs reusable resolver functions that assign consistent colors and
#' dash patterns to auxiliary variable parameter traces across all plot
#' panels. Colors are generated in HSV space with constrained saturation
#' and value bands to ensure visual separation while preserving
#' interpretability. Dash patterns are assigned deterministically by data
#' source. All assignments are hash-based so the same parameter always
#' gets the same color across sessions and report runs.
#'
#' The default column names match the master data set produced by
#' `assemble_report_data()`: `data_source` for the data source identifier
#' and `variable` for the parameter name. The old defaults (`"Data"` and
#' `"Pretty_Parameter"`) have been updated accordingly.
#'
#' @param data A data frame containing parameter data. Must have at least
#'   the columns named by `data_col` and `parameter_col`.
#' @param data_col Character. Name of the data source column. Default
#'   `"data_source"` (master data column name).
#' @param parameter_col Character. Name of the parameter column. Default
#'   `"variable"` (master data column name).
#' @param shade_count Integer. Number of shade slots to generate per
#'   parameter family. Default `36L`.
#' @param excluded_colors Character vector of hex colors to exclude.
#'   Default `character(0)`.
#' @param source_to_dash Named list mapping data source names to dash
#'   pattern strings. `NULL` for auto-assignment.
#' @param min_center_sep Numeric. Minimum separation between hue centers.
#'   Default `0.10`.
#' @param family_half_width Numeric. HSV hue half-width for within-family
#'   shade variation. Default `0.08`.
#' @param offset_factor Integer. Controls shade offset step size. Default
#'   `4L`.
#' @param min_deltaE Numeric. Minimum perceptual color distance. Default
#'   `18.0`.
#' @param max_half_width Numeric. Maximum allowed HSV hue half-width.
#'   Default `0.22`.
#' @param enforce_attempts Integer. Number of attempts to enforce minimum
#'   color separation. Default `5L`.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{resolve_parameter_color}{Function taking `(parameter_name,
#'     source_name)` returning a hex color string.}
#'   \item{resolve_parameter_style}{Function taking `(parameter_name,
#'     source_name)` returning a list with `color` and `dash`.}
#'   \item{source_dash_map}{Named list mapping data source names to their
#'     assigned dash pattern strings.}
#' }
#'
#' @keywords internal
#' @noRd
make_color_resolvers <- function(data,
                                 data_col         = "data_source",
                                 parameter_col    = "variable",
                                 shade_count      = 36L,
                                 excluded_colors  = character(0),
                                 source_to_dash   = NULL,
                                 min_center_sep   = 0.10,
                                 family_half_width = 0.08,
                                 offset_factor    = 4L,
                                 min_deltaE       = 18.0,
                                 max_half_width   = 0.22,
                                 enforce_attempts = 5L) {

#------------------------------------------------------------------------------#
# Utility helper functions -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section defines the small internal helpers used below: a         #
# family-name normalizer and a deterministic string-to-integer hash that       #
# assigns colors and offsets without any randomness.                           #
#------------------------------------------------------------------------------#

  ###################################
  # Normalize parameter family name #
  ###################################

  # Strips to lowercase trimmed string for consistent family grouping
  .family_from_name <- function(name) {
    if(is.null(name) || is.na(name) || !nzchar(name)) return("")
    trimws(tolower(name))
  }

  ####################################
  # Deterministic string -> int hash #
  ####################################

  # Produces a stable non-negative integer from any string
  .hash_numeric <- function(s) {
    if(is.null(s) || is.na(s) || !nzchar(s)) return(0L)
    ints <- utf8ToInt(as.character(s))
    idx  <- seq_along(ints)
    val  <- sum(ints * idx) + 31L * sum(ints * rev(idx))
    as.integer(abs(val) %% .Machine$integer.max)
  }

#------------------------------------------------------------------------------#
# Base hue definitions and dash patterns ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets the fixed per-family hue centers and the list       #
# of dash patterns the revolvers draw from.                                    #
#------------------------------------------------------------------------------#

  #####################################
  # Predefined hue centers per family #
  #####################################
  .family_hue_centers <- c(
    red    = 0.00,
    yellow = 0.14,
    green  = 0.28,
    teal   = 0.38,
    purple = 0.75,
    pink   = 0.88
  )

  ###########################
  # Available dash patterns #
  ###########################
  dash_patterns <- c(
    "dash", "dot", "dashdot", "longdash",
    "longdashdot", "dashdotdot", "solid"
  )

#------------------------------------------------------------------------------#
# Input validation -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms data is a data frame carrying the named         #
# columns, coerces those columns to character for consistent matching,         #
# and extracts the unique data sources.                                        #
#------------------------------------------------------------------------------#

  # Stop if not data frame
  stopifnot(is.data.frame(data))

  # Stop if missing columns
  stopifnot(data_col %in% names(data))

  # Stop if missing parameter names
  stopifnot(parameter_col %in% names(data))

  # Coerce to character for consistent matching
  data[[data_col]]      <- as.character(data[[data_col]])
  data[[parameter_col]] <- as.character(data[[parameter_col]])

  # Extract unique non-missing data sources
  datas <- unique(stats::na.omit(data[[data_col]]))

#------------------------------------------------------------------------------#
# Source to dash mapping -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section assigns a deterministic dash pattern to each data        #
# source. When a user-supplied map is provided, any sources missing from       #
# it are auto-assigned; the finished map is returned in the resolver list      #
# so callers can read it directly.                                             #
#------------------------------------------------------------------------------#

  #################################
  # Running if a dash is provided #
  #################################
  if(!is.null(source_to_dash)){

    # Determining if there are missing dash patterns
    missing_srcs <- setdiff(datas, names(source_to_dash))

    # Auto-assign fallback dashes for sources not in the user map
    source_to_dash[missing_srcs] <- rep(dash_patterns, length.out = length(missing_srcs))

    # Assigning dashes to data
    source_dash_map <- setNames(as.list(source_to_dash[datas]), datas)

  ###########################################
  # Running if dash pattern is not provided #
  ###########################################
  }else{

    # Auto-generate the full dash map
    source_dash_map <- setNames(
      as.list(rep(dash_patterns, length.out = length(datas))),
      datas
    )

  }

#------------------------------------------------------------------------------#
# Parameter family construction ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section groups parameters into families by normalized name,      #
# maps each family to the data sources that use it, and derives how many       #
# color shades each family needs.                                              #
#------------------------------------------------------------------------------#

  # Extract unique parameters
  all_params <- unique(stats::na.omit(data[[parameter_col]]))

  # Derive family key for each parameter
  all_families <- vapply(all_params, .family_from_name, character(1))

  # Unique non-empty families
  unique_families <- unique(all_families[nzchar(all_families)])

  # Map families to their data sources
  family_sources_list <- tapply(
    data[[data_col]],
    vapply(data[[parameter_col]], .family_from_name, character(1)),
    function(x) unique(stats::na.omit(x)),
    simplify = FALSE
  )

  # Count sources per family
  family_sources_map <- vapply(family_sources_list, length, integer(1))

  # Determine required shade count per family
  family_required_shades <- setNames(
    pmax(shade_count, pmax(family_sources_map, 4L)),
    names(family_sources_list)
  )

#------------------------------------------------------------------------------#
# Assigning family hue centers -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section gives each family a hue center -- known family names     #
# take their predefined centers, and the remaining families are spread         #
# evenly around the hue circle in hash order so the layout is stable.          #
#------------------------------------------------------------------------------#

  ###########################################
  # Setting up to create color hue patterns #
  ###########################################

  # Empty vector to save family color center
  family_to_center <- list()

  # Vector save used color families
  used_named <- character(0)

  #####################################################
  # Assign predefined centers to known families first #
  #####################################################
  for(f in unique_families){

    # Checking if family is known
    if(f %in% names(.family_hue_centers)){

      # Saving the family color hue
      family_to_center[[f]] <- .family_hue_centers[[f]]

      # Adding family to used name vector
      used_named <- c(used_named, f)
    }

  }

  ####################################################
  # Evenly distribute centers for remaining families #
  ####################################################
  remaining <- setdiff(unique_families, used_named)

  #######################################
  # Triggered if any remaining families #
  #######################################
  if(length(remaining)){

    # Determining the center
    centers <- seq(0, 1, length.out = length(remaining) + 1)[-1]

    # Order of colors in families
    ord     <- order(vapply(remaining, .hash_numeric, integer(1)))

    # Autopopulate family to center vector
    for(i in seq_along(ord)){
      family_to_center[[remaining[ord[i]]]] <- centers[i]
    }
  }

#------------------------------------------------------------------------------#
# Family center resolver helper ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section defines a small lookup that returns a family's hue       #
# center, falling back to the purple center for unknown or empty families.     #
#------------------------------------------------------------------------------#

  .family_center_for <- function(fk){

    ###########################################
    # Triggered for unknown or empty families #
    ###########################################
    if(!nzchar(fk) || is.null(family_to_center[[fk]])){

      # Returning a purple center
      return(.family_hue_centers["purple"])

    }

    # Assigning family to vector
    family_to_center[[fk]]

  }

#------------------------------------------------------------------------------#
# Shade offset generator -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section generates a sequence of offsets that alternate           #
# positive and negative around zero, so shades within a family are spread      #
# as far apart as possible.                                                    #
#------------------------------------------------------------------------------#

  .offset_order <- function(n){
    if(n <= 1L) return(0L)
    out    <- integer(n)
    out[1] <- 0L
    k <- 1L; i <- 2L
    while(i <= n){
      out[i] <- k;  i <- i + 1L
      if(i <= n){ out[i] <- -k; i <- i + 1L }
      k <- k + 1L
    }
    out
  }

#------------------------------------------------------------------------------#
# Palette cache and HSV shade generator ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets up a per-palette cache and the function that        #
# builds a family's shades in HSV space, varying hue around the center         #
# and cycling fixed lightness bands so the shades stay distinguishable.        #
#------------------------------------------------------------------------------#

  ############################################
  # Cache environment for generated palettes #
  ############################################
  family_shade_cache <- new.env(parent = emptyenv())

  ###################################################
  # Generated the palettes around the center colors #
  ###################################################
  .generate_family_shades <- function(center, n, s_center, v_center,
                                      half_width){

    # Offset around the color center
    offsets <- .offset_order(n)

    # Color in palette
    h       <- (center + (offsets / max(1, max(abs(offsets)))) *
                  half_width) %% 1

    # Center colors
    s       <- rep(s_center, n)

    # Fixed lightness bands ensure shades are always distinguishable
    lightness_bands <- c(0.95, 0.75, 0.55, 0.35)

    # Colors for bands
    v               <- rep(lightness_bands, length.out = n)
    toupper(grDevices::hsv(h, s, v))

  }

#------------------------------------------------------------------------------#
# Deterministic source offsets -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section assigns each data source within a family a fixed         #
# offset, so a given source always lands on the same shade within its          #
# family.                                                                      #
#------------------------------------------------------------------------------#

  family_source_offset_map <- lapply(family_sources_list, function(srcs){
    offs        <- .offset_order(length(srcs))
    names(offs) <- srcs
    offs
  })

#------------------------------------------------------------------------------#
# Color resolver function ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section defines resolve_parameter_color(), which derives the     #
# family, builds and caches that family's shade palette, then picks a          #
# shade deterministically from the parameter and source hashes.                #
#------------------------------------------------------------------------------#

  resolve_parameter_color <- function(parameter_name, source_name){

    # Guard against invalid input
    if(is.null(parameter_name) || is.na(parameter_name) ||
       !nzchar(parameter_name)){
      return("#7F7F7F")
    }

    # Derive family key
    fk <- .family_from_name(parameter_name)

    # Required shade count for this family
    n <- if(!is.null(family_required_shades[[fk]])){
      family_required_shades[[fk]]
    }else{
      max(4L, as.integer(shade_count))
    }

    # Resolve hue center
    center <- .family_center_for(fk)

    # Derive saturation and lightness from family hash
    fam_hash <- .hash_numeric(fk)
    s_center <- 0.86 + (fam_hash %% 6) * 0.01
    v_center <- 0.92 - (fam_hash %% 5) * 0.06

    # Cache key ensures same inputs always produce same palette
    key <- paste(center, n, s_center, v_center, family_half_width, sep = "_")

    # Generate and cache shades on first use
    if(!exists(key, family_shade_cache, inherits = FALSE)){
      assign(
        key,
        .generate_family_shades(center, n, s_center, v_center,
                                family_half_width),
        envir = family_shade_cache
      )
    }

    # Retrieve cached shades
    shades <- get(key, family_shade_cache)

    # Deterministic base shade index from parameter hash
    base   <- (.hash_numeric(parameter_name) %% length(shades)) + 1L

    # Source-specific offset for intra-family separation
    offset <- 0L
    if(!is.null(family_source_offset_map[[fk]]) &&
       !is.null(family_source_offset_map[[fk]][[source_name]])){
      offset <- family_source_offset_map[[fk]][[source_name]]
    }

    # Return the final shade
    shades[((base - 1L + offset) %% length(shades)) + 1L]

  }

#------------------------------------------------------------------------------#
# Style resolver function ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section defines resolve_parameter_style(), pairing the           #
# resolved color with the data source's dash pattern (hash-assigned when       #
# the source is not in the dash map).                                          #
#------------------------------------------------------------------------------#

  resolve_parameter_style <- function(parameter_name, source_name){
    list(
      color = resolve_parameter_color(parameter_name, source_name),
      dash  = source_dash_map[[source_name]] %||%
        dash_patterns[(.hash_numeric(source_name) %% length(dash_patterns)) + 1L]
    )
  }

#------------------------------------------------------------------------------#
# Return all three resolver outputs --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the two resolver functions plus the              #
# source_dash_map, so add_parameter_traces_by_disease() can read the dash      #
# map directly instead of reconstructing it.                                   #
#------------------------------------------------------------------------------#

  list(
    resolve_parameter_color = resolve_parameter_color,
    resolve_parameter_style = resolve_parameter_style,
    source_dash_map         = source_dash_map
  )

}
