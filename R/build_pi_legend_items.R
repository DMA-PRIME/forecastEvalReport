#' Build prediction interval legend items
#'
#' Constructs checkbox legend items for each prediction interval level
#' that has non-NULL bounds data. PI levels with NULL bounds are skipped
#' so the legend only reflects what is actually plotted.
#'
#' @param bounds A named list of PI bounds produced by
#'   `prepare_pi_bounds()`. Each entry is either a named list with
#'   `lower` and `upper` data frames, or `NULL`.
#' @param pi_styles A named list of PI style settings from
#'   `PLOT_STYLES$pi_styles`. Each entry must have `label` and
#'   `swatch_class`.
#'
#' @return A list of `htmltools` legend item tags, one per available
#'   PI level, or `NULL` if no PI levels have data.
#'
#' @keywords internal
#' @noRd
build_pi_legend_items <- function(bounds, pi_styles) {

#------------------------------------------------------------------------------#
# Preparing to build the PI legend item ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks to see if we can construct check box legend items #
# for each prediction interval level that has non-NULL bounds data. PI levels  #
# with NULL bounds are skipped so the legend only reflects what is actually    #
# plotted.                                                                     #
#------------------------------------------------------------------------------#

  ############################
  # Return NULL if no bounds #
  ############################
  if(is.null(bounds) || length(bounds) == 0) return(NULL)

  ################################
  # Return NULL if no plot style #
  ################################
  if(is.null(pi_styles) || length(pi_styles) == 0) return(NULL)

#------------------------------------------------------------------------------#
# Building legend items for the PI bounds --------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the legend items for the PI bounds when they are  #
# not NULL. It builds one legend item per PI level that has data.              #
#------------------------------------------------------------------------------#

  ####################################################
  # Build one legend item per PI level that has data #
  ####################################################
  items <- lapply(names(pi_styles), function(pi_name){

    # Skip if no bounds data for this PI level
    if(is.null(bounds[[pi_name]])) return(NULL)

    # Pulling the lower bound
    lower_data <- bounds[[pi_name]]$lower

    # Pulling the upper bound
    upper_data <- bounds[[pi_name]]$upper

    #########################
    # Handling missing data #
    #########################

    # Returning NULL if NULL lower/upper data
    if(is.null(lower_data) || is.null(upper_data)) return(NULL)

    # Returning NULL if no rows for lower/upper data
    if(nrow(lower_data) == 0 || nrow(upper_data) == 0) return(NULL)

    ###########################################
    # Build the legend item for this PI level #
    ###########################################
    build_legend_item(
      label        = pi_styles[[pi_name]]$label,
      trace_name   = paste0("PI-", pi_name),
      swatch_class = pi_styles[[pi_name]]$swatch_class,
      checked      = TRUE
    )

  })

  ########################################
  # Handling ANY null bound legend items #
  ########################################

  # Filtering out NULL values
  items <- Filter(Negate(is.null), items)

  # Returning NULL for no returned rows
  if(length(items) == 0) return(NULL)

  # Returning bounds
  items

}
