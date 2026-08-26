#' Build and append auxiliary variable traces to a Plotly figure
#'
#' Dynamically constructs parameter-level traces and appends them to a
#' Plotly object. Traces are grouped by data source and disease, with
#' colors and dash patterns resolved from a modular color resolver system.
#' Right y-axis assignment is determined from the variables crosswalk via
#' `is_right_axis_param()`.Legend group titles and hover labels are resolved
#' from the crosswalk via `get_aux_source_labels()`.
#'
#' All parameter traces are hidden on initial load (`visible = "legendonly"`)
#' and toggled on by the floating legend checkboxes.
#'
#' Column names match the master data set from `assemble_report_data()`:
#' `data_source`, `disease_name_clean`, `date`, `value`, `variable`.
#' The old function used `data`, `DISEASE`, `Date`, `Value`, `Pretty_Parameter`.
#'
#' @param p A Plotly object to add the parameter traces to.
#' @param data A data frame of auxiliary variable data for a single location,
#'   produced by `prepare_parameter_data()`. Must contain columns:
#'   `data_source`, `disease_name_clean`, `date`, `value`, `variable`.
#' @param variables_crosswalk A validated crosswalk data frame produced by
#'   `validate_variables_crosswalk()`. Used for right-axis and label lookups.
#' @param outcome Character. The outcome display label, used to determine
#'   right-axis assignment for percent-based outcomes.
#' @param shade_count Integer. Number of color shades to generate for the
#'   color resolver. Default `36L`.
#' @param excluded_colors Character vector of hex colors to exclude from
#'   the resolver palette. Default `c()`.
#' @param source_to_dash Named list mapping data source names to dash
#'   patterns. Default `NULL` (auto-assigned).
#' @param resolvers A pre-built resolvers object from `make_color_resolvers()`.
#'   Default `NULL` (built automatically from the data).
#' @param legendrank_start Integer. Starting legend rank for the first
#'   parameter trace. Default `5L`.
#' @param visible Character or logical. Initial visibility of the traces.
#'   Default `"legendonly"`.
#'
#' @return The Plotly object with all auxiliary variable traces added.
#'
#' @keywords internal
#' @noRd
add_parameter_traces_by_disease <- function(p,
                                            data,
                                            variables_crosswalk,
                                            outcome,
                                            shade_count      = 36L,
                                            excluded_colors  = c(),
                                            source_to_dash   = NULL,
                                            resolvers        = NULL,
                                            legendrank_start = 5L,
                                            visible          = "legendonly") {

#------------------------------------------------------------------------------#
# Column name constants --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section names the standardized master-data columns as            #
# constants, so they are easy to update if a future version renames them.      #
#------------------------------------------------------------------------------#

  # Column containing the data source identifier (legend group)
  data_col      <- "data_source"

  # Column containing the disease display name
  disease_col   <- "disease_name_clean"

  # Column containing the date of observation
  date_col      <- "date"

  # Column containing the observed value
  value_col     <- "value"

  # Column containing the variable/parameter name
  parameter_col <- "variable"

#------------------------------------------------------------------------------#
# Input validation -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms the data frame has all required columns         #
# before proceeding, returning p unchanged when any are missing so the         #
# plot still renders without errors.                                           #
#------------------------------------------------------------------------------#

  #############################
  # data must be a data frame #
  #############################
  if(!is.data.frame(data)){

    # Warning to show to users if data is wrong format
    warning(
      "add_parameter_traces_by_disease: `data` must be a data.frame. ",
      "Returning plot unchanged.",
      call. = FALSE
    )

    # Returning the plot
    return(p)

  }

  ####################################
  # Required columns must be present #
  ####################################

  # List of required columns
  required_cols <- c(data_col, disease_col, date_col, value_col, parameter_col)

  # Checking for missing columns
  missing_cols  <- setdiff(required_cols, names(data))

  # Triggered if there are columns missing
  if(length(missing_cols) > 0){

    # Warning to show to users
    warning(
      "add_parameter_traces_by_disease: missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Returning plot unchanged.",
      call. = FALSE
    )

    # Returning the plot
    return(p)

  }

#------------------------------------------------------------------------------#
# Coercing column types --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section coerces the data source and parameter columns to         #
# character to prevent type-related failures when filtering or comparing       #
# values.                                                                      #
#------------------------------------------------------------------------------#

  # Converting the data source column to character
  data[[data_col]]      <- as.character(data[[data_col]])

  # Converting the parameter name column to character
  data[[parameter_col]] <- as.character(data[[parameter_col]])

#------------------------------------------------------------------------------#
# Building a trimmed parameter label for hover text ----------------------------
#------------------------------------------------------------------------------#
# About: This section builds a cleaned parameter label for the hover           #
# tooltip -- looking up clean_name_full from the crosswalk and falling         #
# back to the trimmed raw name -- so the hover text stays concise.             #
#------------------------------------------------------------------------------#

  # Internal column name for the trimmed hover label
  trimmed_label_col <- ".param_label_for_hover"

  ###########################################################################
  # Strip trailing " (disease)" or " -..." suffixes from the parameter name #
  ###########################################################################
  data[[trimmed_label_col]] <- vapply(data[[parameter_col]], function(var){

    # Running if cross walk is available
    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk)){

      # Extracting auxiliary variable rows
      aux_rows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "aux_variable" &
          !is.na(variables_crosswalk$variable) &
          variables_crosswalk$variable == var, ]

      # Triggering if auxiliary variables are available
      if(nrow(aux_rows) > 0 &&
         !is.na(aux_rows$clean_name_full[1]) &&
         nchar(trimws(aux_rows$clean_name_full[1])) > 0){

        # Returning their clean names
        return(aux_rows$clean_name_full[1])

      }

    }

    # Fall back to trimmed raw name
    trimws(sub(" \\(.*$| -.*$", "", var))

  }, character(1))

#------------------------------------------------------------------------------#
# Resolver initialization ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds, or reuses, the color and style revolvers         #
# that assign consistent colors and dash patterns to each parameter            #
# trace. When no revolvers object is supplied, make_color_resolvers()          #
# constructs one from the data (and must be loaded in the environment).        #
#------------------------------------------------------------------------------#

  ######################################
  # Building revolvers if not provided #
  ######################################
  if(is.null(resolvers)){

    # Only running if color resolver is not provided
    if(!exists("make_color_resolvers", mode = "function")){

      # Stopping the script
      stop(
        "add_parameter_traces_by_disease: make_color_resolvers() not found. ",
        "Ensure it is loaded before calling this function.",
        call. = FALSE
      )
    }

    ##############################################
    # Build the resolver from the parameter data #
    ##############################################
    resolvers <- make_color_resolvers(
      data            = data,
      data_col        = data_col,
      parameter_col   = parameter_col,
      shade_count     = as.integer(shade_count),
      excluded_colors = excluded_colors,
      source_to_dash  = source_to_dash
    )

  }

  # Extracting parameter colors
  resolve_parameter_color <- resolvers$resolve_parameter_color

  # Extracting parameter style
  resolve_parameter_style <- resolvers$resolve_parameter_style

  # Extracting line dash types
  source_dash_map         <- resolvers$source_dash_map

#------------------------------------------------------------------------------#
# Initializing the legend rank counter -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section initializes the sequential legend-rank counter so        #
# the plotly legend order is deterministic; ranks start at                     #
# legendrank_start and increment by one per data source and per parameter.     #
#------------------------------------------------------------------------------#

  legend_rank <- legendrank_start

#------------------------------------------------------------------------------#
# Looping over data sources ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section loops over each unique data source, resolving its        #
# dash pattern and looking up its legend group title and hover label from      #
# the crosswalk, then drawing a trace for every parameter within it.           #
#------------------------------------------------------------------------------#

  ########################################
  # Creating unique list of data sources #
  ########################################
  data_sources <- unique(data[[data_col]])

  #######################################
  # Looping through unique data sources #
  #######################################
  for(d in data_sources){

    # Subset data to the current data source
    d_data <- data[data[[data_col]] == d, , drop = FALSE]

    # Skip if no rows
    if(nrow(d_data) == 0L) next

    # Increment legend rank for the data source group
    legend_rank <- legend_rank + 1L

    ##########################
    # Resolving dash pattern #
    ##########################
    dash_for_source <- NULL

    # Look up from source_dash_map if available
    if(!is.null(source_dash_map) &&
       as.character(d) %in% names(source_dash_map)){
      dash_for_source <- source_dash_map[[as.character(d)]]
    }

    # Fall back to style resolver
    if(is.null(dash_for_source)){
      dash_for_source <- resolve_parameter_style("", d)$dash
    }

    ##########################################
    # Resolving legend labels from crosswalk #
    ##########################################

    # Getting the entire list of source labels
    source_labels <- get_aux_source_labels(d, variables_crosswalk)

    # Pulling the legend group title
    legend_group_title_final <- source_labels$legend_group_title

    # Pulling the hover label
    data_label_hover <- source_labels$hover_label

    ############################################
    # Extract diseases within this data source #
    ############################################
    diseases <- unique(d_data[[disease_col]])

    #########################
    # Looping over diseases #
    #########################
    for(disease in diseases){

      # Subset data to the current disease
      disease_data <- d_data[d_data[[disease_col]] == disease, , drop = FALSE]

      # Skip if no rows
      if(nrow(disease_data) == 0L) next

      # Increment legend rank for this disease group
      legend_rank <- legend_rank + 1L

      # Extract unique parameter names within this disease group
      params <- unique(disease_data[[parameter_col]])

      ###########################
      # Looping over parameters #
      ###########################
      for(param in params){

        # Subset data to the current parameter
        param_data <- disease_data[
          disease_data[[parameter_col]] == param, , drop = FALSE]

        # Skip if no rows
        if(nrow(param_data) == 0L) next

        # Resolve color for this parameter
        color_val <- resolve_parameter_color(param, d)

        # Assign dash pattern from the data source resolver
        dash_val  <- dash_for_source

        ###########################
        # Right y-axis assignment #
        ###########################

        # Pulling if aux variable should be on right axis
        use_right_axis   <- is_right_axis_param(param, variables_crosswalk)

        # Determining the axis the aux variable should sit on
        yaxis_assignment <- if(use_right_axis) "y2" else "y"

        ###################################
        # Appending the trace to the plot #
        ###################################
        p <- p %>%
          plotly::add_trace(
            data       = param_data,
            x          = as.formula(paste0("~", date_col)),
            y          = as.formula(paste0("~", value_col)),
            type       = "scatter",
            mode       = "lines",

            # Trace name: parameter + disease tag for legend group matching
            name       = paste0(param, " (", disease, ")"),

            # Line styling from color and dash resolvers
            line       = list(
              dash  = dash_val,
              color = color_val,
              width = 1.5
            ),

            # Trimmed label for cleaner hover text
            text       = as.formula(paste0("~", trimmed_label_col)),
            customdata = "param",

            # y-axis assignment from crosswalk on_right_axis flag
            yaxis      = yaxis_assignment,

            # Legend group matches the data source title
            legendgroup = paste0(legend_group_title_final, "|", param),
            showlegend       = FALSE,

            # Hidden on initial load -- toggled via floating legend
            visible          = visible,

            legendgrouptitle = list(
              text = paste0("<b>", legend_group_title_final, "</b>")
            ),
            legendrank = legend_rank,

            # Hover template includes parameter, source, disease, week, value
            hovertemplate = paste0(
              "<b>%{text} (", data_label_hover, ")</b><br>",
              "Disease: ", disease, "<br>",
              "Week: %{x|%Y-%m-%d}<br>",
              "Value: %{y}<br>",
              if(use_right_axis) "(Right Axis)" else "",
              "<extra></extra>"
            )
          )

        # Increment legend rank after each parameter trace
        legend_rank <- legend_rank + 1L

      }
    }
  }

  ##############################
  # Returning the updated plot #
  ##############################
  p

}
