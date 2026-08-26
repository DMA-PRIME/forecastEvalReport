#' Render the main forecast visualization section
#'
#' Builds an interactive Plotly-based forecast visualization for each
#' location in the forecast file. Each plot shows the target data (truth
#' line), implementation model projections and prediction intervals,
#' historical estimates (when available), evaluation model traces (per
#' horizon), auxiliary variable traces, modeling period phase ribbons,
#' a current-week reference line, and a range slider.
#'
#' When only one location is present, a single plot is rendered without
#' a geography selector. When multiple locations are present, a dropdown
#' selector is shown above the plot and panels are toggled via JavaScript.
#'
#' All data comes from the master data set produced by
#' `assemble_report_data()`. Implementation and evaluation model filtering
#' uses raw location codes (e.g., "SC", "45") since those files have not
#' been normalized. Master data filtering uses normalized display names
#' since `assemble_report_data()` normalizes all locations.
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`,
#'   or `NULL`.
#' @param eval_meta Metadata list from `extract_evaluation_data()`,
#'   or `NULL`.
#' @param config Validated config list from `validate_report_params()`.
#' @param implementation_model Validated implementation model data frame,
#'   or `NULL`.
#' @param master_data Assembled master data frame from
#'   `assemble_report_data()`, or `NULL`.
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`.
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`.
#' @param location Optional. Restrict the plots to a single geography (or a
#'   subset): a display name (e.g. `"Upstate"`) or a positive integer index
#'   into the alphabetically-ordered geographies. `NULL` (default) builds every
#'   location. Used by the plot-only HTML export to render one geography.
#' @param return_plots Logical. When `TRUE`, the function skips HTML rendering
#'   and returns the named list of per-location plotly objects (keyed by
#'   display name) before any report-page legend JavaScript is attached, for
#'   programmatic export via [save_forecast_plot()]. Defaults to `FALSE`.
#' @param for_export Logical. When `TRUE`, the plots are prepared for a static
#'   image: the range slider is removed, the x-axis shows all data (no default
#'   zoom window), the y-axis is pinned to the full data range, every trace is
#'   turned on, and the plot's native legend is shown (on the right) instead of
#'   the floating legend. Fullscreen is kept so the static export can capture
#'   the sized view. Defaults to `FALSE`.
#'
#' @return Called for its side effect of rendering HTML via
#'   [htmltools::tagList()]. Returns `NULL` invisibly when no data is
#'   available to plot. When `return_plots = TRUE`, returns a named list of
#'   plotly objects instead (one per location), and renders no HTML.
#'
#' @keywords internal
#' @noRd
section_forecast_plots <- function(impl_meta,
                                   eval_meta,
                                   config,
                                   implementation_model = NULL,
                                   master_data          = NULL,
                                   variables_crosswalk  = NULL,
                                   plot_styles          = NULL,
                                   location             = NULL,
                                   return_plots         = FALSE,
                                   for_export           = FALSE,
                                   font_size            = NULL,
                                   view_start           = NULL,
                                   view_end             = NULL) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the function inputs to ensure they ar present.    #
# Both inputs are critical for the remainder of the script, so if they are     #
# not present or there is an issue, this section returns an error message and  #
# returns NULL.                                                                #
#------------------------------------------------------------------------------#

  ######################################
  # Preparing the plot styles: Default #
  ######################################
  if(is.null(plot_styles) || !is.list(plot_styles)){

    # Using default if user file not provided
    plot_styles <- forecastEvalReport::create_plot_styles()

  }

  ######################################
  # Guard: master_data must be present #
  ######################################
  if(is.null(master_data) || !is.data.frame(master_data) ||
     nrow(master_data) == 0){

    # Error to return to users
    message("section_forecast_plots: No master data available. Skipping.")

    # Stopping the script
    return(invisible(NULL))
  }

#------------------------------------------------------------------------------#
# Preparing the metadata -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the meta data to use, either the              #
# implementation or evaluation, normalizes the locations, and prepares the     #
# disease, outcome, and spatial scale labels.                                  #
#------------------------------------------------------------------------------#

  ################################
  # Pulling the meta data source #
  ################################
  meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

  ################################################
  # Normalized locations: Pulling from meta data #
  ################################################
  locations <- if(!is.null(meta$locations) && length(meta$locations) > 0){

    # List of locations
    meta$locations

  ##################################################
  # Normalized locations: Pulling from master data #
  ##################################################
  }else{

    # List of unique locations from master data set
    locs <- unique(master_data$location)

    # Assigning names
    setNames(locs, locs)

  }

  #########################
  # Outcome display label #
  #########################
  outcome_display <- if(!is.null(variables_crosswalk)){

    # Pulling the outcome rows
    out_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "outcome", ]

    # Triggered if multiple outcome rows
    if(nrow(out_rows) > 0){

      # Pulling cleaned names: User Provided
      clean <- unique(out_rows$clean_name_full)

      # Rooting out non-valid rows
      clean <- clean[!is.na(clean) & nchar(trimws(clean)) > 0 &
                       clean != "USER: provide a definition"]

      # Check for multiple outcomes: Single outcome
      if(length(clean) > 0) paste(clean, collapse = ", ") else
        paste(meta$outcome, collapse = ", ")

    # Check for multiple outcome: Multiple
    }else{paste(meta$outcome, collapse = ", ")}

  # Check or multiple outcome: Multiple
  }else{paste(meta$outcome, collapse = ", ")}

  #######################
  # Spatial scale label #
  #######################
  spatial_scale   <- if(!is.null(meta$spatial_scale)) meta$spatial_scale else ""

  #########################
  # Disease display label #
  #########################
  disease_display <- {

    # NULL vector to store disease
    dcl <- NULL

    # Pulling disease display: From Crosswalk
    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk) &&
       all(c("variable_type", "disease_name_clean") %in% names(variables_crosswalk))){

      # Pulling outcome rows
      drows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "outcome", ]

      # Triggered if there are outcome rows
      if(nrow(drows) > 0){

        # Unique disease names
        v <- unique(drows$disease_name_clean)

        # Cleaning up disease names
        v <- v[!is.na(v) & nzchar(v)]

        # Handling multiple disease names
        if(length(v) > 0) dcl <- paste(v, collapse = ", ")

      }

    }

    # Handling missing disease names
    if(!is.null(dcl)) dcl else if(!is.null(config$disease)) config$disease else ""
  }

#------------------------------------------------------------------------------#
# Availability flags -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the two flags indicating if we have a user       #
# provided evaluation model and or implementation model. This controls what    #
# elements are shown in the primary forecast plot.                             #
#------------------------------------------------------------------------------#

  #############################
  # Flag for evaluation model #
  #############################
  has_eval <- !is.null(eval_meta) &&
    !is.null(eval_meta$evaluation_model) &&
    is.data.frame(eval_meta$evaluation_model) &&
    nrow(eval_meta$evaluation_model) > 0

  #################################
  # Flag for implementation model #
  #################################
  has_impl <- !is.null(implementation_model) &&
    is.data.frame(implementation_model) &&
    nrow(implementation_model) > 0

#------------------------------------------------------------------------------#
# Eval style object ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the evaluation object that contains the styles   #
# for the evaluation model components of the plot. This includes the title,    #
# colors, line types/widths, and how to handle training data when provided.    #
#------------------------------------------------------------------------------#

  #########################
  # Evaluation style list #
  #########################
  eval_style <- list(
    legend_title   = plot_styles[["Evaluation Model"]]$legend_title,
    colors         = plot_styles$colors_split,
    line_types     = plot_styles$lineType_split,
    line_widths    = plot_styles$lineSize_split,

    # Training data line style (falls back inside build_training_trace if NULL)
    training_color = plot_styles$training_color,
    training_width = plot_styles$training_width
  )

#------------------------------------------------------------------------------#
# Normalize location keys across sources ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section aligns the location columns of the implementation        #
# model, evaluation model, and historical data onto the canonical display      #
# names the extractors already resolved (impl_meta$locations /                 #
# eval_meta$locations), so impl, eval, historical, and master_data all key     #
# on one value. Codes the maps do not cover keep their original value          #
# (the file default).                                                          #
#------------------------------------------------------------------------------#

  ################################################
  # Map a raw location column through a name map #
  ################################################
  normalize_location_column <- function(x, location_map){

    # No map available -- return the column unchanged
    if(is.null(location_map) || length(location_map) == 0){
      return(as.character(x))
    }

    # Look up each raw code's display name in the map
    out <- unname(location_map[as.character(x)])

    # Fallback: keep the original value where the map has no entry
    out[is.na(out)] <- as.character(x)[is.na(out)]

    # Returning the canonical display names
    out

  }

  ######################################
  # Normalize the implementation model #
  ######################################
  if(has_impl){
    implementation_model$location <- normalize_location_column(
      implementation_model$location, impl_meta$locations
    )
  }

  #################################
  # Normalize the historical data #
  #################################
  if(!is.null(impl_meta$historical_data) &&
     is.data.frame(impl_meta$historical_data)){
    impl_meta$historical_data$location <- normalize_location_column(
      impl_meta$historical_data$location, impl_meta$locations
    )
  }

  ##################################
  # Normalize the evaluation model #
  ##################################
  if(has_eval && !is.null(eval_meta$evaluation_model) &&
     is.data.frame(eval_meta$evaluation_model)){
    eval_meta$evaluation_model$location <- normalize_location_column(
      eval_meta$evaluation_model$location, eval_meta$locations
    )
  }

  ############################################
  # Re-key the location loop by display name #
  ############################################
  # raw_loc (names(locations)) now equals loc_display, so every "== raw_loc"
  # filter downstream matches the normalized columns above.
  names(locations) <- unname(locations)

#------------------------------------------------------------------------------#
# Optional location filter -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: When a location is requested, the loop is narrowed to just that       #
# geography (matched by display name, or by index into the alphabetically-     #
# ordered names, matching save_forecast_plot()). This lets the HTML export     #
# render a single geography. NULL (default) keeps every location.              #
#------------------------------------------------------------------------------#

  ##################################
  # Narrowing to a chosen location #
  ##################################
  if(!is.null(location)){

    # Display names in the same alphabetical order used elsewhere
    avail <- sort(unname(locations))

    ###############################
    # Resolving an index to names #
    ###############################
    if(is.numeric(location)){

      # Guarding the index range
      if(any(location < 1) || any(location > length(avail))){
        stop("section_forecast_plots(): location index is out of range. ",
             "Available: ", paste(avail, collapse = ", "), call. = FALSE)
      }

      # Selected display name(s)
      chosen <- avail[location]

    ###############################
    # Resolving a name directly   #
    ###############################
    }else{

      # Matching requested names against the available display names
      chosen <- as.character(location)
      if(!all(chosen %in% avail)){
        stop("section_forecast_plots(): location(s) not found: ",
             paste(setdiff(chosen, avail), collapse = ", "),
             ". Available: ", paste(avail, collapse = ", "), call. = FALSE)
      }

    }

    # Subsetting the loop to the chosen geography(ies)
    locations <- locations[unname(locations) %in% chosen]

  }

#------------------------------------------------------------------------------#
# Main plot loop ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the master plot loop that either creates a       #
# single plot, when there is 1 location or multiple plots when there is more   #
# than one location. The goal of this is to ensure that the function is        #
# dynamic to the user's needs and contains all information provided by the     #
# users within one report.                                                     #
#------------------------------------------------------------------------------#

  #############################
  # Empty list to store plots #
  #############################
  plotly_list <- list()

  ##################################
  # Empty list to store hover text #
  ##################################
  hover_text_list <- list()

  ##########################
  # Empty plot to store ID #
  ##########################
  plot_id <- "plot1"

  ################
  # Plot counter #
  ################
  plot_count <- 0L

  #############################
  # Looping through locations #
  #############################
  for(i in seq_along(locations)){

#------------------------------------------------------------------------------#
# Location identifiers ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the location identifiers for the drop down and  #
# and extraction throughout the scripts. It determines the normalized named    #
# and the raw code names. The normalized name is used to filter the master     #
# data and the figure labels; the raw location name is used to filter the      #
# implementation and evaluation models.                                        #
#------------------------------------------------------------------------------#

    ########################################
    # Pulling the normalized location name #
    ########################################
    loc_display <- locations[[i]]

    #################################
    # Pulling the raw location name #
    #################################
    raw_loc <- names(locations)[i]

    ######################################
    # Ensuring raw location is populated #
    ######################################
    if(is.null(raw_loc) || is.na(raw_loc) || nchar(trimws(raw_loc)) == 0){

      # Populated raw location name with normalized location name
      raw_loc <- loc_display

    }

#------------------------------------------------------------------------------#
# Extracting the truth data ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section extracts the truth data from the master data set. This   #
# is always plotted in the figure. If it is not available for a given plot,    #
# the location is skipped.                                                     #
#------------------------------------------------------------------------------#

    #############################
    # Extracting the truth data #
    #############################
    truth_data <- master_data[
      !is.na(master_data$variable_type) &
        master_data$variable_type == "outcome_data" &
        !is.na(master_data$location) &
        master_data$location == loc_display, ]

    ####################################################
    # Skipping location if truth data is not available #
    ####################################################
    if(nrow(truth_data) == 0) next

#------------------------------------------------------------------------------#
# Creating the hover text label ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section constructs the hover label shown when a user hovers over #
# the target (truth) data trace on a plot. The label is derived from the       #
# abbreviated name of the data source row in the variables crosswalk that is   #
# referenced by the outcome row for the current geography.                     #
#------------------------------------------------------------------------------#

    ###########################
    # Building the hover text #
    ###########################
    hover_text <- build_hover_text(
      location            = loc_display,
      variables_crosswalk = variables_crosswalk,
      config              = config
    )

#------------------------------------------------------------------------------#
# Percent scaling for truth data -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines whether a given variable's values should be   #
# multiplied by 100 (percent conversion) by looking up the `convert_percent`   #
# flag in the variables crosswalk. If `convert_percent` is TRUE for the        #
# variable, all values in the specified column are multiplied by 100 and       #
# rounded to three decimal places. A suffix string is also returned for use in #
# hover text and axis labels.                                                  #
#------------------------------------------------------------------------------#

    ################################
    # Pulling the final truth data #
    ################################
    scaled_truth <- apply_percent_scaling(
      data                = truth_data,
      col                 = "value",
      variable            = truth_data$variable[1],
      variables_crosswalk = variables_crosswalk
    )

    # Truth data to plot
    truth_data   <- scaled_truth$data

    # Truth data variable suffix
    value_suffix <- scaled_truth$suffix

#------------------------------------------------------------------------------#
# Initializing the Plotly figure -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section initializing the plotly figure and applies the color,    #
# width and height selections included in the plotly styles file. This is      #
# done for each plotly in the loop.                                            #
#------------------------------------------------------------------------------#

    ##################################
    # Initializing the plotly object #
    ##################################
    p <- plotly::plot_ly(
      colors = plot_styles$colors_split,
      width  = plot_styles$plot_width,
      height = plot_styles$plot_height
    )

#------------------------------------------------------------------------------#
# Truth trace ------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adds the target data trace to the plotly object          #
# currently indexed in the loop. The trace is styled using the `Target Data`   #
# entry in `plot_styles` via the `resolve_line_style()` and                    #
# `resolve_marker_style()` functions.                                          #
#------------------------------------------------------------------------------#

    ############################
    # Building the truth trace #
    ############################
    p <- build_truth_trace(
      p            = p,
      truth_data   = truth_data,
      outcome      = outcome_display,
      hover_text   = hover_text,
      value_suffix = value_suffix,
      plot_styles  = plot_styles
    )

#------------------------------------------------------------------------------#
# Prediction interval bounds ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the prediction interval bounds traces that show  #
# in the main forecast plots. This includes preparing the bound groups, as     #
# these are user provided and the traces themselves. Both sections rely        #
# strongly on the plot styles provided by the user.                            #
#------------------------------------------------------------------------------#

    ######################
    # NULL bounds vector #
    ######################
    bounds <- NULL

    ################################################
    # Running if implementation model is available #
    ################################################
    if(has_impl){

      #######################
      # Preparing PI Bounds #
      #######################
      bounds <- prepare_pi_bounds(
        implementation.model = implementation_model,
        geography            = raw_loc,
        quantile_pairs       = plot_styles$quantile_pairs,
        outcome.data.label   = config$outcome_data_label
      )

      #########################
      # Building the PI trace #
      #########################
      p <- build_pi_bounds_trace(
        p            = p,
        bounds       = bounds,
        pi_styles    = plot_styles$pi_styles,
        outcome      = outcome_display,
        value_suffix = value_suffix
      )

    }

#------------------------------------------------------------------------------#
# Historical estimates trace ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the historical estimates trace for the plotly    #
# figure if and only if estimates are available to be plotted. Otherwise, this #
# code is not run and the trace does not show in the figure.                   #
#------------------------------------------------------------------------------#

    ##################################################
    # Empty data frame to store historical estimates #
    ##################################################
    historical_data <- data.frame()

    ############################################
    # Trying to pull historical estimates data #
    #############################################
    if(has_impl && !is.null(impl_meta$historical_data) &&
       is.data.frame(impl_meta$historical_data) &&
       nrow(impl_meta$historical_data) > 0){

      # Trying to subset historical data
      historical_data <- impl_meta$historical_data[
        !is.na(impl_meta$historical_data$location) &
          impl_meta$historical_data$location == raw_loc, ]

      ###############################################
      # Running if historical estimates are present #
      ###############################################
      if(nrow(historical_data) > 0){

        # Applying percent scaling
        scaled_hist     <- apply_percent_scaling(
          data                = historical_data,
          col                 = "value",
          variable            = historical_data$outcome_measure[1],
          variables_crosswalk = variables_crosswalk
        )

        # Re-assigning new scaled data
        historical_data <- scaled_hist$data

        #######################################
        # Building historical estimates trace #
        #######################################
        p <- build_estimate_output_trace(
          p             = p,
          estimate.data = historical_data,
          outcome       = outcome_display,
          value_suffix  = value_suffix,
          plot_styles   = plot_styles
        )

      }
    }

#------------------------------------------------------------------------------#
# Current projections trace ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the projections data for each location, and the #
# trace which shows the point projections. It first prepares the data, then    #
# determines if the percent scaling should be applied, it then builds the      #
# trace. If no implementation model has been provided, this section is         #
# skipped.                                                                     #
#------------------------------------------------------------------------------#

    #######################################
    # Empty data frame to store forecasts #
    #######################################
    forecast_data <- data.frame()

    #########################################
    # Checking for the implementation model #
    #########################################
    if(has_impl){

      # Preparing the forecast data for plotting
      forecast_data <- prepare_forecast_data(
        implementation.model = implementation_model,
        geography            = raw_loc,
        reason               = config$reason
      )

      #########################################
      # Running if forecast data is available #
      #########################################
      if(nrow(forecast_data) > 0){

        # Scaling the forecast data
        scaled_fc     <- apply_percent_scaling(
          data                = forecast_data,
          col                 = "value",
          variable            = forecast_data$outcome_measure[1],
          variables_crosswalk = variables_crosswalk
        )

        # Scaled (or not scaled) forecast data
        forecast_data <- scaled_fc$data

        # Suffix for scaled or not scaled forecast data
        value_suffix  <- scaled_fc$suffix

        ##################################
        # Creating the projections trace #
        ##################################
        p <- build_model_output_trace(
          p             = p,
          forecast.data = forecast_data,
          outcome       = outcome_display,
          value_suffix  = value_suffix,
          plot_styles   = plot_styles
        )

      }

    }

#------------------------------------------------------------------------------#
# Evaluation model traces ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the evaluation model traces, including scaling  #
# the results if an evaluation model is provided. It first extracts the        #
# evaluation model results, then checks to see if percent scaling must         #
# occur and then finally creates the trace.                                    #
#------------------------------------------------------------------------------#

    #####################################
    # Creating th evaluation data frame #
    #####################################
    evaluation_temp <<- data.frame()

    #################################################
    # Running only if evaluation model is available #
    #################################################
    if(has_eval){

      # Filtering the evaluation data
      evaluation_temp <- eval_meta$evaluation_model[
        !is.na(eval_meta$evaluation_model$location) &
          eval_meta$evaluation_model$location == raw_loc, ]

      ################################################
      # Running only if evaluation model is provided #
      ################################################
      if(nrow(evaluation_temp) > 0){

        # Checking if percent scaling should be applies
        scaled_eval     <- apply_percent_scaling(
          data                = evaluation_temp,
          col                 = "value",
          variable            = evaluation_temp$outcome_measure[1],
          variables_crosswalk = variables_crosswalk
        )

        # saving the results of the scaling
        evaluation_temp <- scaled_eval$data

        #######################################
        # Building the evaluation model trace #
        #######################################
        p <- build_eval_model_trace(
          p               = p,
          evaluation_temp = evaluation_temp,
          outcome         = outcome_display,
          value_suffix    = value_suffix,
          eval_style      = eval_style
        )

      }
    }

#------------------------------------------------------------------------------#
# Training data trace ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the training data when provided as a row in the #
# master data. It is plotted as a solid line under the Evaluation Model legend #
# heading, hidden by default, with the check-box labeled by the training data  #
# source clean name.                                                           #
#------------------------------------------------------------------------------#

    ############################################
    # Running only if master data is available #
    ############################################
    if(!is.null(master_data)){

      # Filtering for the training data
      training_temp <- master_data[
        !is.na(master_data$variable_type) &
          master_data$variable_type == "training_data" &
          !is.na(master_data$location) &
          master_data$location == loc_display, ]

      #########################################
      # Running if training data is available #
      #########################################
      if(nrow(training_temp) > 0){

        # Applying the percent scaling
        scaled_training <- apply_percent_scaling(
          data                = training_temp,
          col                 = "value",
          variable            = training_temp$variable[1],
          variables_crosswalk = variables_crosswalk
        )

        # Saving the adjusted or non adjusted data
        training_temp <- scaled_training$data

        ########################################
        # Building the trace for training data #
        ########################################
        p <- build_training_trace(
          p             = p,
          training_data = training_temp,
          data_source   = config$training_data_source,
          outcome       = outcome_display,
          value_suffix  = scaled_training$suffix,
          eval_style    = eval_style
        )

      }

    }

#------------------------------------------------------------------------------#
# Auxiliary variable traces ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the auxiliary variable traces for plotting in   #
# the main figure. It first prepares the parameter data from the master data   #
# set, and then creates traces for each auxiliary variable provided by the     #
# user.                                                                        #
#------------------------------------------------------------------------------#

    ################################
    # Preparing the parameter data #
    ################################
    param_data <- prepare_parameter_data(master_data, loc_display)

    #######################################
    # Runs if parameter data is available #
    #######################################
    if(!is.null(param_data)){

      ##########################################
      # Creating the auxiliary parameter trace #
      ##########################################
      p <- add_parameter_traces_by_disease(
        p                   = p,
        data                = param_data,
        variables_crosswalk = variables_crosswalk,
        outcome             = outcome_display
      )

    }

#------------------------------------------------------------------------------#
# Current week reference line --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the "current week" reference line. The current    #
# week is the week in which the forecast is conducted. It follows the hub      #
# verse definition of the reference week. The current week is only plotted     #
# when the implementation file is provided by the user.                        #
#------------------------------------------------------------------------------#

    ####################################
    # Checking for implementation file #
    ####################################
    if(has_impl){

      # Building the current week trace
      p <- build_current_week_line(p, implementation_model)

    }

#------------------------------------------------------------------------------#
# Setting maxPhase value for the rangeslider JS --------------------------------
#------------------------------------------------------------------------------#
# About: This section determines what the max height should be for the range   #
# slider in the plot. It looks at the forecast data, evaluation data, and      #
# truth data to determine the max value of the figure. A bit of padding is     #
# added before the max phase value is returned.                                #
#------------------------------------------------------------------------------#

    #############################################
    # Creating the list of values in the figure #
    #############################################
    vals <- c(
      if(nrow(forecast_data)   > 0) forecast_data$value   else NULL,
      if(nrow(evaluation_temp) > 0) evaluation_temp$value else NULL,
      if(nrow(truth_data)      > 0) truth_data$value      else NULL
    )

    #############################
    # Calculating the max value #
    #############################
    maxPhase_padded <- if(length(vals) > 0 && any(!is.na(vals))){

      # Max value
      max(vals, na.rm = TRUE) * 1.1

    #####################
    # Default max value #
    #####################
    }else{1000}

    # Saving the max value in the plot object
    p$x$maxPhase <- maxPhase_padded

#------------------------------------------------------------------------------#
# Phase ribbons ----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the phase ribbons for each phase of the model    #
# development. This includes the training, validation, and testing periods.    #
# If there is no evaluation file provided, this trace does not run.            #
#------------------------------------------------------------------------------#

    #####################################################
    # Checking if the evaluation file has been provided #
    #####################################################
    if(has_eval && nrow(evaluation_temp) > 0){

      # Building the training phase
      p <- build_phase_ribbon(p, evaluation_temp, "Training Period",
                              plot_styles$phase_colors[["Training Period"]])

      # Building the validation phase
      p <- build_phase_ribbon(p, evaluation_temp, "Validation Period",
                              plot_styles$phase_colors[["Validation Period"]])

      # Build the testing phase
      p <- build_phase_ribbon(p, evaluation_temp, "Testing Period",
                              plot_styles$phase_colors[["Testing Period"]])

    }

#------------------------------------------------------------------------------#
# Range slider -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the range slider that sits under the plot. It    #
# using the forecast, evaluation, and truth files to determine what the        #
# possible max and min dates are and then adds a week cushion. The start date  #
# is the end view date minus the view weeks selected by the user in the plot   #
# style. Finally, the start and end values and slider background are applied   #
# to the plot via layout().                                                    #
#------------------------------------------------------------------------------#

    #########################################
    # Running if forecast data is available #
    #########################################
    if(nrow(forecast_data) > 0){

      # Calculating the end view date
      end_view <- max(anytime::anydate(forecast_data$target_end_date)) +
        lubridate::weeks(1)

    ###########################################
    # Running if evaluation data is available #
    ###########################################
    }else if(has_eval && nrow(evaluation_temp) > 0){

      # Calculating the end view date
      end_view <- max(anytime::anydate(evaluation_temp$target_end_date)) +
        lubridate::weeks(1)

    #########################################
    # Default data to set the end view date #
    #########################################
    }else{end_view <- max(truth_data$date, na.rm = TRUE) + lubridate::weeks(1)}

    ###################################
    # Calculating the start view date #
    ###################################
    start_view <- end_view - lubridate::weeks(plot_styles$default_view_weeks)

    ##################################
    # Applying the range view layout #
    ##################################
    # The interactive report zooms to the most recent weeks and uses the
    # range slider to pan; a static export shows all data with no slider.
    xaxis_layout <- list(
      rangeslider = list(
        visible = !isTRUE(for_export),
        yaxis   = list(range = c(0, maxPhase_padded)),
        bgcolor = plot_styles$rangeslider_bgcolor
      ),
      title      = "",
      tickformat = "%Y-%m-%d"
    )

    # Default view window only when interactive (export shows all data)
    if(!isTRUE(for_export)){
      xaxis_layout$range <- c(start_view, end_view)
    }

    p <- p %>% plotly::layout(xaxis = xaxis_layout)

    # Adjusting the color of layout (slider only)
    if(!isTRUE(for_export)){
      p$x$layout$xaxis$rangeslider$unselected <- list(opacity = 0)
    }

    # Applying the range slider padding
    p$x$maxPhase <- maxPhase_padded

#------------------------------------------------------------------------------#
# Y-axis layout ----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the left and right axis labels. While the       #
# default is a left axis label, the users can specify a right axis label as    #
# well.                                                                        #
#------------------------------------------------------------------------------#

    #################################
    # Building the left axis layout #
    #################################
    p <- build_yaxis_layout(
      p             = p,
      outcome       = outcome_display,
      disease       = disease_display,
      geography     = loc_display,
      spatial.scale = spatial_scale
    )

    ##################################
    # Building the right axis layout #
    ##################################
    p <- add_right_yaxis(
      p,
      outcome     = outcome_display,
      plot_styles = plot_styles,

      # In a static export the range-slider JS that ranges y2 is gone, and the
      # default autorange = FALSE (with no range) leaves y2 tiny, so aux traces
      # on the right axis fall off-screen. Autorange y2 to the data for exports.
      autorange   = isTRUE(for_export)
    )

    ##########################################
    # Static export: pin the left y-axis     #
    ##########################################
    # The visible y-range is normally driven by the range slider's JS, which
    # is gone in a static export, so pin it to the full data range here. (The
    # right axis is handled by autorange = TRUE on the add_right_yaxis() call
    # above, since y2 cannot be pinned to a single value the way y1 can.)
    if(isTRUE(for_export)){
      p <- plotly::layout(p, yaxis = list(range = c(0, maxPhase_padded)))
    }

    ##########################################
    # Whole-figure font size (export option) #
    ##########################################
    # font_size (points) is set on the global font -- which cascades to tick
    # labels, the legend, and hover -- and explicitly on each axis's tick and
    # title font so it wins even where the y-axis builder sized them. Plotly
    # merges these into the existing axes, so titles/ranges are preserved.
    if(!is.null(font_size)){
      p <- plotly::layout(
        p,
        font   = list(size = font_size),
        legend = list(font = list(size = font_size)),
        xaxis  = list(tickfont  = list(size = font_size),
                      titlefont = list(size = font_size)),
        yaxis  = list(tickfont  = list(size = font_size),
                      titlefont = list(size = font_size)),
        yaxis2 = list(tickfont  = list(size = font_size),
                      titlefont = list(size = font_size))
      )
    }

    ##########################################
    # Initial x-view window (export option)  #
    ##########################################
    # view_start / view_end set only the opening x-range; all data stays in the
    # figure. Merged into the x-axis, so its title/tickformat are preserved.
    if(!is.null(view_start) && !is.null(view_end)){
      p <- plotly::layout(
        p,
        xaxis = list(range = c(as.Date(view_start), as.Date(view_end)))
      )
    }

#------------------------------------------------------------------------------#
# Rangeslider JS ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reads the bundled `plotly_fullscreen.js` file from the   #
# package installation using `system.file()`, attaches it to a Plotly widget   #
# via `htmlwidgets::onRender()`, and adds a custom expand button to the Plotly #
# modebar. Clicking the button toggles the plot between its normal size and    #
# a full-viewport overlay.                                                     #
#------------------------------------------------------------------------------#

    #########################
    # Attaching rangeslider #
    #########################
    p <- attach_rangeslider_js(p)

#------------------------------------------------------------------------------#
# Plot dimensions --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section adjusts the layout of the plot by hiding the legend and  #
# turning off the auto-size feature. Therefore, the plot will defer to the     #
# size indicated by the user.                                                  #
#------------------------------------------------------------------------------#

    #############################
    # Adjusting the plot layout #
    #############################
    p <- p %>%
      plotly::layout(
        showlegend = FALSE,
        autosize   = FALSE
      )

    ##########################################
    # Static export: show all data, make     #
    ##########################################
    # The static image uses the report's own floating legend, which the export
    # CSS turns into a side panel in its own right-hand column (outside the
    # plot), so the native legend stays off. Every trace is turned on so all
    # data shows. The right margin only needs room for add_right_yaxis (ticks
    # and title), not the legend, so a modest reserve is enough.
    if(isTRUE(for_export)){
      p <- plotly::style(p, visible = TRUE)
      p <- plotly::layout(p, margin = list(r = 110))
    }

#------------------------------------------------------------------------------#
# Fullscreen button ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the "full screen" button functionality for the    #
# main plots. This includes allowing both expansion and shrinkage of the plot. #
#------------------------------------------------------------------------------#

    ###################################
    # Building the full screen button #
    ###################################
    p <- build_fullscreen_button(p)

    # Static export: build_fullscreen_button() re-enables the modebar via
    # plotly::config(), so hide it again here so the image has no header
    if(isTRUE(for_export)){
      p <- plotly::config(p, displayModeBar = FALSE)
    }

#------------------------------------------------------------------------------#
# Store plot and hover text ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section stores the plot and hover text for each location in the  #
# loop. It increases the plot count index, save the current plot, adds the     #
# location name and adds the hover text.                                       #
#------------------------------------------------------------------------------#

    ###############################
    # Increasing the plotly count #
    ###############################
    plot_count <- plot_count + 1L

    ###########################
    # Saving the current plot #
    ###########################
    plotly_list[[plot_count]]      <- p

    ###########################
    # Naming the current plot #
    ###########################
    names(plotly_list)[plot_count] <- loc_display

    ##############################################
    # Saving the hover text for the current plot #
    ##############################################
    hover_text_list[[plot_count]]  <- hover_text

  }

  ##################################
  # Checking for no rendered plots #
  ##################################
  if(length(plotly_list) == 0){

    # Message to return to users
    message("section_forecast_plots: No plots built. Returning NULL.")

    # Returning NULL
    return(invisible(NULL))

  }

  ######################################################
  # Confirm hover_text_list matches plotly_list length #
  ######################################################
  if(length(hover_text_list) != length(plotly_list)){

    # Warning to show to users
    warning("section_forecast_plots: hover_text_list length mismatch -- truncating to plotly_list length.")

    # Pulling the hover text list
    hover_text_list <- hover_text_list[seq_along(plotly_list)]

  }

#------------------------------------------------------------------------------#
# Sorting plots alphabetically -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sorts the plots alphabetically, therefore, it is easy    #
# to find a plot in the drop down menu. If there is only one location in the   #
# file, this does not apply given there is not a list to sort.                 #
#------------------------------------------------------------------------------#

  ##########################
  # Order to sort plots by #
  ##########################
  sort_order <- order(names(plotly_list))

  ###############################
  # Sorting the plotly elements #
  ###############################

  # Sorting the plotly list
  plotly_list <- plotly_list[sort_order]

  # Sorting the hover text list
  hover_text_list <- hover_text_list[sort_order]

#------------------------------------------------------------------------------#
# Optional return: raw plotly objects ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the named list of per-location plotly objects    #
# when the caller sets return_plots = TRUE. It exits before the report-page    #
# legend-sync JavaScript is attached below, so the returned widgets are        #
# standalone and safe to export (e.g. via save_forecast_plot()) without        #
# depending on globals that exist only inside the rendered report page.        #
#------------------------------------------------------------------------------#

  ################################
  # Returning raw plotly objects #
  ################################
  if(isTRUE(return_plots)){

    # Named list of plotly objects, one per location
    return(plotly_list)

  }

#------------------------------------------------------------------------------#
# Legend data ------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the inputs the legend needs before the          #
# per-location plot loop runs, so the legend is built once from stable values  #
# rather than from loop-scoped state. If a location lacks a given element      #
# (evaluation horizons, training data, parameters, or PI bounds), the matching #
# legend item is gated off downstream.                                         #
#------------------------------------------------------------------------------#

  ##############################
  # Unique evaluation horizons #
  ##############################

  # Pulling unique horizons from evaluation model
  unique_horizons <- if(has_eval){unique(eval_meta$evaluation_model$horizon)

  # No evaluation model to pull from
  }else{NULL}

  ###########################################################
  # Running if master data is available, with training data #
  ###########################################################
  has_training <- !is.null(master_data) &&
    any(!is.na(master_data$variable_type) &
          master_data$variable_type == "training_data")
  training_source_name <- if(has_training &&
                             !is.null(config$training_data_source) &&
                             !all(is.na(config$training_data_source))){

    # Training data name for legend
    as.character(config$training_data_source)[1]

  #############################
  # No training data provided #
  #############################
  }else{NULL}

  #################################
  # Parameter (aux) data presence #
  #################################
  all_param_data <- if(!is.null(master_data)){

    # Pulling auxiliary data
    master_data[!is.na(master_data$variable_type) &
                  master_data$variable_type == "aux_data", ]

  #####################
  # No auxiliary data #
  #####################
  }else{NULL}

  ######################################
  # Pulling location specific aux data #
  ######################################

  # Has aux data indicator
  has_params <- !is.null(all_param_data) && nrow(all_param_data) > 0

  # Determining locations that have parameters
  location_has_params <- setNames(
    vapply(names(plotly_list), function(loc){
      if(!has_params) return(FALSE)
      any(all_param_data$location == loc)
    }, logical(1)),
    names(plotly_list)
  )

  ################################
  # Representative legend bounds #
  ################################

  # NULL to start
  legend_bounds <- NULL

  ###############################################
  # Running if implementation model is provided #
  ###############################################
  if(has_impl){

    # Pulling locations that match the list
    for(loc_code in names(locations)){

      # Preparing the PI bounds
      candidate <- prepare_pi_bounds(
        implementation.model = implementation_model,
        geography            = loc_code,
        quantile_pairs       = plot_styles$quantile_pairs,
        outcome.data.label   = config$outcome_data_label
      )

      # Running if there is an issue with PI bounds
      if(!is.null(candidate) && length(candidate) > 0){
        legend_bounds <- candidate
        break
      }

    }

  }

#------------------------------------------------------------------------------#
# Attaching checkbox sync JS ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section calls the globally loaded syncLegendCheckboxesOnRender   #
# function rather than inlining the full JS per widget. The function is loaded #
# at page level via the loading-scripts RMarkdown chunk.                       #
#------------------------------------------------------------------------------#

  ###############################
  # Looping through the plotlys #
  ###############################
  for(k in seq_along(plotly_list)){

    # Checking if plotly is an HTMLWidget
    if(inherits(plotly_list[[k]], "htmlwidget")){

      # Rendering the plotly as an HTML widget
      plotly_list[[k]] <- htmlwidgets::onRender(
        plotly_list[[k]],
        htmlwidgets::JS(
          "function(el, x) { window.syncLegendCheckboxesOnRender(el, x); }"
        )
      )
    }
  }

#------------------------------------------------------------------------------#
# Floating legend --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the floating legend for each plot. This includes  #
# assigning IDs, and conditionally creating each drop down based on what is    #
# supplied by the user and what is available within the user-selected window.  #
#------------------------------------------------------------------------------#

  ###########################
  # Creating the legend ids #
  ###########################

  # Wrap ID
  wrap_id <- paste0("wrap-", plot_id)

  # Floating legend ID
  float_legend_id  <- paste0("float-legend-", plot_id)

  # Drag-able legend ID
  drag_id <- paste0("legend-drag-", plot_id)

  # Selecting the geography
  geo_select_id <- paste0("geoSelect-", plot_id)

  #############################################################
  # Guard against empty hover_text_list before indexing [[1]] #
  #############################################################
  first_hover_text <- if(length(hover_text_list) > 0) hover_text_list[[1]] else ""

  ################################
  # Building the floating legend #
  ################################
  legend_body <- htmltools::div(

    # Class of HTML object
    class = "legend-body",

#------------------------------------------------------------------------------#
# Building the target data section ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the target data section of the legend. The trace  #
# name, Target Data matches the legend group of the truth trace created in an  #
# above section.                                                               #
#------------------------------------------------------------------------------#

    ##############################################
    # Building the legend section for truth data #
    ##############################################
    build_legend_section(
      title      = "Target Data",
      section_id = "target-data",
      collapsed  = FALSE,
      content    = build_legend_item(
        label        = if(length(plotly_list) > 1){
          htmltools::span(
            id = paste0("target-data-label-", plot_id),
            first_hover_text
          )
        }else{
          first_hover_text
        },
        trace_name   = "Target Data",
        swatch_class = "target-data",
        checked      = TRUE
      )
    ),

#------------------------------------------------------------------------------#
# Implementation Model section -------------------------------------------------
#------------------------------------------------------------------------------#
# ABout: This section constructs the legend section for the implementation     #
# model. The checkbox trace names match the legendgroup values set on the      #
# corresponding plotly traces in the plot loop.                                #
#------------------------------------------------------------------------------#

    ############################################
    # Building the implementation model legend #
    ############################################
    build_implementation_model_legend(
      impl_meta  = impl_meta,
      bounds     = legend_bounds,    # FIX: use legend_bounds instead of loop-scoped bounds
      pi_styles  = plot_styles$pi_styles
    ),

#------------------------------------------------------------------------------#
# Evaluation Model section -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section constructs a collapsible legend section for the          #
# evaluation model where each unique forecast horizon gets its own check box   #
# legend item, plus an optional training data item when training data was      #
# provided. It returns NULL when no evaluation model file is provided, when    #
# the file path is NA or empty, or when there are neither horizons nor         #
# training data to show, so the section is cleanly omitted.                    #
#------------------------------------------------------------------------------#

    ########################################
    # Building the evaluation model legend #
    ########################################
    build_evaluation_model_legend(
      evaluation.model.file = config$evaluation_model_file,
      unique_horizons       = unique_horizons,
      evaluation.temp       = if(has_eval) eval_meta$evaluation_model else NULL,
      training_source_name  = training_source_name
    ),

#------------------------------------------------------------------------------#
# Modeling Periods section -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section constructs a collapsible legend section for the training #
# validation, and testing phase ribbons. Each period is included only when     #
# rows for that phase exist in the evaluation data, so the legend always       #
# reflects what is actually plotted. The section is collapsed by default since #
# phase ribbons serve as passive background context rather than primary data   #
# traces.                                                                      #
#------------------------------------------------------------------------------#

    ###############################################
    # Building the modeling period legend element #
    ###############################################
    build_modeling_periods_legend(
      evaluation.model.file = config$evaluation_model_file,
      evaluation.temp       = if(has_eval) eval_meta$evaluation_model else NULL
    ),

#------------------------------------------------------------------------------#
# Auxiliary Variables section --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section constructs one collapsible legend section per unique     #
# data source group in the auxiliary variable data. The legend group title is  #
# the source's `clean_name_full` from the crosswalk with "Auxiliary Variables" #
# appended. Each individual legend item shows the variable's `clean_name_full` #
# from the crosswalk rather than the raw variable name.                        #
#------------------------------------------------------------------------------#

    ###########################################
    # Building the auxiliary variables legend #
    ###########################################
    build_auxiliary_variables_legend(
      parameter_data      = all_param_data,
      variables_crosswalk = variables_crosswalk
    )

  )

#------------------------------------------------------------------------------#
# Building the floating legend -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the floating legend functionality for the plot.   #
# This applies to the legend as a whole, and allows users to drag it whereever #
# they would like.                                                             #
#------------------------------------------------------------------------------#

  ################################
  # Building the floating legend #
  ################################
  # A static export keeps this legend but renders it expanded (no "collapsed"
  # class) so the export CSS can place it, opened, in the reserved right margin.
  float_legend <- htmltools::div(
    id             = float_legend_id,
    class          = if(isTRUE(for_export)){
      "floating-legend float-legend fe-export-legend"
    }else{
      "floating-legend collapsed float-legend"
    },
    `data-plot-id` = plot_id,

    # Adding the functionality
    htmltools::div(
      class = "legend-header legend-drag",
      id    = drag_id,
      htmltools::span("Legend"),
      htmltools::tags$button(class = "legend-toggle", "\u25be")
    ),

    # Legend body elements
    legend_body
  )

#------------------------------------------------------------------------------#
# Rendering: Single Geography --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the single location plot. Essentially, it is     #
# the same plot as the multi-geography without the location drop down at the   #
# top. It will only run if a single plot is generated above.                   #
#------------------------------------------------------------------------------#

  ################################
  # Running if only one location #
  ################################
  if(length(plotly_list) == 1){

    ############################
    # Building the single plot #
    ############################
    plot_block <- htmltools::div(
      htmltools::div(
        id           = wrap_id,
        `data-fs-id` = plot_id,
        class        = paste("fs-wrap",
                             if(!has_eval) "no-eval-model" else NULL),
        style        = "width:100%;display:flex;flex-direction:column;position:relative;",
        htmltools::div(
          class = "fs-body",
          style = "width:100%;flex:1 1 auto;min-height:0;position:relative;",
          float_legend,
          htmltools::div(
            style = "display:flex;justify-content:center;",
            htmltools::div(
              class = "plot-panel active-plot-panel",
              style = "display:block;",
              plotly_list[[1]]
            )
          )
        )
      )
    )

#------------------------------------------------------------------------------#
# Rendering: Multiple Geographies ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the multi-location plots. Essentially, it is     #
# the same plot as the single-geography with the location drop down at the     #
# top. It will only run if a multiple plots are generated above.               #
#------------------------------------------------------------------------------#

  #############################
  # Running if multiple plots #
  #############################
  }else{

    ###########################
    # Building the plot block #
    ###########################
    plot_block <- htmltools::tagList(

      ####################################
      # Building the geography drop down #
      ####################################
      htmltools::div(
        class = "geo-filter-row",
        htmltools::tags$select(
          id             = geo_select_id,
          `data-plot-id` = plot_id,
          lapply(seq_along(plotly_list), function(i){
            htmltools::tags$option(
              value             = i - 1,
              `data-has-params` = if(location_has_params[[i]]) "true" else "false",
              `data-hover-text` = hover_text_list[[i]],
              names(plotly_list)[i]
            )
          })
        )
      ),

      ######################
      # Building the plots #
      ######################
      htmltools::div(
        id           = wrap_id,
        `data-fs-id` = plot_id,
        class        = paste("fs-wrap",
                             if(!has_eval) "no-eval-model" else NULL),
        style        = "width:100%;display:flex;flex-direction:column;position:relative;",
        htmltools::div(
          class = "fs-body",
          style = "width:100%;flex:1 1 auto;min-height:0;position:relative;",
          float_legend,
          htmltools::div(
            style = "display:flex;justify-content:center;",
            lapply(seq_along(plotly_list), function(i){
              htmltools::div(
                class = paste("plot-panel",
                              if(i == 1) "active-plot-panel" else NULL),
                style = if(i == 1) "display:block;" else "display:none;",
                plotly_list[[i]]
              )
            })
          )
        )
      )
    )

  }

#------------------------------------------------------------------------------#
# Return with spacer -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns returns the plot to the main script as well as   #
# spacer for the top of the plot.                                              #
#------------------------------------------------------------------------------#

  htmltools::tagList(
    plot_block,
    htmltools::div(style = "margin-top: 0em;")
  )

}
