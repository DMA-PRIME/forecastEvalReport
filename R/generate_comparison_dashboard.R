#' Generate an interactive forecast model comparison dashboard
#'
#' Validates and processes two or more completed report options files through
#' the same data-preparation and evaluation pathways used by the
#' package report, then writes one self-contained HTML dashboard. The dashboard
#' can compare current, testing-period, and real-time archived median forecasts,
#' observed values, all available performance metrics, data coverage, and a
#' filterable table of model options, evaluation settings, crosswalk terms, and
#' plain-language definitions. Report-facing disease, outcome, model, and
#' location labels are cleaned before display. It remains an HTML file and
#' requires no web server or internet connection after it is created.
#'
#' Every options file must reference a completed variables crosswalk and
#' observed outcome data. Testing evaluation requires an evaluation model
#' containing a testing period. Real-time evaluation requires an implementation
#' model and uses the rolling forecast archive maintained by the package report.
#' Validation is all-or-nothing: if any file fails the checks needed by the
#' selected mode, no dashboard is written and the combined error identifies
#' each file that needs attention.
#'
#' @param options_files Character vector containing paths to two or more
#'   completed report options `.R` files.
#' @param output_file Path for the resulting `.html` dashboard. Defaults to
#'   `"forecast-model-comparison.html"` in the current working directory.
#' @param title Character title shown at the top of the dashboard.
#' @param evaluation Character. Which evaluation results to include:
#'   `"testing"`, `"realtime"`, or `"both"` (default). Testing requires an
#'   evaluation model with testing-period rows. Real-time requires an
#'   implementation model and scores the rolling `Forecasts/` archive against
#'   observations available through the options file.
#' @param eval_config Optional named list from `create_evaluation_config()`.
#'   When `NULL` (default), the package defaults are used for every model.
#' @param include_row_metrics Logical. When `TRUE` (default), the dashboard data
#'   includes both summary and per-forecast metrics. Set to `FALSE` for a
#'   smaller HTML file containing only overall and horizon summaries.
#' @param overwrite Logical. When `TRUE` (default), an existing output file is
#'   replaced. When `FALSE`, the function stops before doing any model work.
#' @param quiet Logical. When `TRUE` (default), suppresses pipeline progress
#'   messages. Validation errors and the final dashboard path are still shown.
#'
#' @return Invisibly returns the normalized path to the generated HTML file.
#'
#' @examples
#' \dontrun{
#' generate_comparison_dashboard(
#'   options_files = c(
#'     "models/arima/report_options.R",
#'     "models/ets/report_options.R"
#'   ),
#'   output_file = "model-comparison.html"
#' )
#' }
#'
#' @export
generate_comparison_dashboard <- function(
    options_files,
    output_file = "forecast-model-comparison.html",
    title = "Model Summaries",
    evaluation = c("both", "testing", "realtime"),
    eval_config = NULL,
    include_row_metrics = TRUE,
    overwrite = TRUE,
    quiet = TRUE) {

#------------------------------------------------------------------------------#
# Validating the user inputs ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the comparison-specific inputs before running any #
# potentially expensive report-data assembly. The options themselves are then #
# checked by the package's existing report validation pipeline below.          #
#------------------------------------------------------------------------------#

  ##############################################
  # Require at least two options-file pathways #
  ##############################################
  if(missing(options_files) || !is.character(options_files) ||
     length(options_files) < 2L || anyNA(options_files) ||
     any(!nzchar(trimws(options_files)))){

    stop("`options_files` must contain paths to at least two completed ",
         "report options files.", call. = FALSE)

  }

  ############################################
  # Confirm every supplied file can be found #
  ############################################
  missing_files <- options_files[!file.exists(options_files)]

  if(length(missing_files) > 0L){
    stop("Options file(s) not found:\n  - ",
         paste(missing_files, collapse = "\n  - "), call. = FALSE)
  }

  ##########################################
  # Validate the remaining scalar settings #
  ##########################################
  if(!is.character(output_file) || length(output_file) != 1L ||
     is.na(output_file) || !nzchar(trimws(output_file)) ||
     !grepl("\\.html?$", output_file, ignore.case = TRUE)){
    stop("`output_file` must be one path ending in .html or .htm.",
         call. = FALSE)
  }

  if(!is.character(title) || length(title) != 1L || is.na(title) ||
     !nzchar(trimws(title))){
    stop("`title` must be one non-empty character value.", call. = FALSE)
  }

  #########################################
  # Resolve the requested evaluation mode #
  #########################################
  evaluation <- match.arg(evaluation)

  ###################################################
  # Resolve one evaluation configuration for all runs #
  ###################################################
  resolved_eval_config <- if(is.null(eval_config)){
    create_evaluation_config()
  }else{eval_config}

  logical_args <- list(include_row_metrics = include_row_metrics,
                       overwrite = overwrite, quiet = quiet)

  bad_logical <- names(logical_args)[!vapply(
    logical_args,
    function(x) is.logical(x) && length(x) == 1L && !is.na(x),
    logical(1)
  )]

  if(length(bad_logical) > 0L){
    stop("These arguments must each be TRUE or FALSE: ",
         paste(bad_logical, collapse = ", "), ".", call. = FALSE)
  }

  if(!is.null(eval_config) && !is.list(eval_config)){
    stop("`eval_config` must be NULL or a list from ",
         "create_evaluation_config().", call. = FALSE)
  }

  #############################################
  # Stop early rather than overwrite by accident #
  #############################################
  if(file.exists(output_file) && !isTRUE(overwrite)){
    stop("Output file already exists and `overwrite = FALSE`:\n  ",
         output_file, call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Running the verified report-data pathway ------------------------------------
#------------------------------------------------------------------------------#
# About: Each file follows the matching report pathway for the requested       #
# evaluation mode. Testing uses export_testing_evaluation(); real-time uses the #
# report's implementation archive and real-time builder. All runs finish       #
# before any dashboard file is written.                                        #
#------------------------------------------------------------------------------#

  ###########################################
  # A small helper for optional status text #
  ###########################################
  say <- function(...) if(!isTRUE(quiet)) message(...)

  ##############################################
  # Process one fully validated options file   #
  ##############################################
  process_one <- function(path){
    run_pathway <- function(){

      ########################################################
      # Real-time only does not require an evaluation model   #
      ########################################################
      if(identical(evaluation, "realtime")){
        realtime <- prepare_comparison_realtime(
          options_file = path,
          eval_config = resolved_eval_config,
          quiet = quiet
        )
        return(list(
          testing_eval = NULL,
          testing_results = NULL,
          realtime_eval = realtime$realtime_eval,
          context = realtime$context
        ))
      }

      ######################################################
      # Testing pathway, shared by testing and both modes   #
      ######################################################
      testing <- export_testing_evaluation(
        options_file = path,
        ai_form = FALSE,
        save_results = FALSE,
        eval_config = resolved_eval_config,
        quiet = quiet,
        return_context = TRUE
      )

      output <- list(
        testing_eval = testing$testing_eval,
        testing_results = testing$results,
        realtime_eval = NULL,
        context = testing$context
      )

      ############################################################
      # Both requires the implementation archive as well         #
      ############################################################
      if(identical(evaluation, "both")){
        if(is.null(output$context$implementation_model)){
          stop("`evaluation = \"both\"` requires an implementation model ",
               "for real-time evaluation.", call. = FALSE)
        }

        # Match the report by saving the current forecast to its archive
        output$context$impl_meta <- extract_implementation_data(
          implementation_model = output$context$implementation_model,
          config = output$context$config,
          save_data = TRUE
        )

        output$realtime_eval <- build_realtime_evaluation(
          impl_meta = output$context$impl_meta,
          master_data = output$context$master_data,
          variables_crosswalk = output$context$variables_crosswalk
        )
      }

      output
    }

    if(isTRUE(quiet)) suppressMessages(run_pathway()) else run_pathway()
  }

  ###############################
  # Run and collect every result #
  ###############################
  bundles <- vector("list", length(options_files))
  failures <- character(0)

  for(i in seq_along(options_files)){
    say("generate_comparison_dashboard(): validating ", i, " of ",
        length(options_files), " — ", basename(options_files[i]))

    bundles[[i]] <- tryCatch(
      process_one(options_files[i]),
      error = function(e){
        failures <<- c(failures, paste0(
          basename(options_files[i]), ": ", conditionMessage(e)))
        NULL
      }
    )
  }

  #############################################
  # Stop if even one options file did not pass #
  #############################################
  if(length(failures) > 0L){
    stop(
      "The dashboard was not created because these options files did not ",
      "complete report validation and evaluation:\n\n  - ",
      paste(failures, collapse = "\n  - "),
      call. = FALSE
    )
  }

#------------------------------------------------------------------------------#
# Building consistent comparison labels --------------------------------------
#------------------------------------------------------------------------------#
# About: The report's general model type is the dashboard label. When two      #
# options files use the same label, their filename is appended so controls,    #
# legends, and downloaded tables remain unambiguous.                           #
#------------------------------------------------------------------------------#

  ###############################################
  # Pull one safe scalar value from a config list #
  ###############################################
  scalar_chr <- function(x, fallback = NA_character_){
    if(is.null(x) || length(x) == 0L || all(is.na(x))) return(fallback)
    value <- trimws(as.character(x[1]))
    if(!nzchar(value)) fallback else value
  }

  ########################################################
  # Clean an internal identifier for report-facing use   #
  ########################################################
  clean_display_label <- function(x){
    value <- scalar_chr(x)
    if(is.na(value)) return(value)

    known <- c(
      "covid_19" = "COVID-19", "covid-19" = "COVID-19",
      "rsv" = "RSV", "influenza" = "Influenza",
      "flusight" = "FluSight", "covidhub" = "COVID Hub",
      "rsvhub" = "RSV Hub", "metrocast" = "MetroCast"
    )
    key <- tolower(value)
    if(key %in% names(known)) return(unname(known[key]))

    value <- trimws(gsub("_+", " ", value))
    if(identical(value, tolower(value))) tools::toTitleCase(value) else value
  }

  #########################################################
  # Use the same crosswalk-first disease label as reports #
  #########################################################
  clean_disease_label <- function(config, crosswalk){
    if(!is.null(crosswalk) && is.data.frame(crosswalk) &&
       all(c("variable_type", "disease_name_clean") %in% names(crosswalk))){
      rows <- crosswalk[!is.na(crosswalk$variable_type) &
                          crosswalk$variable_type == "outcome", , drop = FALSE]
      values <- unique(trimws(as.character(rows$disease_name_clean)))
      values <- values[!is.na(values) & nzchar(values)]
      if(length(values) > 0L) return(paste(values, collapse = " / "))
    }
    clean_display_label(config$disease)
  }

  #########################################################
  # Resolve clean disease values for filters and counts   #
  #########################################################
  clean_disease_values <- function(config, crosswalk){
    if(!is.null(crosswalk) && is.data.frame(crosswalk) &&
       all(c("variable_type", "disease_name_clean") %in% names(crosswalk))){
      rows <- crosswalk[!is.na(crosswalk$variable_type) &
                          crosswalk$variable_type == "outcome", , drop = FALSE]
      values <- unique(trimws(as.character(rows$disease_name_clean)))
      values <- values[!is.na(values) & nzchar(values)]
      if(length(values) > 0L) return(values)
    }
    clean_display_label(config$disease)
  }

  #########################################################
  # Clean the report-facing spatial scale                 #
  #########################################################
  clean_spatial_scale <- function(context){
    raw <- if(!is.null(context$impl_meta) &&
              !is.null(context$impl_meta$spatial_scale)){
      context$impl_meta$spatial_scale
    }else if(!is.null(context$eval_meta) &&
             !is.null(context$eval_meta$spatial_scale)){
      context$eval_meta$spatial_scale
    }else if(!is.null(context$variables_crosswalk) &&
             "spatial_scale" %in% names(context$variables_crosswalk)){
      unique(context$variables_crosswalk$spatial_scale)
    }else{NA_character_}

    values <- unique(trimws(as.character(raw)))
    values <- values[!is.na(values) & nzchar(values)]
    if(length(values) == 0L) return("Spatial scale not supplied")
    values <- vapply(values, clean_display_label, character(1))
    values <- gsub("\\bHsa\\b", "HSA", values)
    paste(values, collapse = " & ")
  }

  ########################################################
  # Pull a clean report label from selected crosswalk rows #
  ########################################################
  crosswalk_clean_name <- function(crosswalk, type, fallback){
    if(!is.null(crosswalk) && is.data.frame(crosswalk) &&
       all(c("variable_type", "clean_name_full") %in% names(crosswalk))){
      rows <- crosswalk[!is.na(crosswalk$variable_type) &
                          crosswalk$variable_type == type, , drop = FALSE]
      values <- unique(trimws(as.character(rows$clean_name_full)))
      values <- values[!is.na(values) & nzchar(values) &
                         values != "USER: provide a definition"]
      if(length(values) > 0L) return(paste(values, collapse = ", "))
    }
    clean_display_label(fallback)
  }

  ###########################################################
  # Resolve the outcome's data source exactly as the report #
  ###########################################################
  clean_outcome_source <- function(crosswalk, fallback){
    required <- c("variable_type", "variable", "data_source",
                  "clean_name_full")
    if(!is.null(crosswalk) && is.data.frame(crosswalk) &&
       all(required %in% names(crosswalk))){
      outcome_sources <- unique(crosswalk$data_source[
        !is.na(crosswalk$variable_type) &
          crosswalk$variable_type == "outcome"])
      rows <- crosswalk[
        !is.na(crosswalk$variable_type) &
          crosswalk$variable_type == "data_source" &
          crosswalk$variable %in% outcome_sources, , drop = FALSE]
      values <- unique(trimws(as.character(rows$clean_name_full)))
      values <- values[!is.na(values) & nzchar(values)]
      if(length(values) > 0L) return(paste(values, collapse = ", "))
    }
    clean_display_label(fallback)
  }

  ##############################
  # Initial user-facing labels #
  ##############################
  labels <- vapply(seq_along(bundles), function(i){
    context <- bundles[[i]]$context
    clean_model <- clean_display_label(scalar_chr(
      context$config$general_model_type, "Model"))
    clean_disease <- clean_disease_label(
      context$config, context$variables_crosswalk)
    paste0(clean_model, " - ", clean_disease, ", ",
           clean_spatial_scale(context))
  }, character(1))

  # Number only true duplicate labels; never expose options filenames
  duplicates <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  labels[duplicates] <- make.unique(labels[duplicates], sep = " — Model ")

#------------------------------------------------------------------------------#
# Standardizing forecast and metric data --------------------------------------
#------------------------------------------------------------------------------#
# About: This section converts each verified bundle into the small, stable     #
# tables consumed by the dashboard. Median and interval quantiles feed charts; #
# the complete long evaluation output feeds the metric explorer.               #
#------------------------------------------------------------------------------#

  ##########################################
  # Map raw locations to display locations #
  ##########################################
  display_locations <- function(location, mapping){
    raw <- as.character(location)
    if(is.null(mapping) || length(mapping) == 0L || is.null(names(mapping))){
      return(raw)
    }
    matched <- unname(mapping[raw])
    ifelse(is.na(matched) | !nzchar(matched), raw, matched)
  }

  ######################################
  # Convert a date-like vector to text #
  ######################################
  date_text <- function(x){
    if(is.null(x)) return(character(0))
    as.character(suppressWarnings(anytime::anydate(x)))
  }

  #############################################
  # Pull one valid metadata date for a range  #
  #############################################
  metadata_date <- function(x){
    values <- date_text(x)
    values <- values[!is.na(values) & nzchar(values)]
    if(length(values) == 0L) NA_character_ else values[1]
  }

  #############################################
  # Build the empty standardized forecast set #
  #############################################
  empty_forecasts <- function(){
    data.frame(model = character(0), disease = character(0),
               series_type = character(0),
               location = character(0), reference_date = character(0),
               target_end_date = character(0), horizon = character(0),
               target = character(0), quantile = numeric(0), value = numeric(0),
               stringsAsFactors = FALSE)
  }

  ########################################
  # Standardize point and interval rows from a frame #
  ########################################
  standardize_forecasts <- function(df, model, disease, series_type, locations){
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
       !all(c("location", "target_end_date", "value") %in% names(df))){
      return(empty_forecasts())
    }

    quantile <- if("output_type_id" %in% names(df)){
      suppressWarnings(as.numeric(as.character(df$output_type_id)))
    }else{rep(NA_real_, nrow(df))}

    # Retain central estimates and the bounds needed for 80% and 95% PIs.
    keep_quantiles <- is.na(quantile) |
      vapply(quantile, function(x) any(abs(x - c(.025, .1, .5, .9, .975)) < 1e-8),
             logical(1))
    df <- df[keep_quantiles, , drop = FALSE]
    quantile <- quantile[keep_quantiles]

    # Fill optional columns consistently across input formats
    reference_date <- if("reference_date" %in% names(df)){
      date_text(df$reference_date)
    }else{rep(NA_character_, nrow(df))}

    horizon <- if("horizon" %in% names(df)){
      as.character(df$horizon)
    }else{rep(NA_character_, nrow(df))}

    target <- if("target" %in% names(df)){
      as.character(df$target)
    }else{rep(NA_character_, nrow(df))}

    output <- data.frame(
      model           = rep(model, nrow(df)),
      disease         = rep(disease, nrow(df)),
      series_type     = rep(series_type, nrow(df)),
      location        = display_locations(df$location, locations),
      reference_date  = reference_date,
      target_end_date = date_text(df$target_end_date),
      horizon         = horizon,
      target          = target,
      quantile        = quantile,
      value           = suppressWarnings(as.numeric(df$value)),
      stringsAsFactors = FALSE
    )

    output <- output[!is.na(output$target_end_date) &
                       !is.na(output$value), , drop = FALSE]
    output[!duplicated(output), , drop = FALSE]
  }

  ##########################################
  # Keep current implementation projections #
  ##########################################
  current_projection_rows <- function(df){
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0L) return(df)

    if("estimate_projected_report" %in% names(df)){
      current <- !is.na(df$estimate_projected_report) &
        suppressWarnings(as.numeric(df$estimate_projected_report)) == 1
      if(any(current)) return(df[current, , drop = FALSE])
    }

    if(all(c("reference_date", "target_end_date") %in% names(df))){
      reference <- suppressWarnings(anytime::anydate(df$reference_date))
      target <- suppressWarnings(anytime::anydate(df$target_end_date))
      current <- !is.na(reference) & !is.na(target) & target >= reference
      return(df[current, , drop = FALSE])
    }

    df
  }

  ########################################
  # Empty standardized observed data set #
  ########################################
  empty_truth <- function(){
    data.frame(model = character(0), disease = character(0),
               location = character(0),
               date = character(0), value = numeric(0),
               label = character(0), stringsAsFactors = FALSE)
  }

  ############################################
  # Build the detailed peak-figure data set  #
  ############################################
  empty_peak_details <- function(){
    data.frame(
      model = character(0), disease = character(0), location = character(0),
      season = character(0), horizon = character(0), horizon_name = character(0),
      observed_peak_date = character(0), observed_peak_value = numeric(0),
      predicted_peak_date = character(0), predicted_peak_value = numeric(0),
      observed_at_predicted_peak = numeric(0), peak_week_forecast_value = numeric(0),
      peak_week_forecast_exists = logical(0), predicted_peak_timing_off = numeric(0),
      predicted_peak_timing_label = character(0), predicted_peak_magnitude_off = numeric(0),
      predicted_peak_accuracy = numeric(0), same_day_magnitude_off = numeric(0),
      same_day_accuracy = numeric(0), peak_week_magnitude_off = numeric(0),
      peak_week_accuracy = numeric(0), stringsAsFactors = FALSE)
  }

  standardize_peak_details <- function(df, model, disease, locations){
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
       !"location" %in% names(df)) return(empty_peak_details())

    value_or <- function(name, fallback = NA){
      if(name %in% names(df)) df[[name]] else rep(fallback, nrow(df))
    }
    numeric_or <- function(name) suppressWarnings(as.numeric(value_or(name, NA_real_)))
    character_or <- function(name) as.character(value_or(name, NA_character_))

    output <- data.frame(
      model = model, disease = disease,
      location = display_locations(df$location, locations),
      season = character_or("season"), horizon = character_or("horizon"),
      horizon_name = character_or("horizonName"),
      observed_peak_date = date_text(value_or("observedPeakDate", NA_character_)),
      observed_peak_value = numeric_or("observedPeakValue"),
      predicted_peak_date = date_text(value_or("predictedPeakDate", NA_character_)),
      predicted_peak_value = numeric_or("predictedPeakValue"),
      observed_at_predicted_peak = numeric_or("observedAtPredictedPeak"),
      peak_week_forecast_value = numeric_or("peakWeekForecastValue"),
      peak_week_forecast_exists = as.logical(value_or("peakWeekForecastExists", FALSE)),
      predicted_peak_timing_off = numeric_or("predictedPeakTimingOff"),
      predicted_peak_timing_label = character_or("predictedPeakTimingLabel"),
      predicted_peak_magnitude_off = numeric_or("predictedPeakMagnitudeOff"),
      predicted_peak_accuracy = numeric_or("predictedPeakAccuracy"),
      same_day_magnitude_off = numeric_or("sameDayMagnitudeOff"),
      same_day_accuracy = numeric_or("sameDayAccuracy"),
      peak_week_magnitude_off = numeric_or("peakWeekMagnitudeOff"),
      peak_week_accuracy = numeric_or("peakWeekAccuracy"),
      stringsAsFactors = FALSE
    )
    output[!duplicated(output), , drop = FALSE]
  }

  #############################################
  # Convert testing data's observed values    #
  #############################################
  standardize_truth <- function(df, model, disease, locations,
                                observed_label, period){
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
       !all(c("location", "target_end_date", "Observed") %in% names(df))){
      return(empty_truth())
    }

    output <- data.frame(
      model    = rep(model, nrow(df)),
      disease  = rep(disease, nrow(df)),
      location = display_locations(df$location, locations),
      date     = date_text(df$target_end_date),
      value    = suppressWarnings(as.numeric(df$Observed)),
      label    = rep(paste0("Observed — ", observed_label, " (", period, ")"),
                     nrow(df)),
      stringsAsFactors = FALSE
    )

    output <- output[!is.na(output$date) & !is.na(output$value), , drop = FALSE]
    output <- output[!duplicated(output), , drop = FALSE]

    # Evaluation rows repeat the same observation across forecast horizons and
    # quantiles. Keep one observed value per model, location, and target date.
    keys <- paste(output$model, output$location, output$date, sep = "\r")
    output[!duplicated(keys), , drop = FALSE]
  }

  ###########################################
  # Read plain-language model descriptions #
  ###########################################
  model_description <- function(config){
    md <- config$model_descriptions
    if(is.null(md) || !is.data.frame(md) || nrow(md) == 0L ||
       !"description" %in% names(md)) return(NA_character_)
    values <- trimws(as.character(md$description))
    values <- values[!is.na(values) & nzchar(values)]
    if(length(values) == 0L) NA_character_ else paste(unique(values), collapse = " ")
  }

  ########################################################
  # List clean data sources and variables used           #
  ########################################################
  crosswalk_items <- function(crosswalk, types, field = "clean_name_full"){
    if(is.null(crosswalk) || !is.data.frame(crosswalk) ||
       !all(c("variable_type", field) %in% names(crosswalk))) return("Not supplied")
    rows <- crosswalk$variable_type %in% types
    values <- unique(trimws(as.character(crosswalk[[field]][rows])))
    values <- values[!is.na(values) & nzchar(values) &
                       values != "USER: provide a definition"]
    if(length(values) == 0L) "Not supplied" else paste(values, collapse = ", ")
  }

  #######################################################
  # Format an option value for a readable HTML table    #
  #######################################################
  parameter_value <- function(x){
    if(is.null(x) || length(x) == 0L || all(is.na(x))) return("Not supplied")
    if(is.logical(x)) return(paste(ifelse(x, "Yes", "No"), collapse = ", "))
    paste(as.character(x), collapse = ", ")
  }

  ########################################################
  # Build documented option and evaluation-setting rows #
  ########################################################
  build_parameter_table <- function(model, disease, locations, context,
                                    outcome_display, source_display,
                                    training_start, training_end,
                                    validation_start, validation_end,
                                    testing_start, testing_end,
                                    realtime_start, realtime_end){
    config <- context$config

    parameters <- c(
      "Model", "Model description", "Disease", "Forecast target",
      "Target data source", "Report context", "Target population",
      "State context", "Contact", "Contact email",
      "Implementation model file", "Evaluation model file",
      "Evaluation mode", "Training period", "Validation period",
      "Testing period", "Real-time period",
      "Non-transmission months", "Season start", "Peak window",
      "Stable-count threshold", "Percentage-error cushion",
      "Peak timing tolerance", "Peak magnitude agreement threshold"
    )

    values <- c(
      model,
      parameter_value(model_description(config)),
      disease,
      outcome_display,
      source_display,
      clean_display_label(config$reason),
      clean_display_label(config$population_label),
      parameter_value(config$state_context),
      parameter_value(config$contact_name),
      parameter_value(config$contact_email),
      if(!is.na(config$implementation_model_file))
        basename(config$implementation_model_file) else "Not supplied",
      if(!is.na(config$evaluation_model_file))
        basename(config$evaluation_model_file) else "Not supplied",
      clean_display_label(evaluation),
      if(!is.na(training_start) && !is.na(training_end))
        paste(training_start, "through", training_end) else "Not available",
      if(!is.na(validation_start) && !is.na(validation_end))
        paste(validation_start, "through", validation_end) else "Not available",
      if(!is.na(testing_start) && !is.na(testing_end))
        paste(testing_start, "through", testing_end) else "Not available",
      if(!is.na(realtime_start) && !is.na(realtime_end))
        paste(realtime_start, "through", realtime_end) else "Not available",
      parameter_value(resolved_eval_config$non_transmission_months),
      parameter_value(resolved_eval_config$season_start_day_month),
      paste0(parameter_value(resolved_eval_config$peak_window), "%"),
      parameter_value(resolved_eval_config$stable_threshold),
      paste0(parameter_value(resolved_eval_config$pct_error_cushion),
             " percentage points"),
      parameter_value(resolved_eval_config$timing_tol_steps),
      parameter_value(resolved_eval_config$mag_tol)
    )

    groups <- c(
      rep("Model and purpose", 5), rep("Report setup", 7),
      rep("Evaluation coverage", 5), rep("Evaluation settings", 7)
    )

    definitions <- c(
      "The report-facing name used to identify this forecast model.",
      "The model developer's plain-language explanation of the approach and assumptions.",
      "The clean disease or condition name supplied by the variables crosswalk.",
      "The clean outcome measure the forecasts are intended to predict.",
      "The clean name of the observed data source used as the evaluation truth.",
      "The forecast-report workflow or hub format used to validate and interpret the model.",
      "The population represented by the forecasts and observed outcome.",
      "Whether state-level contextual rules are applied while validating general-format files.",
      "The person listed as the point of contact for the model report.",
      "The contact email supplied in the options file.",
      "The operational forecast file used for current and real-time evaluation.",
      "The retrospective model file containing training, validation, or testing periods.",
      "Whether this dashboard includes testing-period evaluation, real-time evaluation, or both.",
      "The first and last dates included in model training.",
      "The first and last dates included in model validation.",
      "The first and last dates included in testing-period evaluation.",
      "The first and last realized target dates available from the real-time forecast archive.",
      "Calendar months excluded from transmission-season aggregate summaries.",
      "The month and day used to assign observations and forecasts to a disease season.",
      "The percent tolerance below the observed seasonal maximum used to define the peak phase.",
      "Observed values below this count are omitted from percentage-bias summaries because small absolute errors can create unstable percentages.",
      "The percentage-point band around zero counted as within range for forecast-bias labels.",
      "The number of forecast time steps allowed around the observed peak before timing is labeled early or late.",
      "The minimum rank-matched agreement required for peak magnitude to be labeled on target."
    )

    output <- data.frame(
      model = model, disease = disease, location = "All locations",
      group = groups, parameter = parameters, value = values,
      definition = definitions, stringsAsFactors = FALSE
    )

    # Add one explicit row per clean report location
    if(length(locations) > 0L){
      location_rows <- data.frame(
        model = model, disease = disease, location = locations,
        group = "Geography", parameter = "Location",
        value = locations,
        definition = "A clean report-facing geography included in this model's forecasts or evaluation results.",
        stringsAsFactors = FALSE
      )
      output <- rbind(output, location_rows)
    }

    # Add crosswalk terms and their user-supplied definitions
    crosswalk <- context$variables_crosswalk
    if(!is.null(crosswalk) && is.data.frame(crosswalk) &&
       all(c("variable_type", "variable", "definition") %in% names(crosswalk))){
      keep <- !is.na(crosswalk$definition) &
        nzchar(trimws(as.character(crosswalk$definition))) &
        trimws(as.character(crosswalk$definition)) != "USER: provide a definition"
      documented <- crosswalk[keep, , drop = FALSE]

      if(nrow(documented) > 0L){
        names_clean <- if("clean_name_full" %in% names(documented))
          as.character(documented$clean_name_full) else rep(NA_character_, nrow(documented))
        names_clean[is.na(names_clean) | !nzchar(trimws(names_clean))] <-
          as.character(documented$variable[is.na(names_clean) |
                                             !nzchar(trimws(names_clean))])

        row_locations <- if("location" %in% names(documented)){
          mapping <- if(!is.null(context$impl_meta) &&
                        !is.null(context$impl_meta$locations)){
            context$impl_meta$locations
          }else if(!is.null(context$eval_meta)){
            context$eval_meta$locations
          }else{NULL}
          display_locations(documented$location, mapping)
        }else{rep("All locations", nrow(documented))}
        row_locations[is.na(row_locations) | !nzchar(row_locations)] <- "All locations"

        source_values <- if("data_source" %in% names(documented))
          as.character(documented$data_source) else as.character(documented$variable_type)

        crosswalk_rows <- data.frame(
          model = model, disease = disease, location = row_locations,
          group = paste0("Crosswalk: ", vapply(
            documented$variable_type, clean_display_label, character(1))),
          parameter = names_clean,
          value = source_values,
          definition = as.character(documented$definition),
          stringsAsFactors = FALSE
        )
        output <- rbind(output, crosswalk_rows)
      }
    }

    output[!duplicated(output), , drop = FALSE]
  }

  ############################
  # Dashboard color sequence #
  ############################
  colors <- c("#087f86", "#e76f51", "#3a6ea5", "#8b5fbf",
              "#27815b", "#c18a14", "#c94f72", "#50667a")

  forecast_pieces <- truth_pieces <- performance_pieces <- peak_detail_pieces <-
    vector("list", length(bundles))
  model_pieces <- parameter_pieces <- vector("list", length(bundles))

  ########################################
  # Standardize each successfully run file #
  ########################################
  for(i in seq_along(bundles)){
    bundle <- bundles[[i]]
    context <- bundle$context
    config <- context$config
    location_map <- if(!is.null(context$impl_meta) &&
                       !is.null(context$impl_meta$locations)){
      context$impl_meta$locations
    }else if(!is.null(context$eval_meta) &&
             !is.null(context$eval_meta$locations)){
      context$eval_meta$locations
    }else{NULL}

    # Report-facing labels, resolved from the completed crosswalk first
    disease_display <- clean_disease_label(
      config, context$variables_crosswalk)
    disease_values <- clean_disease_values(
      config, context$variables_crosswalk)
    outcome_display <- crosswalk_clean_name(
      context$variables_crosswalk, "outcome", config$outcome_name)
    source_display <- clean_outcome_source(
      context$variables_crosswalk, config$outcome_data_label)
    spatial_scale_display <- clean_spatial_scale(context)
    data_sets_used <- crosswalk_items(
      context$variables_crosswalk, "data_source")
    variables_used <- crosswalk_items(
      context$variables_crosswalk,
      c("outcome", "aux_variable", "training"))

    # Testing-period median forecasts and observed values, when requested
    testing_data <- if(!is.null(bundle$testing_eval)){
      bundle$testing_eval$data
    }else{NULL}
    testing_forecasts <- standardize_forecasts(
      testing_data, labels[i], disease_display,
      "Testing median forecasts", location_map)

    observed_label <- scalar_chr(source_display, "Observed")
    testing_truth <- standardize_truth(
      testing_data, labels[i], disease_display, location_map,
      observed_label, "Testing")

    # Preserve the report's three distinct peak comparisons for its linked
    # timing/magnitude figure design in the model-comparison dashboard.
    testing_peak <- if(!is.null(bundle$testing_eval)){
      bundle$testing_eval$peakPhase
    }else{NULL}
    peak_detail_pieces[[i]] <- standardize_peak_details(
      testing_peak, labels[i], disease_display, location_map)

    # Rolling real-time archive forecasts and realized observations
    realtime_data <- if(!is.null(bundle$realtime_eval)){
      bundle$realtime_eval$data
    }else{NULL}
    realtime_forecasts <- standardize_forecasts(
      realtime_data, labels[i], disease_display,
      "Real-time archived forecasts", location_map)
    realtime_truth <- standardize_truth(
      realtime_data, labels[i], disease_display, location_map,
      observed_label, "Real-time")
    truth_pieces[[i]] <- rbind(testing_truth, realtime_truth)

    # Optional current implementation forecast
    current_data <- current_projection_rows(context$implementation_model)
    current_map <- if(!is.null(context$impl_meta$locations)){
      context$impl_meta$locations
    }else{location_map}
    current_forecasts <- standardize_forecasts(
      current_data, labels[i], disease_display,
      "Current forecast", current_map)

    forecast_pieces[[i]] <- rbind(
      testing_forecasts, realtime_forecasts, current_forecasts)

    # Full tidy testing and/or real-time metric table
    testing_performance <- bundle$testing_results
    if(!is.null(testing_performance) && nrow(testing_performance) > 0L){
      testing_performance$evaluation_period <- "Testing"
      testing_performance <- testing_performance[c(
        "model", "evaluation_period",
        setdiff(names(testing_performance), c("model", "evaluation_period"))
      )]
    }

    realtime_performance <- if(!is.null(bundle$realtime_eval)){
      melt_comparison_metrics(
        bundle$realtime_eval, labels[i], "Real-time")
    }else{NULL}

    performance_parts <- list(testing_performance, realtime_performance)
    performance_parts <- performance_parts[vapply(
      performance_parts,
      function(x) is.data.frame(x) && nrow(x) > 0L,
      logical(1)
    )]
    performance <- if(length(performance_parts) > 0L){
      do.call(rbind, performance_parts)
    }else{NULL}

    if(!isTRUE(include_row_metrics) && !is.null(performance) &&
       nrow(performance) > 0L){
      performance <- performance[performance$scope != "row", , drop = FALSE]
    }
    if(!is.null(performance) && nrow(performance) > 0L){
      performance$model <- labels[i]
      performance$disease <- disease_display
      performance$location <- display_locations(
        performance$location, location_map)
    }
    performance_pieces[[i]] <- performance

    # Metadata shown in the overview and coverage tabs
    locations <- unique(c(forecast_pieces[[i]]$location,
                          if(!is.null(performance) && nrow(performance) > 0L)
                            performance$location else character(0)))
    locations <- sort(locations[!is.na(locations) & nzchar(locations)])

    realtime_dates <- if(!is.null(realtime_data) &&
                         is.data.frame(realtime_data) &&
                         nrow(realtime_data) > 0L){
      date_text(realtime_data$target_end_date)
    }else{character(0)}
    realtime_dates <- realtime_dates[!is.na(realtime_dates)]

    testing_start <- if(!is.null(context$eval_meta)){
      metadata_date(context$eval_meta$testing_start)
    }else{NA_character_}
    testing_end <- if(!is.null(context$eval_meta)){
      metadata_date(context$eval_meta$testing_end)
    }else{NA_character_}
    training_start <- if(!is.null(context$eval_meta)){
      metadata_date(context$eval_meta$training_start)
    }else{NA_character_}
    training_end <- if(!is.null(context$eval_meta)){
      metadata_date(context$eval_meta$training_end)
    }else{NA_character_}
    validation_start <- if(!is.null(context$eval_meta)){
      metadata_date(context$eval_meta$validation_start)
    }else{NA_character_}
    validation_end <- if(!is.null(context$eval_meta)){
      metadata_date(context$eval_meta$validation_end)
    }else{NA_character_}

    evaluation_display <- switch(
      evaluation,
      testing = "Testing-period evaluation",
      realtime = "Real-time evaluation",
      both = "Testing-period and real-time evaluation"
    )

    model_pieces[[i]] <- data.frame(
      model         = labels[i],
      color         = colors[(i - 1L) %% length(colors) + 1L],
      reason        = scalar_chr(clean_display_label(config$reason),
                                 "Not supplied"),
      reason_keys   = scalar_chr(clean_display_label(config$reason),
                                 "Not supplied"),
      disease       = scalar_chr(disease_display, "Not supplied"),
      disease_keys  = paste(disease_values, collapse = "|||"),
      outcome       = scalar_chr(outcome_display, observed_label),
      spatial_scale = spatial_scale_display,
      description   = model_description(config),
      locations     = paste(locations, collapse = ", "),
      location_keys = paste(locations, collapse = "|||"),
      evaluation    = evaluation_display,
      data_sets     = data_sets_used,
      variables     = variables_used,
      training_start = training_start,
      training_end = training_end,
      validation_start = validation_start,
      validation_end = validation_end,
      testing_start = testing_start,
      testing_end   = testing_end,
      realtime_start = if(length(realtime_dates) > 0L)
        min(realtime_dates) else NA_character_,
      realtime_end = if(length(realtime_dates) > 0L)
        max(realtime_dates) else NA_character_,
      stringsAsFactors = FALSE
    )

    # Fully documented model/options/crosswalk table for the dashboard
    parameter_pieces[[i]] <- build_parameter_table(
      model = labels[i],
      disease = disease_display,
      locations = locations,
      context = context,
      outcome_display = outcome_display,
      source_display = source_display,
      training_start = training_start,
      training_end = training_end,
      validation_start = validation_start,
      validation_end = validation_end,
      testing_start = testing_start,
      testing_end = testing_end,
      realtime_start = model_pieces[[i]]$realtime_start,
      realtime_end = model_pieces[[i]]$realtime_end
    )
  }

  ##############################
  # Stack the standardized data #
  ##############################
  bind_pieces <- function(x, empty){
    present <- x[vapply(x, function(item) is.data.frame(item) && nrow(item) > 0L,
                        logical(1))]
    if(length(present) == 0L) empty() else do.call(rbind, present)
  }

  models <- do.call(rbind, model_pieces)
  parameters <- do.call(rbind, parameter_pieces)
  forecasts <- bind_pieces(forecast_pieces, empty_forecasts)
  truth <- bind_pieces(truth_pieces, empty_truth)
  peak_details <- bind_pieces(peak_detail_pieces, empty_peak_details)

  performance <- bind_pieces(performance_pieces, function(){
    data.frame(model = character(0), disease = character(0),
               evaluation_period = character(0),
               location = character(0),
               season = character(0), horizon = character(0),
               scope = character(0), family = character(0),
               metric = character(0), value = numeric(0),
               value_chr = character(0), reference_date = character(0),
               target_end_date = character(0), stringsAsFactors = FALSE)
  })

#------------------------------------------------------------------------------#
# Packaging the self-contained dashboard -------------------------------------
#------------------------------------------------------------------------------#
# About: CSS, JavaScript, and JSON are embedded directly into one HTML file.   #
# No external libraries, network calls, sidecar folders, or runtime server are #
# needed to open and share the completed dashboard.                            #
#------------------------------------------------------------------------------#

  #########################################
  # Locate package assets in dev/install use #
  #########################################
  asset_dir <- system.file("comparison-dashboard", package = "forecastEvalReport")
  if(!nzchar(asset_dir)) asset_dir <- file.path("inst", "comparison-dashboard")

  css_path <- file.path(asset_dir, "comparison.css")
  js_path <- file.path(asset_dir, "comparison.js")
  if(!file.exists(css_path) || !file.exists(js_path)){
    stop("Dashboard assets could not be found. Reinstall forecastEvalReport ",
         "and try again.", call. = FALSE)
  }

  css <- paste(readLines(css_path, warn = FALSE, encoding = "UTF-8"),
               collapse = "\n")
  javascript <- paste(readLines(js_path, warn = FALSE, encoding = "UTF-8"),
                      collapse = "\n")

  ##############################################
  # Serialize data safely inside a script tag #
  ##############################################
  payload <- list(
    generated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    title = title,
    evaluation = evaluation,
    models = models,
    parameters = parameters,
    forecasts = forecasts,
    truth = truth,
    performance = performance,
    peak_details = peak_details,
    trend_horizon = 2L,
    season_start_month = suppressWarnings(as.integer(substr(
      resolved_eval_config$season_start_day_month, 1L, 2L))),
    trend_stable_threshold = resolved_eval_config$stable_threshold,
    metric_descriptions = metric_descriptions()
  )

  json <- jsonlite::toJSON(
    payload, dataframe = "rows", auto_unbox = TRUE, na = "null",
    null = "null", Date = "ISO8601", digits = 10
  )

  # Prevent user-supplied text from ending the JSON script element
  json <- gsub("&", "\\\\u0026", json, fixed = TRUE)
  json <- gsub("<", "\\\\u003c", json, fixed = TRUE)
  json <- gsub(">", "\\\\u003e", json, fixed = TRUE)

  ################################
  # Minimal HTML escaping helper #
  ################################
  escape_html <- function(x){
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }

  generated <- format(Sys.time(), "%B %d, %Y at %I:%M %p %Z")
  evaluation_tag <- switch(
    evaluation,
    testing = "Testing-period evaluation",
    realtime = "Real-time evaluation",
    both = "Testing-period + real-time evaluation"
  )
  html <- paste0(
    "<!doctype html>\n<html lang=\"en\">\n<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", escape_html(title), "</title>\n<style>\n", css,
    "\n</style>\n</head>\n<body>\n",
    "<a class=\"skip\" href=\"#main\">Skip to dashboard</a>\n",
    "<header class=\"hero\"><div class=\"eyebrow\">forecastEvalReport</div>",
    "<h1>", escape_html(title), "</h1>",
    "<p>A clear view of model forecasts, observed outcomes, testing and real-time performance, and data coverage.</p>",
    "<div class=\"hero-meta\"><span class=\"pill\">", escape_html(evaluation_tag), "</span>",
    "<span class=\"pill\">Generated ", escape_html(generated), "</span>",
    "</div></header>\n",
    "<main id=\"main\" class=\"dashboard\">",
    "<section class=\"summary-grid\" aria-label=\"Comparison summary\">",
    "<article class=\"summary-card\"><div class=\"summary-label\">Models</div><div id=\"summaryModels\" class=\"summary-value\">—</div></article>",
    "<article class=\"summary-card\"><div class=\"summary-label\">Locations</div><div id=\"summaryLocations\" class=\"summary-value\">—</div></article>",
    "<article class=\"summary-card\"><div class=\"summary-label\">Diseases included</div><div id=\"summaryDiseases\" class=\"summary-value\">—</div></article>",
    "<article class=\"summary-card\"><div class=\"summary-label\">Evaluation window</div><div id=\"summaryWindow\" class=\"summary-value summary-date\">—</div></article>",
    "</section>",
    "<section class=\"filter-shell\"><div class=\"global-filters\"><div class=\"disease-filter\"><label for=\"diseaseFilter\">Disease</label><select id=\"diseaseFilter\"></select></div><div class=\"disease-filter\"><label for=\"locationFilter\">Location</label><select id=\"locationFilter\"></select></div><div class=\"disease-filter\"><label for=\"reasonFilter\">Forecasting reason</label><select id=\"reasonFilter\"></select></div></div><div class=\"model-filter-section\"><h2 class=\"filter-title\">Models shown throughout the dashboard</h2><div id=\"modelChecks\" class=\"model-checks\"></div></div></section>",
    "<nav class=\"tabs\" aria-label=\"Dashboard sections\">",
    "<button class=\"tab active\" data-panel=\"overviewPanel\">Overview</button>",
    "<button class=\"tab\" data-panel=\"forecastPanel\">Forecasts</button>",
    "<button class=\"tab\" data-panel=\"performancePanel\">Performance</button>",
    "<button class=\"tab\" data-panel=\"configurationPanel\">Configuration</button>",
    "<button class=\"tab\" data-panel=\"coveragePanel\">Coverage</button></nav>",
    "<section id=\"overviewPanel\" class=\"panel active\"><div class=\"section-head\"><div><h2>Near-term forecast outlook</h2><p>See the direction most models expect for each disease and location over the next two forecast steps.</p></div></div><div id=\"trendIndicators\" class=\"trend-indicator-grid\"></div><p class=\"trend-method-note\"><strong>How to read this:</strong> Each model receives its own trend call from its latest median forecast. The displayed direction is the majority call across models. Consensus describes model agreement, not a statistical probability.</p><div class=\"overview-model-heading\"><h2>Models at a glance</h2><p>Methods, data, variables, and evaluation periods for each model.</p></div><div id=\"modelGrid\" class=\"model-grid\"></div></section>",
    "<section id=\"forecastPanel\" class=\"panel\"><div class=\"section-head\"><div><h2>Forecast comparison</h2><p>Compare complete horizon 0–X forecast trajectories, prediction intervals, and observed outcomes.</p></div></div>",
    "<div class=\"controls forecast-controls\">",
    "<section class=\"forecast-control-group forecast-data-group\"><div class=\"forecast-group-heading\"><span class=\"forecast-step\">1</span><div><h3>Choose the forecast data</h3><p>Select where, why, and which type of forecast to compare.</p></div></div><div class=\"forecast-fields forecast-fields-three\">",
    "<div class=\"control\"><label for=\"forecastLocation\">Location</label><span class=\"control-help\">Area displayed in the charts</span><select id=\"forecastLocation\"></select></div>",
    "<div class=\"control\"><label for=\"forecastReason\">Forecasting purpose</label><span class=\"control-help\">Why the forecasts were produced</span><select id=\"forecastReason\"></select></div>",
    "<div class=\"control\"><label for=\"forecastView\">Forecast source</label><span class=\"control-help\">Current, testing-period, or archived forecasts</span><select id=\"forecastView\"></select></div></div></section>",
    "<section class=\"forecast-control-group forecast-history-group\"><div class=\"forecast-group-heading\"><span class=\"forecast-step\">2</span><div><h3>Choose the forecast history</h3><p>Control which forecast dates and lead times are included.</p></div></div><div class=\"forecast-fields forecast-fields-two\">",
    "<div class=\"control\"><label for=\"forecastReference\">Forecast issuance</label><span class=\"control-help\">When each forecast was created</span><select id=\"forecastReference\"></select></div>",
    "<div class=\"control\"><label for=\"forecastHorizon\">Forecast horizon</label><span class=\"control-help\">How far ahead the trajectory extends</span><select id=\"forecastHorizon\"></select></div></div></section>",
    "<section class=\"forecast-control-group forecast-layer-group\"><div class=\"forecast-group-heading\"><span class=\"forecast-step\">3</span><div><h3>Choose what appears</h3><p>Show or hide uncertainty bands and actual observed values.</p></div></div><div class=\"forecast-layer-options\">",
    "<label class=\"toggle-control\"><input id=\"showForecast80\" type=\"checkbox\" checked><span><strong>80% interval</strong><small>Narrower uncertainty range</small></span></label>",
    "<label class=\"toggle-control\"><input id=\"showForecast95\" type=\"checkbox\" checked><span><strong>95% interval</strong><small>Wider uncertainty range</small></span></label>",
    "<label class=\"toggle-control\"><input id=\"showForecastObserved\" type=\"checkbox\" checked><span><strong>Observed data</strong><small>Actual reported values</small></span></label></div></section>",
    "<div class=\"forecast-actions\"><button id=\"downloadForecast\" class=\"btn\">Download displayed data</button><button id=\"printDashboard\" class=\"btn\">Print or save as PDF</button></div></div>",
    "<div id=\"forecastCharts\" class=\"forecast-chart-grid\"></div>",
    "<article class=\"table-card\"><div class=\"table-head\"><h3>Forecast data</h3><span id=\"forecastTableNote\" class=\"table-note\"></span></div><div class=\"table-wrap\"><table id=\"forecastTable\"></table></div></article></section>",
    "<section id=\"performancePanel\" class=\"panel\"><div class=\"section-head\"><div><h2>Model performance</h2><p>Explore testing-period and real-time results separately, using clear and comparable performance measures.</p></div></div>",
    "<div id=\"evaluationBreakdown\" class=\"evaluation-breakdown\" aria-label=\"Evaluation type summary\"></div>",
    "<div class=\"controls performance-controls\"><section class=\"performance-control-group\"><div class=\"forecast-group-heading\"><span class=\"forecast-step\">1</span><div><h3>Choose the evaluation results</h3><p>Select the evaluation pathway and the report location to compare.</p></div></div><div class=\"performance-fields performance-evaluation-fields\">",
    "<div class=\"control\"><label for=\"perfPeriod\">Evaluation type</label><span class=\"control-help\">Held-out testing period or archived real-time forecasts</span><select id=\"perfPeriod\"></select></div>",
    "<div class=\"control\"><label for=\"perfDisease\">Disease</label><span class=\"control-help\">One outcome stream at a time</span><select id=\"perfDisease\"></select></div><div class=\"control\"><label for=\"perfLocation\">Location</label><span class=\"control-help\">One report-style location view at a time</span><select id=\"perfLocation\"></select></div><div class=\"control\"><label for=\"perfSeason\">Season</label><span class=\"control-help\">Keeps separate peak seasons from being combined</span><select id=\"perfSeason\"></select></div></div></section>",
    "<section class=\"performance-control-group\"><div class=\"forecast-group-heading\"><span class=\"forecast-step\">2</span><div><h3>Choose a report performance section</h3><p>Use the same performance families, measures, and summary levels as the individual reports.</p></div></div><div class=\"performance-fields performance-metric-fields\">",
    "<div class=\"control\"><label for=\"perfFamily\">Report section</label><span class=\"control-help\">Percent Agreement, Forecast Bias, Peak/Phase, or Traditional Metrics</span><select id=\"perfFamily\"></select></div>",
    "<div class=\"control\"><label for=\"perfScope\">Report view</label><span class=\"control-help\">Over time, horizon summary, or overall summary</span><select id=\"perfScope\"></select></div>",
    "<div class=\"control\"><label for=\"perfMetric\">Measure</label><span class=\"control-help\">The exact statistic shown in the chart</span><select id=\"perfMetric\"></select></div>",
    "<div class=\"control\"><label for=\"perfHorizon\">Forecast horizon</label><span class=\"control-help\">All or one selected lead time</span><select id=\"perfHorizon\"></select></div></div></section>",
    "<div class=\"performance-actions\"><button id=\"downloadPerformance\" class=\"btn\">Download displayed results</button></div></div>",
    "<p id=\"metricHelp\" class=\"metric-help\"></p>",
    "<article id=\"performanceChartCard\" class=\"chart-card performance-chart-card\"><div class=\"performance-chart-head\"><h3 id=\"performanceChartTitle\">Performance over time</h3><p id=\"performanceChartSubtitle\"></p><p id=\"performanceChartGuide\" class=\"performance-chart-guide\"></p></div><div class=\"canvas-wrap\"><canvas id=\"performanceCanvas\" role=\"img\" aria-label=\"Interactive performance comparison\"></canvas><div id=\"performanceTooltip\" class=\"plot-tooltip\" role=\"status\"></div><div id=\"performanceEmpty\" class=\"empty\">No numeric performance results match these choices.</div></div><div id=\"performanceLegend\" class=\"legend performance-legend\"></div></article>",
    "<article class=\"table-card\"><div class=\"table-head\"><h3>Report-aligned performance results</h3><span id=\"performanceTableNote\" class=\"table-note\"></span></div><div class=\"table-wrap\"><table id=\"performanceTable\"></table></div></article></section>",
    "<section id=\"configurationPanel\" class=\"panel\"><div class=\"section-head\"><div><h2>Model configuration and definitions</h2><p>Compare report options, evaluation thresholds, geography coverage, and crosswalk definitions across models.</p></div></div>",
    "<div class=\"controls\"><div class=\"control\"><label for=\"configModel\">Model</label><select id=\"configModel\"></select></div>",
    "<div class=\"control\"><label for=\"configLocation\">Location</label><select id=\"configLocation\"></select></div>",
    "<div class=\"control\"><label for=\"configGroup\">Parameter group</label><select id=\"configGroup\"></select></div>",
    "<div class=\"control config-search\"><label for=\"configSearch\">Search table</label><input id=\"configSearch\" type=\"search\" placeholder=\"Search values or definitions\"></div>",
    "<button id=\"downloadConfiguration\" class=\"btn\">Download filtered CSV</button></div>",
    "<article class=\"table-card configuration-table\"><div class=\"table-head\"><h3>Parameters and definitions</h3><span id=\"configurationTableNote\" class=\"table-note\"></span></div><div class=\"table-wrap\"><table id=\"configurationTable\"></table></div></article></section>",
    "<section id=\"coveragePanel\" class=\"panel\"><div class=\"section-head\"><div><h2>Coverage and validation</h2><p>Every model below completed the options, model, crosswalk, assembly, and evaluation checks required by the selected mode.</p></div></div><div class=\"validation-banner\"><strong>Validation passed for all supplied options files.</strong><span>The dashboard was created only after every file completed the matching report pathway.</span></div><div id=\"coverageGrid\" class=\"coverage-grid\"></div></section>",
    "</main><footer>Created with forecastEvalReport · This dashboard contains its data, styles, and interactions in one HTML file.</footer>\n",
    "<script id=\"dashboard-data\" type=\"application/json\">", json, "</script>\n",
    "<script>\n", javascript, "\n</script>\n</body>\n</html>"
  )

  #######################################
  # Create the output directory safely #
  #######################################
  output_dir <- dirname(output_file)
  if(!dir.exists(output_dir)){
    ok <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if(!ok) stop("Could not create the dashboard output directory:\n  ",
                 output_dir, call. = FALSE)
  }

  # Write to a temporary sibling, then move into place
  temporary <- tempfile(pattern = ".forecast-comparison-",
                        tmpdir = output_dir, fileext = ".html")
  writeLines(enc2utf8(html), temporary, useBytes = TRUE)

  if(file.exists(output_file)) unlink(output_file)
  if(!file.rename(temporary, output_file)){
    unlink(temporary)
    stop("Could not move the completed dashboard to:\n  ", output_file,
         call. = FALSE)
  }

  final_path <- normalizePath(output_file, winslash = "/", mustWork = TRUE)
  message("\u2713 Model comparison dashboard created:\n  ", final_path)
  invisible(final_path)
}
