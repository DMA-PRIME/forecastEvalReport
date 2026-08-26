#' Export the testing-period evaluation without rendering a report
#'
#' Runs the same data pipeline `generate_report()` feeds into the report
#' template -- validating the options file, loading and validating the model
#' files and variables crosswalk, assembling the master data set, and building
#' the testing-period evaluation via `build_testing_evaluation()` -- but stops
#' before any RMarkdown render. Instead of an HTML report it returns the
#' testing-period metrics and, on request, writes them to disk in two forms:
#' an AI-ready Markdown summary (`ai_form`) and/or a tidy long-format CSV
#' (`save_results`). The CSV is designed so several runs can later be stacked
#' to build cross-model comparison tables.
#'
#' This function never calls `rmarkdown::render()` and never alters
#' `generate_report()`; it is a standalone entry point that reuses the same
#' helper pathway. The model-file and master-data extractors are called with
#' `save_data = FALSE`, so the only files written are the outputs requested
#' through `ai_form` / `save_results`.
#'
#' @param options_file Path to a completed report options `.R` file produced
#'   by `create_options_template()`. Must define a `report_options` list and
#'   reference a completed variables crosswalk.
#' @param output_dir Directory where the requested output file(s) are written.
#'   Defaults to the current working directory. Created if it does not exist.
#' @param ai_form Logical. When `TRUE`, writes a self-contained Markdown
#'   summary built for reading directly into an AI system alongside a prompt.
#'   Default `FALSE`.
#' @param save_results Logical. When `TRUE`, writes the full set of metrics as
#'   a tidy long-format CSV (one row per model / location / horizon / metric).
#'   Default `FALSE`.
#' @param ai_row_detail Logical. When `TRUE`, the Markdown summary also
#'   includes per-forecast (row-level) tables. Row-level results are always
#'   written to the CSV regardless of this flag. Default `FALSE`.
#' @param eval_config Optional named list from `create_evaluation_config()`.
#'   When `NULL` (default), the package defaults are used.
#' @param file_prefix Optional character stem for the output filenames. When
#'   `NULL` (default), a stem is built from the contact name, disease, reason,
#'   and model type in the options file.
#' @param quiet Logical. When `TRUE` (default), suppresses the progress
#'   messages emitted while the pipeline runs.
#'
#' @return Invisibly, a named list with `testing_eval` (the raw bundle from
#'   `build_testing_evaluation()`), `results` (the tidy long-format data
#'   frame), and `paths` (the written file paths, or `NA` when not requested).
#'
#' @export
export_testing_evaluation <- function(options_file,
                                       output_dir   = getwd(),
                                       ai_form      = FALSE,
                                       save_results = FALSE,
                                       ai_row_detail = FALSE,
                                       eval_config  = NULL,
                                       file_prefix  = NULL,
                                       quiet        = TRUE) {

#------------------------------------------------------------------------------#
# Validating the function inputs -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms the options file exists and the output          #
# directory is usable before any pipeline work is done, mirroring the          #
# front-end checks generate_report() performs so the two entry points          #
# behave consistently for the user.                                            #
#------------------------------------------------------------------------------#

  #################################################
  # A small helper to message only when not quiet #
  #################################################
  say <- function(...) if(!isTRUE(quiet)) message(...)

  #################################
  # Options file must be supplied #
  #################################
  if(missing(options_file) || is.null(options_file) || is.na(options_file)){

    # Stopping if no options file is provided
    stop(
      "No options file provided.\n\n",
      "Create a template with create_options_template(), fill it in, then ",
      "call export_testing_evaluation('path/to/report_options.R').",
      call. = FALSE
    )

  }

  ########################################
  # Options file path must exist on disk #
  ########################################
  if(!file.exists(options_file)){

    # Stopping if the options file can not be found
    stop("Options file not found: ", options_file, call. = FALSE)

  }

  ###############################################
  # Output directory must exist or be creatable #
  ###############################################
  if(!dir.exists(output_dir)){

    # Trying to create the user-provided directory
    ok <- tryCatch({dir.create(output_dir, recursive = TRUE); TRUE},
                   error = function(e) FALSE)

    # Stopping if the directory could not be created
    if(!ok){
      stop("Output directory does not exist and could not be created:\n  ",
           output_dir, call. = FALSE)
    }

  }

#------------------------------------------------------------------------------#
# Resolving the evaluation config ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section resolves the evaluation tuning settings. When the        #
# user supplies their own create_evaluation_config() list it is used as        #
# given; otherwise the package defaults are applied, matching the report       #
# behavior exactly.                                                            #
#------------------------------------------------------------------------------#

  ######################################################
  # Resolve eval_config: user list or package defaults #
  ######################################################
  if(is.null(eval_config)){

    # Use the package defaults
    eval_config <- create_evaluation_config()

  # User-provided config must be a list
  }else if(!is.list(eval_config)){

    # Stopping if the supplied config is not a list
    stop("`eval_config` must be a list from create_evaluation_config().",
         call. = FALSE)

  }

#------------------------------------------------------------------------------#
# Reading the options file -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sources the options file into a clean environment so     #
# report_options is available without touching the user's global               #
# environment, then injects the internal fields the validators and             #
# extractors expect to travel alongside the user inputs.                       #
#------------------------------------------------------------------------------#

  ########################################################
  # Source the options file into an isolated environment #
  ########################################################
  opts_env <- new.env(parent = baseenv())

  # Trying to source the options file
  tryCatch(

    # Calling the options file
    source(options_file, local = opts_env),

    # Options file errored
    error = function(e){

      # Error to show to users
      stop("Failed to read the options file as valid R code.\n",
           "R error: ", conditionMessage(e), call. = FALSE)

    }

  )

  ###############################################
  # The options file must define report_options #
  ###############################################
  if(!exists("report_options", envir = opts_env)){

    # Stopping if the list is missing
    stop("The options file does not define a `report_options` list.",
         call. = FALSE)

  }

  ###########################################
  # Extract and type-check the options list #
  ###########################################
  opts <- get("report_options", envir = opts_env)

  # Stopping if report_options is not a list
  if(!is.list(opts)){

    # Error to return to users
    stop("`report_options` must be a list, but found: ", class(opts)[1], ".",
         call. = FALSE)

  }

  ###################################################
  # Inject the internal fields the pipeline expects #
  ###################################################

  # Directory the options file lives in, used by the extractors
  opts$output.dir  <- normalizePath(dirname(options_file))

  # The resolved evaluation config, read later as EVAL_CONFIG
  opts$eval.config <- eval_config

  ############################################
  # The variables crosswalk file is required #
  ############################################
  cwf <- opts$variables.crosswalk.file

  # Stopping if the crosswalk path is missing or NA
  if(is.null(cwf) || (length(cwf) == 1L && is.na(cwf)) ||
     nchar(trimws(as.character(cwf))) == 0L){

    # Actionable error pointing at the crosswalk workflow
    stop(
      "`variables.crosswalk.file` is required but was not provided.\n",
      "Run build_crosswalk_from_options(), complete the crosswalk, then add ",
      "its path to the options file before exporting.",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Validating the report parameters ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section validates the assembled options list, confirming all     #
# required fields are present, paths exist, and enumerations are valid. The    #
# returned config object drives every downstream extractor, exactly as in      #
# the report pipeline.                                                         #
#------------------------------------------------------------------------------#

  ###############################################
  # Validate the options and capture the config #
  ###############################################
  config <- tryCatch(

    # Trying to validate report parameters
    validate_report_params(opts, verbose = FALSE),

    # Issues occurred with trying to validate report parameters
    error = function(e){

      # Error to show to users
      stop("Options file validation failed.\n", conditionMessage(e),
           call. = FALSE)

    }
  )

#------------------------------------------------------------------------------#
# Running the report data pipeline (no render) ---------------------------------
#------------------------------------------------------------------------------#
# About: This section reproduces the data-preparation chunks of the report     #
# template in order: load and validate the model files and crosswalk,          #
# extract the implementation and evaluation metadata, and assemble the         #
# master data set. The extractors run with save_data = FALSE so no             #
# Forecasts/ or Data/ folders are written. The result is the eval_meta and     #
# master_data objects build_testing_evaluation() consumes.                     #
#------------------------------------------------------------------------------#

  ###############################
  # Status message for the user #
  ###############################
  say("export_testing_evaluation(): preparing data ...")

  #########################################################
  # Load and validate the implementation model (optional) #
  #########################################################
  if(!is.na(config$implementation_model_file)){

    # Dispatching validation on the operational reason
    implementation_model <- switch(
      config$reason,
      "FluSight"  = validate_flusight_model(config$implementation_model_file),
      "COVIDHub"  = validate_covidhub_model(config$implementation_model_file),
      "RSVHub"    = validate_rsvhub_model(config$implementation_model_file),
      "MetroCast" = validate_metrocast_model(config$implementation_model_file),
      "Software"  = validate_general_model(
        config$implementation_model_file,
        state_context = config$state_context),
      "Internal"  = validate_general_model(
        config$implementation_model_file,
        state_context = config$state_context),
      # Fallback for hubs without a dedicated validator
      utils::read.csv(config$implementation_model_file,
                      colClasses       = c(location = "character"),
                      stringsAsFactors = FALSE)
    )

  # No implementation model supplied
  }else{implementation_model <- NULL}

  ##########################################################
  # Load and validate the evaluation model (required here) #
  ##########################################################
  if(!is.na(config$evaluation_model_file)){

    # Validating the evaluation model file structure
    evaluation_model <- validate_eval_model(
      config$evaluation_model_file,
      state_context = config$state_context
    )

  # No evaluation model: testing-period metrics cannot be built
  }else{

    # Stopping with a clear, actionable message
    stop(
      "An evaluation model file is required to export the testing-period ",
      "evaluation, but evaluation.model.file is NA in the options file.",
      call. = FALSE
    )

  }

  #############################################
  # Read and validate the variables crosswalk #
  #############################################
  variables_crosswalk_raw <- utils::read.csv(
    config$variables_crosswalk_file,
    stringsAsFactors = FALSE,
    na.strings       = c("NA", "")
  )

  # Validating the crosswalk contents
  variables_crosswalk <- validate_variables_crosswalk(
    variables_crosswalk_raw,
    verbose = FALSE
  )

  ###############################################################
  # Extract implementation metadata (when a model was supplied) #
  ###############################################################
  if(!is.null(implementation_model)){

    # Implementation metadata, without writing a Forecasts/ folder
    impl_meta <- extract_implementation_data(
      implementation_model = implementation_model,
      config               = config,
      save_data            = FALSE
    )

  # No implementation model: no implementation metadata
  }else{impl_meta <- NULL}

  ###############################
  # Extract evaluation metadata #
  ###############################
  eval_meta <- extract_evaluation_data(
    evaluation_model = evaluation_model,
    config           = config,
    impl_meta        = impl_meta
  )

  # Stopping if the evaluation metadata could not be built
  if(is.null(eval_meta)){

    # Error to show to users
    stop("Evaluation metadata could not be extracted from the evaluation ",
         "model file.", call. = FALSE)

  }

  ##########################################################
  # Assemble the master data set (no Data/ folder written) #
  ##########################################################
  master_result <- assemble_report_data(
    config               = config,
    variables_crosswalk  = variables_crosswalk,
    impl_meta            = impl_meta,
    eval_meta            = eval_meta,
    implementation_model = implementation_model,
    evaluation_model     = evaluation_model,
    save_data            = FALSE
  )

  # The assembled long-format master data frame
  master_data <- master_result$data

  # Stopping if the master data set is empty or missing
  if(is.null(master_data) || !is.data.frame(master_data) ||
     nrow(master_data) == 0){

    # Error to show to users
    stop("The assembled master data set is empty; cannot score the testing ",
         "period.", call. = FALSE)

  }

  ##############################################
  # The evaluation config, read as EVAL_CONFIG #
  ##############################################
  EVAL_CONFIG <- opts$eval.config

#------------------------------------------------------------------------------#
# Building the testing-period evaluation ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section calls build_testing_evaluation() with exactly the        #
# arguments the report's testing chunk uses, returning the bundle of the       #
# prepared frame and the four metric families (percent agreement, forecast     #
# bias, peak phase, and traditional scores).                                   #
#------------------------------------------------------------------------------#

  ###############################
  # Status message for the user #
  ###############################
  say("export_testing_evaluation(): scoring the testing period ...")

  ############################################
  # Run the testing-period evaluation bundle #
  ############################################
  testing_eval <- build_testing_evaluation(
    eval_meta               = eval_meta,
    master_data             = master_data,
    non_transmission_months = EVAL_CONFIG$non_transmission_months,
    season_start_day_month  = EVAL_CONFIG$season_start_day_month,
    peak_window             = EVAL_CONFIG$peak_window,
    stable_threshold        = EVAL_CONFIG$stable_threshold,
    pct_error_cushion       = EVAL_CONFIG$pct_error_cushion,
    timing_tol_steps        = EVAL_CONFIG$timing_tol_steps,
    mag_tol                 = EVAL_CONFIG$mag_tol
  )

#------------------------------------------------------------------------------#
# Melting the metrics into tidy long format ------------------------------------
#------------------------------------------------------------------------------#
# About: This section reshapes each augmented metric frame into one tidy       #
# long table. A column is treated as an overall-scope metric when it holds     #
# a single value within each location, and as a horizon-scope metric when      #
# it holds a single value within each location-by-horizon group but varies     #
# across horizons. Raw per-row quantile columns vary within a group and are    #
# therefore dropped, leaving only the summary results. This structural rule    #
# adapts automatically to whatever summary columns the helpers emit.           #
#------------------------------------------------------------------------------#

  ###################################################
  # Numeric value of x, or NA when x is not numeric #
  ###################################################
  num_if <- function(x){
    if(is.numeric(x) || is.logical(x)) as.numeric(x) else NA_real_
  }

  #########################################################
  # Character value of x, or NA when x is numeric/logical #
  #########################################################
  chr_if <- function(x){
    if(is.character(x) || is.factor(x)) as.character(x) else NA_character_
  }

  ############################################################
  # An empty long-format results frame with the fixed schema #
  ############################################################
  empty_results <- function(){
    data.frame(model = character(0), location = character(0),
               season = character(0), horizon = character(0),
               scope = character(0), family = character(0),
               metric = character(0), value = numeric(0),
               value_chr = character(0), reference_date = character(0),
               target_end_date = character(0),
               stringsAsFactors = FALSE)
  }

  ########################################################
  # Melt one augmented metric frame into the long schema #
  ########################################################
  melt_metric_frame <- function(df, family, model_label){

    # Empty / non-data-frame guard
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0){
      return(empty_results())
    }

    # A location key is required to melt
    if(!"location" %in% names(df)) return(empty_results())

    # Identity / row-level columns never treated as metrics
    id_cols <- c("location", "horizon", "model", "target_end_date",
                 "reference_date", "output_type", "output_type_id",
                 "quantile", "type", "value", "Observed", "date",
                 "is_transmission", "is_stable", "season")

    # Per-row model label
    models <- if("model" %in% names(df)){as.character(df$model)

    # Using label provided in config
    }else{rep(model_label, nrow(df))}

    # Group keys: Locations
    loc <- as.character(df$location)

    # Group keys: Horizons
    hor <- if("horizon" %in% names(df)) df$horizon else NULL

    # Candidate metric columns, restricted to atomic numeric/char/logical
    cand <- setdiff(names(df), id_cols)

    # Checking the metrics columns
    cand <- cand[vapply(cand, function(cn){
      x <- df[[cn]]
      is.numeric(x) || is.character(x) || is.logical(x) || is.factor(x)
    }, logical(1))]

    # Per-family allowlist of genuine row-level metric columns, the date
    # column(s) that identify a row, and whether the family is emitted only
    # at row scope. Peak metrics are per-horizon values, not window summaries,
    # so peak is row-only and keys on season plus horizon (it has no dates).
    row_spec <- list(
      percentAgreement = list(metrics  = c("per_agreement"),
                              dates    = c("target_end_date"),
                              row_only = FALSE),
      forecastBias     = list(metrics  = c("raw_error", "pct_error"),
                              dates    = c("target_end_date"),
                              row_only = FALSE),
      traditional      = list(metrics  = c("WIS", "MAE", "ae_median",
                                           "interval_score"),
                              dates    = c("reference_date",
                                           "target_end_date"),
                              row_only = FALSE),
      peakPhase        = list(metrics  = c("predictedPeakTimingOff",
                                           "predictedPeakTimingLabel",
                                           "predictedPeakMagnitudeOff",
                                           "predictedPeakAccuracy",
                                           "sameDayMagnitudeOff",
                                           "sameDayAccuracy",
                                           "peakWeekMagnitudeOff",
                                           "peakWeekAccuracy"),
                              dates    = character(0),
                              row_only = TRUE)
    )

    # This family's row spec, and whether it is emitted at row scope only
    spec_fam <- row_spec[[family]]
    row_only <- !is.null(spec_fam) && isTRUE(spec_fam$row_only)

    # Row-allowlisted metrics are emitted per-forecast below, so drop them
    # from the summary candidates to avoid double-counting a column
    if(!is.null(spec_fam)) cand <- setdiff(cand, spec_fam$metrics)

    # Count distinct non-NA values of x within group factor g
    distinct_within <- function(x, g){
      vapply(split(x, g), function(v){
        v <- v[!is.na(v)]; length(unique(v))
      }, integer(1))
    }

    # Classify each candidate column by scope: Overall
    overall_cols <- character(0)

    # Classify each candidate column by scope: Horizon
    horizon_cols <- character(0)

    # Row-only families skip summary classification entirely
    if(!row_only){

      # Looping each candidate to assign its summary scope
      for(cn in cand){

        # Overall when at most one distinct value per location
        if(all(distinct_within(df[[cn]], loc) <= 1L)){
          overall_cols <- c(overall_cols, cn)
          next
        }

        # Horizon when at most one distinct value per location x horizon
        if(!is.null(hor)){
          gk <- interaction(loc, as.character(hor), drop = TRUE)
          if(all(distinct_within(df[[cn]], gk) <= 1L)){
            horizon_cols <- c(horizon_cols, cn)
          }
        }

      }

    }

    # Collecting the melted pieces
    pieces <- list()

    # Overall-scope records: one row per model x location x metric
    if(length(overall_cols) > 0){
      key  <- data.frame(model = models, location = loc,
                         stringsAsFactors = FALSE)
      base <- unique(cbind(key, df[overall_cols]))
      for(cn in overall_cols){
        pieces[[length(pieces) + 1L]] <- data.frame(
          model = base$model, location = base$location,
          season = NA_character_, horizon = NA_character_,
          scope = "overall", family = family, metric = cn,
          value = num_if(base[[cn]]), value_chr = chr_if(base[[cn]]),
          reference_date = NA_character_, target_end_date = NA_character_,
          stringsAsFactors = FALSE)
      }
    }

    # Horizon-scope records: one row per model x location x horizon x metric
    if(length(horizon_cols) > 0 && !is.null(hor)){
      key  <- data.frame(model = models, location = loc,
                         horizon = as.character(hor),
                         stringsAsFactors = FALSE)
      base <- unique(cbind(key, df[horizon_cols]))
      for(cn in horizon_cols){
        pieces[[length(pieces) + 1L]] <- data.frame(
          model = base$model, location = base$location,
          season = NA_character_, horizon = base$horizon,
          scope = "horizon", family = family, metric = cn,
          value = num_if(base[[cn]]), value_chr = chr_if(base[[cn]]),
          reference_date = NA_character_, target_end_date = NA_character_,
          stringsAsFactors = FALSE)
      }
    }

    # Row-scope records: per-forecast values, keyed by the family's date(s)
    spec <- row_spec[[family]]
    if(!is.null(spec)){

      # Allowlisted row metrics actually present in this frame
      row_cols  <- intersect(spec$metrics, names(df))

      # Date columns that identify a row and are present in the frame
      date_cols <- intersect(spec$dates, names(df))

      # Only emit when at least one row metric is available
      if(length(row_cols) > 0){

        # Horizon carried with each row (NA when the frame has none)
        row_hor <- if(!is.null(hor)) as.character(hor) else
          rep(NA_character_, nrow(df))

        # Season carried with each row (NA when the frame has none), so
        # per-forecast rows stay distinct across seasons at one horizon
        row_season <- if("season" %in% names(df)){
          as.character(df$season)
        }else{rep(NA_character_, nrow(df))}

        # Reference date, only when it is a key for this family
        ref_d <- if("reference_date" %in% date_cols){
          as.character(df$reference_date)
        }else{rep(NA_character_, nrow(df))}

        # Target date, only when it is a key for this family
        tgt_d <- if("target_end_date" %in% date_cols){
          as.character(df$target_end_date)
        }else{rep(NA_character_, nrow(df))}

        # Emit each allowlisted row metric, de-duplicated on its identity
        for(cn in row_cols){
          rec <- data.frame(
            model = models, location = loc, season = row_season,
            horizon = row_hor, scope = "row", family = family, metric = cn,
            value = num_if(df[[cn]]), value_chr = chr_if(df[[cn]]),
            reference_date = ref_d, target_end_date = tgt_d,
            stringsAsFactors = FALSE)

          # Drop rows where the metric is missing
          keep <- !is.na(rec$value) | !is.na(rec$value_chr)
          rec  <- rec[keep, , drop = FALSE]

          # One value per identity key
          # (model/location/season/horizon/dates)
          dup_on <- rec[c("model", "location", "season", "horizon",
                          "reference_date", "target_end_date")]
          pieces[[length(pieces) + 1L]] <-
            rec[!duplicated(dup_on), , drop = FALSE]
        }

      }

    }

    # Returning the stacked long frame, or an empty one
    if(length(pieces) == 0) return(empty_results())
    do.call(rbind, pieces)

  }

  ###################################################
  # The config-level model label used as a fallback #
  ###################################################
  model_label <- as.character(config$general_model_type)

  ################################################
  # Melt all four metric families and stack them #
  ################################################
  results <- rbind(
    melt_metric_frame(testing_eval$percentAgreement, "percentAgreement",
                      model_label),
    melt_metric_frame(testing_eval$forecastBias,     "forecastBias",
                      model_label),
    melt_metric_frame(testing_eval$peakPhase,        "peakPhase",
                      model_label),
    melt_metric_frame(testing_eval$traditional,      "traditional",
                      model_label)
  )

#------------------------------------------------------------------------------#
# Deriving the run header metadata ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section gathers the descriptive facts that head the AI           #
# Markdown summary: the model label, disease, reason, the testing window,      #
# the locations and horizons covered, and which truth source the metrics       #
# were scored against (re-deriving the training-vs-outcome decision with       #
# the same overlap rule the data preparation uses).                            #
#------------------------------------------------------------------------------#

  #####################################################
  # Testing-window dates from the evaluation metadata #
  #####################################################
  win_start <- if(!is.null(eval_meta$testing_start)){

    as.character(eval_meta$testing_start)

  }else{NA_character_}

  # The testing-window end date
  win_end <- if(!is.null(eval_meta$testing_end)){

    as.character(eval_meta$testing_end)

  }else{NA_character_}

  ##########################################################
  # Exact testing target dates, used for the overlap check #
  ##########################################################
  testing_dates <- if(!is.null(eval_meta$testing_data) &&
                      "target_end_date" %in% names(eval_meta$testing_data)){

    # Converting date to date format
    suppressWarnings(anytime::anydate(eval_meta$testing_data$target_end_date))

  }else{as.Date(character(0))}

  # Dropping any unparseable dates
  testing_dates <- testing_dates[!is.na(testing_dates)]

  #################################################
  # Re-derive which truth source the metrics used #
  #################################################
  truth_used <- "outcome data"

  # Inspecting the master data for usable training rows
  if("variable_type" %in% names(master_data)){

    # Training truth rows present in the master data
    tr <- master_data[!is.na(master_data$variable_type) &
                       master_data$variable_type == "training_data", ]

    # Training dates that line up with the testing window
    if(nrow(tr) > 0 && "date" %in% names(tr)){
      tr_dates <- suppressWarnings(anytime::anydate(tr$date))
      if(any(tr_dates %in% testing_dates)) truth_used <- "training data"
    }

  }

  ################################################
  # Locations and horizons covered by the export #
  ################################################
  locs_covered <- if(length(results$location) > 0){
    sort(unique(results$location))
  }else{character(0)}

  # Distinct horizons present in the results
  hors_covered <- if(length(results$horizon) > 0){
    sort(unique(stats::na.omit(results$horizon)))
  }else{character(0)}

#------------------------------------------------------------------------------#
# Building the output filenames ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the shared filename stem from the contact         #
# name, disease, reason, and model type (unless the user supplied one), so     #
# the AI and CSV outputs sit together and read clearly.                        #
#------------------------------------------------------------------------------#

  ##########################################################
  # Sanitize helper matching the report's filename pattern #
  ##########################################################
  sanitize <- function(x) gsub(" ", "_", trimws(as.character(x)))

  #############################
  # Resolve the filename stem #
  #############################
  stem <- if(!is.null(file_prefix) && nzchar(file_prefix)){
    file_prefix
  }else{
    paste0(sanitize(config$contact_name), "-", sanitize(config$disease), "-",
           sanitize(config$reason), "-", sanitize(config$general_model_type),
           "-testing_evaluation")
  }

  # Full output paths for each artifact
  csv_path <- file.path(output_dir, paste0(stem, ".csv"))
  md_path  <- file.path(output_dir, paste0(stem, ".md"))

#------------------------------------------------------------------------------#
# Writing the long-format CSV --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section writes the tidy long-format results when save_results    #
# is TRUE. The fixed model / location / horizon / scope / family / metric /    #
# value schema is what the future multi-file table tool will stack across      #
# runs, so it is written verbatim with no reshaping.                           #
#------------------------------------------------------------------------------#

  ################################
  # Write the CSV when requested #
  ################################
  csv_written <- NA_character_

  # Triggering only when the user asked to save results
  if(isTRUE(save_results)){

    # Writing the long-format results to disk
    utils::write.csv(results, csv_path, row.names = FALSE)

    # Recording the written path and messaging the user
    csv_written <- csv_path
    say("export_testing_evaluation(): wrote results CSV to ", csv_path)

  }

#------------------------------------------------------------------------------#
# Writing the AI-ready Markdown summary ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section writes a single self-contained Markdown file built to    #
# be read straight into an AI system with a prompt. It opens with a context    #
# header and a short legend so each metric name is self-describing, then       #
# lays out the metrics as compact per-family tables grouped by scope.          #
#------------------------------------------------------------------------------#

  ######################################################
  # Render one Markdown table for a family/scope slice #
  ######################################################
  md_table <- function(sub){

    # Nothing to render for an empty slice
    if(nrow(sub) == 0) return(character(0))

    # Choosing the value to show (numeric, else the character label)
    shown <- ifelse(is.na(sub$value), sub$value_chr,
                    formatC(sub$value, format = "g", digits = 5))

    # Header row of the table
    head_row <- "| location | horizon | metric | value |"
    sep_row  <- "|---|---|---|---|"

    # Body rows of the table
    body <- paste0("| ", sub$location, " | ",
                   ifelse(is.na(sub$horizon), "-", sub$horizon), " | ",
                   sub$metric, " | ", shown, " |")

    # Returning the assembled table lines
    c(head_row, sep_row, body, "")

  }

  ############################################################
  # Render one Markdown row-level table (includes the dates) #
  ############################################################
  md_table_row <- function(sub){

    # Nothing to render for an empty slice
    if(nrow(sub) == 0) return(character(0))

    # Choosing the value to show (numeric, else the character label)
    shown <- ifelse(is.na(sub$value), sub$value_chr,
                    formatC(sub$value, format = "g", digits = 5))

    # Header and separator rows of the table
    head_row <- paste0("| location | season | horizon | ",
                       "reference_date | target_end_date | metric | ",
                       "value |")
    sep_row  <- "|---|---|---|---|---|---|---|"

    # Body rows of the table
    body <- paste0("| ", sub$location, " | ",
                   ifelse(is.na(sub$season), "-", sub$season), " | ",
                   ifelse(is.na(sub$horizon), "-", sub$horizon), " | ",
                   ifelse(is.na(sub$reference_date), "-",
                          sub$reference_date), " | ",
                   ifelse(is.na(sub$target_end_date), "-",
                          sub$target_end_date), " | ",
                   sub$metric, " | ", shown, " |")

    # Returning the assembled table lines
    c(head_row, sep_row, body, "")

  }

  ##################################################
  # Assemble and write the Markdown when requested #
  ##################################################
  md_written <- NA_character_

  # Triggering only when the user asked for the AI form
  if(isTRUE(ai_form)){

    # Opening context header
    md <- c(
      "# Testing-Period Forecast Evaluation",
      "",
      paste0("- Model: ", model_label),
      paste0("- Disease: ", config$disease),
      paste0("- Reason / hub: ", config$reason),
      paste0("- Truth source used for scoring: ", truth_used),
      paste0("- Testing window: ", win_start, " to ", win_end),
      paste0("- Locations: ", if(length(locs_covered) > 0)
             paste(locs_covered, collapse = ", ") else "none"),
      paste0("- Horizons: ", if(length(hors_covered) > 0)
             paste(hors_covered, collapse = ", ") else "none"),
      paste0("- Generated: ", as.character(Sys.time())),
      ""
    )

    # Short legend so the metric names are self-describing
    md <- c(md,
      "## How to read this",
      "",
      "Each row is one metric value. `scope = overall` is summarized across ",
      "the whole testing window for a location; `scope = horizon` is ",
      "summarized within a single forecast horizon. Metric names ending in ",
      "`Overall` or `Horizon` indicate that scope. `scope = row` is a ",
      "single per-forecast value, identified by its reference and/or ",
      "target end date. `value` is numeric unless a metric is a text ",
      "label.",
      ""
    )

    # Metric definitions, pulled from the shared descriptions table
    defs <- metric_descriptions()

    # The families actually present in the results
    fams_present <- unique(results$family)

    # Opening the definitions section
    md <- c(md, "## Metric definitions", "")

    # One subsection per present family, in a stable order
    for(fam in c("percentAgreement", "forecastBias", "peakPhase",
                 "traditional")){

      # Definition rows for this family
      sub <- defs[defs$family == fam, ]

      # Skipping families that are absent or undocumented
      if(!fam %in% fams_present || nrow(sub) == 0) next

      # Family heading
      md <- c(md, paste0("### ", fam), "")

      # One bullet per metric: bold label then prose
      for(i in seq_len(nrow(sub))){
        md <- c(md, paste0("- **", sub$metric[i], "** \u2014 ",
                           sub$description[i]))
      }

      # Spacer after the family
      md <- c(md, "")

    }

    # One section per metric family, overall scope first
    families <- c("percentAgreement", "forecastBias", "peakPhase",
                  "traditional")

    # Looping each family to build its tables
    for(fam in families){

      # Slice for this family
      fam_rows <- results[results$family == fam, ]

      # Skipping families with no rows
      if(nrow(fam_rows) == 0) next

      # Family heading
      md <- c(md, paste0("## ", fam), "")

      # Overall-scope table
      ov <- fam_rows[fam_rows$scope == "overall", ]
      if(nrow(ov) > 0) md <- c(md, "### Overall", "", md_table(ov))

      # Horizon-scope table
      hz <- fam_rows[fam_rows$scope == "horizon", ]
      if(nrow(hz) > 0) md <- c(md, "### By horizon", "", md_table(hz))

      # Row-level table, only when per-forecast detail was requested
      if(isTRUE(ai_row_detail)){

        # Per-forecast rows for this family
        rw <- fam_rows[fam_rows$scope == "row", ]

        # Append the row-level table when present
        if(nrow(rw) > 0){
          md <- c(md, "### Row level", "", md_table_row(rw))
        }

      }

    }

    # Writing the Markdown file to disk
    writeLines(md, md_path)

    # Recording the written path and messaging the user
    md_written <- md_path
    say("export_testing_evaluation(): wrote AI summary to ", md_path)

  }

#------------------------------------------------------------------------------#
# Returning the results --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section reports a short completion summary and returns the       #
# raw bundle, the tidy results, and any written paths invisibly so the call    #
# can be used inside a larger pipeline.                                        #
#------------------------------------------------------------------------------#

  ##############################
  # Completion summary message #
  ##############################
  message(
    "\n",
    "================================================================================\n",
    "  \u2713  Testing-period evaluation exported\n",
    "================================================================================\n",
    "  Rows in long results: ", nrow(results), "\n",
    "  Truth source used:    ", truth_used, "\n",
    "  AI summary:           ",
    if(is.na(md_written)) "not requested" else md_written, "\n",
    "  Results CSV:          ",
    if(is.na(csv_written)) "not requested" else csv_written, "\n",
    "================================================================================\n"
  )

  ###################################################
  # Return the bundle, results, and paths invisibly #
  ###################################################
  invisible(list(
    testing_eval = testing_eval,
    results      = results,
    paths        = list(ai_form = md_written, results = csv_written)
  ))

}
