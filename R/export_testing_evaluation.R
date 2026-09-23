#' Export the testing-period evaluation without rendering a report
#'
#' Runs the same data pipeline `generate_report()` feeds into the report
#' template -- validating the options file, loading and validating the model
#' files and variables crosswalk, assembling the master data set, and building
#' the testing-period evaluation via `build_testing_evaluation()` -- but stops
#' before any RMarkdown render. Instead of an HTML report it returns the
#' testing-period metrics and, on request, writes Markdown, CSV, and JSON:
#' an AI-ready Markdown summary (`ai_form`) and/or a tidy long-format CSV
#' (`save_results`), plus standalone JSON (`save_json`). The CSV is designed so several runs can later be stacked
#' to build cross-model comparison tables.
#'
#' This function never calls `rmarkdown::render()` and never alters
#' `generate_report()`; it is a standalone entry point that reuses the same
#' helper pathway. The model-file and master-data extractors are called with
#' `save_data = FALSE`, so the only files written are the outputs requested
#' through `ai_form` / `save_results` / `save_json`. No AI service is called.
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
#' @param return_context Logical. When `TRUE`, the invisible return value also
#'   includes the validated configuration, model data, metadata, variables
#'   crosswalk, and assembled master data used to produce the scores. Defaults
#'   to `FALSE`. This is primarily used by package workflows that need to reuse
#'   the verified report data without repeating the complete preparation step.
#'
#' @param save_json Logical. Write standalone JSON with metadata, effective settings,
#'   results, forecast/truth pairs, definitions, WIS diagnostics, and trend calibration.
#'   Default FALSE. JSON always includes per-forecast results.
#' @param population_crosswalk Optional named numeric population vector or
#'   location/population data frame for trend labels.
#' @return Invisibly, a named list with `testing_eval` (the raw bundle from
#'   `build_testing_evaluation()`), `results` (the tidy long-format data
#'   frame), `payload` (the JSON-ready structured results), and `paths` (the written file paths, or `NA` when not requested).
#'
#' @export
export_testing_evaluation <- function(options_file,
                                       output_dir   = getwd(),
                                       ai_form      = FALSE,
                                       save_results = FALSE,
                                       ai_row_detail = FALSE,
                                       eval_config  = NULL,
                                       file_prefix  = NULL,
                                       quiet        = TRUE,
                                       return_context = FALSE,
                                       save_json = FALSE,
                                       population_crosswalk = NULL) {
  validate_evaluation_export_args(ai_form,save_results,save_json,ai_row_detail,quiet,return_context,file_prefix)

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
# prepared frame and the four metric families (similarity index, forecast     #
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

  context <- list(config=config, implementation_model=implementation_model,
    evaluation_model=evaluation_model, variables_crosswalk=variables_crosswalk,
    impl_meta=impl_meta, eval_meta=eval_meta, master_data=master_data)
  finish_evaluation_export(testing_eval,context,"testing",eval_config,output_dir,
    ai_form,save_results,save_json,ai_row_detail,file_prefix,quiet,return_context,population_crosswalk)
}
