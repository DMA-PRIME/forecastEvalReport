#' Prepare a verified real-time comparison bundle
#'
#' Internal dashboard helper that reproduces the report's options, model,
#' crosswalk, metadata, archive, master-data, and real-time evaluation pathway
#' without rendering the report itself.
#'
#' @keywords internal
#' @noRd
prepare_comparison_realtime <- function(options_file,
                                        eval_config = NULL,
                                        quiet = TRUE,
                                        archive_current = TRUE) {

#------------------------------------------------------------------------------#
# Reading and validating the options ------------------------------------------
#------------------------------------------------------------------------------#
# About: The options file is sourced into a clean environment and passed to   #
# validate_report_params(), matching the report and testing export pathways.   #
#------------------------------------------------------------------------------#

  if(is.null(eval_config)) eval_config <- create_evaluation_config()

  opts_env <- new.env(parent = baseenv())
  tryCatch(
    source(options_file, local = opts_env),
    error = function(e){
      stop("Failed to read the options file as valid R code.\nR error: ",
           conditionMessage(e), call. = FALSE)
    }
  )

  if(!exists("report_options", envir = opts_env)){
    stop("The options file does not define a `report_options` list.",
         call. = FALSE)
  }

  opts <- get("report_options", envir = opts_env)
  if(!is.list(opts)) stop("`report_options` must be a list.", call. = FALSE)

  opts$output.dir <- normalizePath(dirname(options_file))
  opts$eval.config <- eval_config

  config <- tryCatch(
    validate_report_params(opts, verbose = FALSE),
    error = function(e){
      stop("Options file validation failed.\n", conditionMessage(e),
           call. = FALSE)
    }
  )

#------------------------------------------------------------------------------#
# Validating the model files and variables crosswalk --------------------------
#------------------------------------------------------------------------------#
# About: Real-time evaluation requires the implementation model. An evaluation #
# model is optional in this pathway, but is still validated and included in    #
# assembly whenever the options file supplies one.                             #
#------------------------------------------------------------------------------#

  if(is.na(config$implementation_model_file)){
    stop("Real-time evaluation requires an implementation model file, but ",
         "implementation.model.file is NA in the options file.", call. = FALSE)
  }

  implementation_model <- switch(
    config$reason,
    "FluSight" = validate_flusight_model(config$implementation_model_file),
    "COVIDHub" = validate_covidhub_model(config$implementation_model_file),
    "RSVHub" = validate_rsvhub_model(config$implementation_model_file),
    "MetroCast" = validate_metrocast_model(config$implementation_model_file),
    "Software" = validate_general_model(
      config$implementation_model_file,
      state_context = config$state_context),
    "Internal" = validate_general_model(
      config$implementation_model_file,
      state_context = config$state_context),
    utils::read.csv(config$implementation_model_file,
                    colClasses = c(location = "character"),
                    stringsAsFactors = FALSE)
  )

  evaluation_model <- if(!is.na(config$evaluation_model_file)){
    validate_eval_model(config$evaluation_model_file,
                        state_context = config$state_context)
  }else{NULL}

  if(is.na(config$variables_crosswalk_file)){
    stop("`variables.crosswalk.file` is required for real-time evaluation.",
         call. = FALSE)
  }

  variables_crosswalk <- validate_variables_crosswalk(
    utils::read.csv(config$variables_crosswalk_file,
                    stringsAsFactors = FALSE,
                    na.strings = c("NA", "")),
    verbose = FALSE
  )

#------------------------------------------------------------------------------#
# Reproducing the report's archive and data assembly --------------------------
#------------------------------------------------------------------------------#
# About: save_data = TRUE intentionally matches the report: the current        #
# implementation forecast is added to its Forecasts/ archive before the        #
# real-time frame is assembled from all archived forecast snapshots.           #
#------------------------------------------------------------------------------#

  impl_meta <- extract_implementation_data(
    implementation_model = implementation_model,
    config = config,
    save_data = archive_current
  )

  eval_meta <- if(!is.null(evaluation_model)){
    extract_evaluation_data(
      evaluation_model = evaluation_model,
      config = config,
      impl_meta = impl_meta
    )
  }else{NULL}

  master_result <- assemble_report_data(
    config = config,
    variables_crosswalk = variables_crosswalk,
    impl_meta = impl_meta,
    eval_meta = eval_meta,
    implementation_model = implementation_model,
    evaluation_model = evaluation_model,
    save_data = FALSE
  )

  master_data <- master_result$data
  if(is.null(master_data) || !is.data.frame(master_data) ||
     nrow(master_data) == 0L){
    stop("The assembled master data set is empty; cannot score real-time ",
         "forecasts.", call. = FALSE)
  }

  realtime_eval <- build_realtime_evaluation(
    impl_meta = impl_meta,
    master_data = master_data,
    variables_crosswalk = variables_crosswalk,
    stable_threshold = eval_config$stable_threshold,
    pct_error_cushion = eval_config$pct_error_cushion
  )

  list(
    realtime_eval = realtime_eval,
    context = list(
      config = config,
      implementation_model = implementation_model,
      evaluation_model = evaluation_model,
      variables_crosswalk = variables_crosswalk,
      impl_meta = impl_meta,
      eval_meta = eval_meta,
      master_data = master_data
    )
  )
}


#' Melt evaluation metric frames for the comparison dashboard
#'
#' Internal counterpart to the testing-export reshaper. It applies the same
#' structural rules to either testing or real-time metric frames and adds an
#' explicit evaluation-period field for dashboard filtering.
#'
#' @keywords internal
#' @noRd
melt_comparison_metrics <- function(evaluation_bundle,
                                    model_label,
                                    evaluation_period) {

#------------------------------------------------------------------------------#
# Shared output schema and value helpers --------------------------------------
#------------------------------------------------------------------------------#

  empty_results <- function(){
    data.frame(
      model = character(0), evaluation_period = character(0),
      location = character(0), season = character(0),
      horizon = character(0), scope = character(0),
      family = character(0), metric = character(0),
      value = numeric(0), value_chr = character(0),
      reference_date = character(0), target_end_date = character(0),
      stringsAsFactors = FALSE
    )
  }

  num_if <- function(x){
    if(is.numeric(x) || is.logical(x)) as.numeric(x) else NA_real_
  }
  chr_if <- function(x){
    if(is.character(x) || is.factor(x)) as.character(x) else NA_character_
  }

#------------------------------------------------------------------------------#
# Melting one metric family ----------------------------------------------------
#------------------------------------------------------------------------------#

  melt_one <- function(df, family){
    if(is.null(df) || !is.data.frame(df) || nrow(df) == 0L ||
       !"location" %in% names(df)) return(empty_results())

    id_cols <- c(
      "location", "horizon", "model", "target_end_date", "reference_date",
      "output_type", "output_type_id", "quantile", "type", "value",
      "Observed", "date", "is_transmission", "is_stable", "season"
    )

    models <- rep(model_label, nrow(df))
    locations <- as.character(df$location)
    horizons <- if("horizon" %in% names(df)) df$horizon else NULL

    row_spec <- list(
      percentAgreement = list(metrics = "per_agreement",
                              dates = "target_end_date"),
      forecastBias = list(metrics = c("raw_error", "pct_error"),
                          dates = "target_end_date"),
      traditional = list(metrics = c("WIS", "MAE", "ae_median",
                                     "interval_score"),
                         dates = c("reference_date", "target_end_date")),
      peakPhase = list(metrics = c(
        "predictedPeakTimingOff", "predictedPeakTimingLabel",
        "predictedPeakMagnitudeOff", "predictedPeakAccuracy",
        "sameDayMagnitudeOff", "sameDayAccuracy",
        "peakWeekMagnitudeOff", "peakWeekAccuracy"),
        dates = character(0), row_only = TRUE)
    )

    spec <- row_spec[[family]]
    candidates <- setdiff(names(df), id_cols)
    candidates <- candidates[vapply(candidates, function(column){
      x <- df[[column]]
      is.numeric(x) || is.character(x) || is.logical(x) || is.factor(x)
    }, logical(1))]
    if(!is.null(spec)) candidates <- setdiff(candidates, spec$metrics)

    distinct_within <- function(x, group){
      vapply(split(x, group), function(values){
        values <- values[!is.na(values)]
        length(unique(values))
      }, integer(1))
    }

    overall_cols <- horizon_cols <- character(0)
    if(is.null(spec) || !isTRUE(spec$row_only)){
      for(column in candidates){
        if(all(distinct_within(df[[column]], locations) <= 1L)){
          overall_cols <- c(overall_cols, column)
        }else if(!is.null(horizons)){
          groups <- interaction(locations, as.character(horizons), drop = TRUE)
          if(all(distinct_within(df[[column]], groups) <= 1L)){
            horizon_cols <- c(horizon_cols, column)
          }
        }
      }
    }

    pieces <- list()
    make_records <- function(base, columns, scope){
      lapply(columns, function(column){
        data.frame(
          model = base$model,
          evaluation_period = evaluation_period,
          location = base$location,
          season = NA_character_,
          horizon = if("horizon" %in% names(base)) base$horizon else NA_character_,
          scope = scope, family = family, metric = column,
          value = num_if(base[[column]]), value_chr = chr_if(base[[column]]),
          reference_date = NA_character_, target_end_date = NA_character_,
          stringsAsFactors = FALSE
        )
      })
    }

    if(length(overall_cols) > 0L){
      base <- unique(cbind(
        data.frame(model = models, location = locations,
                   stringsAsFactors = FALSE),
        df[overall_cols]
      ))
      pieces <- c(pieces, make_records(base, overall_cols, "overall"))
    }

    if(length(horizon_cols) > 0L && !is.null(horizons)){
      base <- unique(cbind(
        data.frame(model = models, location = locations,
                   horizon = as.character(horizons),
                   stringsAsFactors = FALSE),
        df[horizon_cols]
      ))
      pieces <- c(pieces, make_records(base, horizon_cols, "horizon"))
    }

    if(!is.null(spec)){
      row_cols <- intersect(spec$metrics, names(df))
      for(column in row_cols){
        records <- data.frame(
          model = models, evaluation_period = evaluation_period,
          location = locations,
          season = if("season" %in% names(df)) as.character(df$season) else NA_character_,
          horizon = if(!is.null(horizons)) as.character(horizons) else NA_character_,
          scope = "row", family = family, metric = column,
          value = num_if(df[[column]]), value_chr = chr_if(df[[column]]),
          reference_date = if("reference_date" %in% spec$dates)
            as.character(df$reference_date) else NA_character_,
          target_end_date = if("target_end_date" %in% spec$dates)
            as.character(df$target_end_date) else NA_character_,
          stringsAsFactors = FALSE
        )
        records <- records[!is.na(records$value) |
                             !is.na(records$value_chr), , drop = FALSE]
        keys <- records[c("model", "location", "season", "horizon",
                          "reference_date", "target_end_date")]
        pieces[[length(pieces) + 1L]] <-
          records[!duplicated(keys), , drop = FALSE]
      }
    }

    if(length(pieces) == 0L) empty_results() else do.call(rbind, pieces)
  }

#------------------------------------------------------------------------------#
# Stacking all available families ---------------------------------------------
#------------------------------------------------------------------------------#

  families <- c("percentAgreement", "forecastBias", "peakPhase",
                "traditional")
  pieces <- lapply(families, function(family){
    melt_one(evaluation_bundle[[family]], family)
  })
  present <- pieces[vapply(pieces, nrow, integer(1)) > 0L]
  if(length(present) == 0L) empty_results() else do.call(rbind, present)
}
