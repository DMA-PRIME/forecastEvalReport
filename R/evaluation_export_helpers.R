# Shared serialization for testing and real-time exports. No scores are recomputed
# here; scopes come from the metric contract, never from observed value variation.
evaluation_export_results <- function(bundle, model, period) {
  empty <- data.frame(model=character(), evaluation_period=character(),
    location=character(), season=character(), horizon=character(), scope=character(),
    family=character(), metric=character(), value=numeric(), value_chr=character(),
    reference_date=character(), target_end_date=character(), stringsAsFactors=FALSE)
  pieces <- list()
  for(family in c("percentAgreement", "traditional", "forecastBias", "peakPhase")) {
    df <- bundle[[family]]
    if(!is.data.frame(df) || !nrow(df)) next
    rows <- switch(family,
      traditional=paste0(c("MAE","WIS","Under","Over","Cov50","Cov80","Cov95"),"_Forecast"),
      forecastBias=c("raw_error","pct_error","bias_group"),
      percentAgreement="per_agreement",
      peakPhase=c("predictedPeakTimingOff","predictedPeakTimingLabel",
        "predictedPeakMagnitudeOff","predictedPeakAccuracy","sameDayMagnitudeOff",
        "sameDayAccuracy","peakWeekMagnitudeOff","peakWeekAccuracy"))
    summaries <- if(family == "peakPhase") character() else
      grep("(Horizon|Overall|_Season|_horizon|_overall)$", names(df), value=TRUE)
    for(metric in intersect(c(rows, summaries),names(df))) {
      scope <- if(metric %in% rows) "row" else if(grepl("(Horizon|_horizon)$",metric))
        "horizon" else if(grepl("_Season$",metric)) "season" else "overall"
      field <- function(name, keep=TRUE) if(keep && name %in% names(df))
        as.character(df[[name]]) else rep(NA_character_,nrow(df))
      x <- df[[metric]]
      records <- data.frame(model=rep(as.character(model)[1],nrow(df)),
        evaluation_period=period, location=field("location"),
        season=field("season", scope == "season" || scope == "row"),
        horizon=field("horizon",scope %in% c("horizon","row")), scope=scope,
        family=family, metric=metric,
        value=if(is.numeric(x) || is.logical(x)) as.numeric(x) else NA_real_,
        value_chr=if(is.character(x) || is.factor(x)) as.character(x) else NA_character_,
        reference_date=field("reference_date",scope == "row"),
        target_end_date=field("target_end_date",scope == "row"), stringsAsFactors=FALSE)
      records$value[!is.finite(records$value)] <- NA_real_
      pieces[[length(pieces)+1L]] <- unique(records)
    }
  }
  if(!length(pieces)) empty else dplyr::bind_rows(pieces)
}

validate_evaluation_export_args <- function(ai_form, save_results, save_json,
                                            ai_row_detail, quiet, return_context,
                                            file_prefix) {
  flags <- list(ai_form=ai_form,save_results=save_results,save_json=save_json,
    ai_row_detail=ai_row_detail,quiet=quiet,return_context=return_context)
  for(nm in names(flags)) if(!is.logical(flags[[nm]]) || length(flags[[nm]]) != 1L || is.na(flags[[nm]]))
    stop("`",nm,"` must be TRUE or FALSE.",call.=FALSE)
  if(!is.null(file_prefix) && (!is.character(file_prefix) || length(file_prefix)!=1L ||
      is.na(file_prefix) || !nzchar(file_prefix) || grepl("[/\\\\]",file_prefix) || file_prefix %in% c(".","..")))
    stop("`file_prefix` must be one nonempty filename stem, without directory separators.",call.=FALSE)
}

finish_evaluation_export <- function(bundle, context, period, eval_config, output_dir,
                                     ai_form, save_results, save_json, ai_row_detail,
                                     file_prefix, quiet, return_context,
                                     population_crosswalk=NULL) {
  config <- context$config
  model <- if(is.null(config$general_model_type)) "Model" else config$general_model_type
  results <- evaluation_export_results(bundle,model,period)
  data <- bundle$data
  if(!is.data.frame(data)) data <- data.frame()
  covered <- nrow(data)>0 && "Observed" %in% names(data) && any(is.finite(data$Observed))
  truth <- attr(data,"truth_source")
  if(is.null(truth)) truth <- "unavailable"
  date_range <- function(column) {
    dates <- if(column %in% names(data)) as.Date(data[[column]]) else as.Date(character())
    dates <- dates[!is.na(dates)]
    if(length(dates)) as.character(range(dates)) else rep(NA_character_,2)
  }
  settings <- eval_config[setdiff(names(eval_config),c("trend_calibration_data","trend_thresholds"))]
  if(period == "real_time") settings$non_transmission_months <- integer()
  trend <- list(status="unavailable", reason="No scored forecast data", thresholds=data.frame(), calls=data.frame())
  if(covered) trend <- tryCatch({
    pop <- resolve_population_values(unique(data$location), custom_crosswalk=population_crosswalk,
      location_crosswalk=if(period == "testing") context$eval_meta$locations else context$impl_meta$locations)
    z <- suppressWarnings(trendCallCalculation(data,population=pop,
      week_days=if(period=="testing") {if(is.null(context$eval_meta$time_step)) 7 else context$eval_meta$time_step} else {if(is.null(context$impl_meta$time_step)) 7 else context$impl_meta$time_step},
      stable_threshold=if(is.null(eval_config$trend_count_threshold)) 10 else eval_config$trend_count_threshold,
      calibration_data=if(is.null(eval_config$trend_calibration_data)) attr(data,"trend_history") else eval_config$trend_calibration_data,
      thresholds=eval_config$trend_thresholds))
    columns <- intersect(c("location","reference_date","target_end_date","horizon",
      "observed_trend","forecast_trend","trend_agreement","anchor_source"),names(z$df))
    list(status=if(all(z$location_thresholds$status=="ok")) "ok" else "partial_or_unavailable",
      reason=if(all(z$location_thresholds$status=="ok")) NA_character_ else "See each location's calibration status",
      thresholds=z$location_thresholds,calls=z$df[columns])
  },error=function(e) list(status="unavailable",reason=conditionMessage(e),thresholds=data.frame(),calls=data.frame()))
  forecasts <- data[intersect(c("location","reference_date","target_end_date","horizon",
    "output_type_id","value","Observed"),names(data))]
  if("location" %in% names(forecasts)) forecasts$location <- as.character(forecasts$location)
  diagnostics <- attr(bundle$traditional,"wis_diagnostics")
  if(is.null(diagnostics)) diagnostics <- data.frame()
  metadata <- list(model=as.character(model), disease=config$disease, reason=config$reason,
    truth_source=truth, has_observed_targets=covered,
    status=if(covered) "scored" else "no_observed_targets",
    target_date_range=date_range("target_end_date"), reference_date_range=date_range("reference_date"),
    generated_at_utc=format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
  observed_dates <- if(covered) as.Date(data$target_end_date[is.finite(data$Observed)]) else as.Date(character())
  observed_dates <- observed_dates[!is.na(observed_dates)]
  metadata$observed_target_date_range <- if(length(observed_dates)) as.character(range(observed_dates)) else rep(NA_character_,2)
  notes <- c("Percent Accuracy (Similarity Index) is the primary reported metric. It is descriptive, not the percentage of forecasts that were correct. MAE is a secondary point-forecast measure.",
    "Unavailable numeric results are JSON null / CSV NA, never zero. WIS, MAE and coverage may use different forecast samples.",
    "Trend thresholds use frozen pre-evaluation history and full numeric precision by default; display formatting does not change cutoffs. Saved tables are used as supplied, so previously rounded tables require recalibration. Unavailable calibration is reported explicitly.",
    if(period == "testing") "Peaks and observed phase stratification use the testing period retrospectively." else
      "Real-time evaluation includes all realized weeks; peak metrics are not defined for this workflow.",
    "AI summaries contain calculated results and definitions, not AI-generated conclusions. Do not interpret missing scores as successful predictions.")
  payload <- list(schema_version="1.0",evaluation_period=period,metadata=metadata,
    settings=settings,notes=notes,metric_definitions=metric_descriptions(),
    results=results,forecasts=forecasts,
    diagnostics=list(wis=diagnostics, wis_quantile_grids=attr(bundle$traditional,"wis_quantile_grids")),trend=trend)
  sanitize <- function(x) gsub("[^[:alnum:]_.-]+","_",as.character(x))
  stem <- if(!is.null(file_prefix)) file_prefix else paste0(
    paste(sanitize(c(config$contact_name,config$disease,config$reason,model)),collapse="-"),
    if(period=="testing") "-testing_evaluation" else "-realtime_evaluation")
  if(!dir.exists(output_dir) && !dir.create(output_dir,recursive=TRUE)) stop("Could not create output directory.")
  output_dir <- normalizePath(output_dir,winslash="/",mustWork=TRUE)
  paths <- list(ai_form=NA_character_,results=NA_character_,json=NA_character_)
  if(save_results) {
    paths$results <- file.path(output_dir,paste0(stem,".csv"))
    utils::write.csv(results,paths$results,row.names=FALSE,fileEncoding="UTF-8")
  }
  if(save_json) {
    paths$json <- file.path(output_dir,paste0(stem,".json"))
    jsonlite::write_json(payload,paths$json,auto_unbox=TRUE,dataframe="rows",na="null",
                        null="null",digits=NA,pretty=TRUE,Date="ISO8601")
  }
  if(ai_form) {
    table <- function(x) {
      if(!is.data.frame(x) || !nrow(x)) return("No records available.")
      escape <- function(x) {x <- as.character(x); x[is.na(x)] <- "NA";
        x <- gsub("[\r\n]+"," ",x); gsub("|","\\|",x,fixed=TRUE)}
      y <- lapply(x,escape)
      c(paste0("| ",paste(names(x),collapse=" | ")," |"),
        paste0("| ",paste(rep("---",ncol(x)),collapse=" | ")," |"),
        apply(as.data.frame(y,stringsAsFactors=FALSE),1,function(r) paste0("| ",paste(r,collapse=" | ")," |")))
    }
    md <- c(paste0("# ",if(period=="testing") "Testing" else "Real-time"," evaluation"),"",
      paste0("- Model: ",model),paste0("- Truth source: ",truth),paste0("- Status: ",metadata$status),
      paste0("- Submitted target dates: ",paste(metadata$target_date_range,collapse=" to ")),
      paste0("- Target dates with observed truth: ",paste(metadata$observed_target_date_range,collapse=" to ")),
      paste0("- Generated (UTC): ",metadata$generated_at_utc),"","## Interpretation",paste0("- ",notes),
      "","## Effective settings",paste0("- ",names(settings),": ",vapply(settings,function(x) paste(x,collapse=", "),character(1))),
      "","## Metric definitions",table(metric_descriptions()),"","## Summary results")
    for(scope in c("overall","horizon","season")) md <- c(md,"",paste0("### ",scope),"",table(results[results$scope==scope,]))
    if(any(results$family=="peakPhase")) md <- c(md,"","## Testing-period peak summaries","",table(results[results$family=="peakPhase",]))
    if(ai_row_detail) md <- c(md,"","## Per-forecast results","",table(results[results$scope=="row" & results$family!="peakPhase",]))
    availability <- if(nrow(diagnostics)) as.data.frame(base::table(diagnostics$wis_reason),stringsAsFactors=FALSE) else data.frame()
    if(ncol(availability)) names(availability) <- c("wis_status","forecast_cases")
    md <- c(md,"","## WIS availability","",table(if(ai_row_detail) diagnostics else availability),"","## Historical trend calibration",
      paste0("Status: ",trend$status),if(!is.na(trend$reason)) trend$reason else "",table(format_trend_threshold_table(trend$thresholds)))
    if(ai_row_detail) md <- c(md,"","## Weekly trend calls","",table(trend$calls))
    paths$ai_form <- file.path(output_dir,paste0(stem,".md"))
    writeLines(enc2utf8(md),paths$ai_form,useBytes=TRUE)
  }
  if(!quiet) for(path in unlist(paths)) if(!is.na(path)) message("Wrote ",path)
  output <- list(results=results,paths=paths,payload=payload)
  output[[if(period=="testing") "testing_eval" else "realtime_eval"]] <- bundle
  if(return_context) output$context <- context
  invisible(output)
}
