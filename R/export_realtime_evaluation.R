#' Export real-time evaluation as AI-ready Markdown, JSON, and CSV
#'
#' Scores archived operational forecasts against the selected observed truth,
#' using the report's real-time preparation pathway. All realized weeks are
#' evaluated; testing-only peak metrics are omitted. By default the current
#' implementation forecast is archived first, as when generating a report.
#' This produces data for AI interpretation, not an AI-generated narrative.
#' No external AI service is called and no HTML report is rendered.
#'
#' @param options_file Completed report options R file with an implementation
#'   model and variables crosswalk. An evaluation model is optional.
#' @param output_dir Output directory, created when necessary.
#' @param ai_form Write an AI-ready Markdown summary. Default FALSE.
#' @param save_results Write the complete long-format results CSV. Default FALSE.
#' @param ai_row_detail Include per-forecast tables in Markdown. JSON and CSV
#'   always include per-forecast results. Default FALSE.
#' @param eval_config Settings from create_evaluation_config(), or NULL for defaults.
#' @param file_prefix Optional filename stem, without directory separators.
#' @param quiet Suppress preparation progress messages. Default TRUE.
#' @param return_context Include validated input context in the returned R object.
#' @param save_json Write standalone JSON with metadata, settings, results,
#'   forecast/truth pairs, metric definitions, WIS diagnostics, and trend calibration.
#' @param archive_current Archive the current implementation forecast first.
#'   Default TRUE. FALSE evaluates the existing archive without adding a snapshot.
#' @param population_crosswalk Optional population lookup for trend labels,
#'   as a named numeric vector or location/population data frame.
#' @return Invisibly, a list with realtime_eval, results, payload, paths, and
#'   optional context. Unrequested file paths are NA. Empty evaluations are
#'   explicitly marked no_observed_targets, not reported as zero error.
#' @export
export_realtime_evaluation <- function(options_file, output_dir=getwd(),
    ai_form=FALSE, save_results=FALSE, ai_row_detail=FALSE, eval_config=NULL,
    file_prefix=NULL, quiet=TRUE, return_context=FALSE, save_json=FALSE,
    archive_current=TRUE, population_crosswalk=NULL) {
  validate_evaluation_export_args(ai_form,save_results,save_json,ai_row_detail,quiet,return_context,file_prefix)
  if(!is.logical(archive_current) || length(archive_current)!=1L || is.na(archive_current))
    stop("`archive_current` must be TRUE or FALSE.")
  if(missing(options_file) || !is.character(options_file) || length(options_file)!=1L ||
      is.na(options_file) || !file.exists(options_file)) stop("Supply an existing report options R file.")
  if(is.null(eval_config)) eval_config <- create_evaluation_config()
  if(!is.list(eval_config)) stop("`eval_config` must be a list from create_evaluation_config().")
  prepare <- function() prepare_comparison_realtime(options_file,eval_config,quiet,archive_current)
  prepared <- if(quiet) suppressMessages(prepare()) else prepare()
  finish_evaluation_export(prepared$realtime_eval,prepared$context,"real_time",eval_config,
    output_dir,ai_form,save_results,save_json,ai_row_detail,file_prefix,quiet,return_context,population_crosswalk)
}
