#' Calculate point and probabilistic forecast scores
#'
#' MAE is mean absolute error of the median. WIS and its directional components
#' require a complete, finite, noncrossing symmetric quantile grid beyond the
#' median. By default the grid is the union of declared quantiles at each
#' location within the evaluation season; it is shared across dates, horizons,
#' and seasons at that location. Missing quantiles never become a reduced-grid
#' WIS. A supplied quantile_grid explicitly fixes the scoring grid for all
#' locations (additional quantiles are ignored for WIS). Coverage uses each
#' stated interval independently. All aggregates average forecast cases once,
#' not quantile rows, and omit unavailable scores.
#' @keywords internal
#' @noRd
traditionalMetricsCalculation <- function(data.for.evaluation,
                                          non_transmission_months = c(6, 7),
                                          season_month = 8,
                                          quantile_grid = NULL) {
  if(is.null(data.for.evaluation) || !is.data.frame(data.for.evaluation) ||
     nrow(data.for.evaluation) == 0L) return(data.for.evaluation)
  keys <- c("location", "reference_date", "target_end_date", "horizon")
  needed <- c(keys, "output_type_id", "value", "Observed")
  missing <- setdiff(needed, names(data.for.evaluation))
  if(length(missing)) stop("Missing evaluation columns: ", paste(missing, collapse=", "))
  df <- data.for.evaluation
  if(!all(vapply(df[c("output_type_id", "value", "Observed")], is.numeric, logical(1))))
    stop("Quantile levels, predictions and observations must be numeric.")
  if(length(season_month) != 1L || !is.finite(season_month) ||
     season_month != floor(season_month) || season_month < 1 || season_month > 12)
    stop("season_month must be an integer month from 1 to 12.")
  ref <- as.Date(df$reference_date)
  year <- as.integer(format(ref, "%Y"))
  year <- year - as.integer(as.integer(format(ref, "%m")) < season_month)
  df$season <- ifelse(is.na(year), NA_character_, paste0(year, "-", year+1L))
  canonical_q <- function(q) round(q, 10L)
  valid_grid <- function(q) length(q) >= 3L && all(is.finite(q)) &&
    all(q > 0 & q < 1) && !anyDuplicated(q) && .5 %in% q &&
    all(canonical_q(1-q) %in% q)
  if(!is.null(quantile_grid)) {
    if(!is.numeric(quantile_grid)) stop("quantile_grid must be numeric.")
    quantile_grid <- sort(canonical_q(quantile_grid))
    if(!valid_grid(quantile_grid)) stop("quantile_grid needs unique symmetric levels, including the median and an interval.")
  }
  df$.q <- canonical_q(df$output_type_id)
  units <- df %>% dplyr::distinct(dplyr::across(dplyr::all_of(c(keys, "season"))))
  units$.unit_id <- seq_len(nrow(units))
  df <- dplyr::left_join(df, units[c(keys, ".unit_id")], by=keys)
  units$.eligible <- !is.na(as.Date(units$target_end_date)) &
    !is.na(as.Date(units$reference_date)) & is.finite(units$horizon) &
    !is.na(units$location) &
    !lubridate::month(as.Date(units$target_end_date)) %in% non_transmission_months
  metric_names <- c("WIS", "MAE", "Under", "Over", "Cov50", "Cov80", "Cov95")
  for(name in metric_names) units[[name]] <- NA_real_
  grids <- lapply(split(df[df$.unit_id %in% units$.unit_id[units$.eligible], ],
                        df$location[df$.unit_id %in% units$.unit_id[units$.eligible]]),
                  function(x) if(is.null(quantile_grid)) sort(unique(x$.q), na.last=TRUE) else quantile_grid)
  units$wis_reason <- "Outside evaluation period or invalid forecast identity"
  wis_ids <- integer()
  for(i in seq_len(nrow(units))) {
    if(!units$.eligible[i]) next
    d <- df[df$.unit_id == i, , drop=FALSE]
    if(anyDuplicated(d$.q)) stop("Duplicate quantile levels within a forecast: unit ", i, ".")
    observed <- unique(d$Observed[is.finite(d$Observed)])
    if(length(observed) > 1L) stop("Conflicting observed truth within a forecast: unit ", i, ".")
    if(!length(observed)) {units$wis_reason[i] <- "Missing observed truth"; next}
    prediction <- function(q) {
      j <- match(q, d$.q)
      if(is.na(j) || !is.finite(d$value[j])) NA_real_ else d$value[j]
    }
    units$MAE[i] <- abs(prediction(.5) - observed)
    coverage <- function(lo, hi) {
      lower <- prediction(lo); upper <- prediction(hi)
      if(is.na(lower) || is.na(upper) || lower > upper) return(NA_real_)
      as.numeric(observed >= lower && observed <= upper)
    }
    units$Cov50[i] <- coverage(.25,.75)
    units$Cov80[i] <- coverage(.1,.9)
    units$Cov95[i] <- coverage(.025,.975)
    q <- grids[[as.character(units$location[i])]]
    if(!valid_grid(q)) {units$wis_reason[i] <- "No valid common interval grid"; next}
    j <- match(q, d$.q)
    if(anyNA(j) || any(!is.finite(d$value[j]))) {
      units$wis_reason[i] <- "Incomplete quantile grid"; next
    }
    if(any(diff(d$value[j]) < 0)) {units$wis_reason[i] <- "Crossing quantiles"; next}
    units$wis_reason[i] <- "scored"
    wis_ids <- c(wis_ids, i)
  }
  if(length(wis_ids) && requireNamespace("scoringutils", quietly=TRUE)) {
    scoring_rows <- dplyr::bind_rows(lapply(wis_ids, function(i) {
      d <- df[df$.unit_id == i, , drop=FALSE]
      q <- grids[[as.character(units$location[i])]]
      d <- d[match(q,d$.q), , drop=FALSE]
      data.frame(.unit_id=i, observed=unique(d$Observed[is.finite(d$Observed)])[1],
                 predicted=d$value, quantile_level=d$.q)
    }))
    # Score each grid separately; do not ask scoringutils to mix grids.
    grid_key <- vapply(wis_ids, function(i) paste(grids[[as.character(units$location[i])]],collapse=","), character(1))
    for(ids in split(wis_ids, grid_key)) {
      input <- scoring_rows[scoring_rows$.unit_id %in% ids, , drop=FALSE]
      obj <- scoringutils::as_forecast_quantile(input, forecast_unit=".unit_id")
      metrics <- scoringutils::get_metrics(obj)
      scores <- as.data.frame(scoringutils::score(obj, metrics=metrics[c("wis","underprediction","overprediction")]))
      j <- match(scores$.unit_id, units$.unit_id)
      units$WIS[j] <- scores$wis
      units$Under[j] <- scores$underprediction
      units$Over[j] <- scores$overprediction
    }
  } else if(length(wis_ids)) {
    units$wis_reason[wis_ids] <- "scoringutils unavailable"
    warning("WIS unavailable: install scoringutils. MAE and coverage were still calculated.", call.=FALSE)
  }
  excluded <- units$.eligible & is.finite(units$MAE) & is.na(units$WIS)
  if(any(excluded)) message("WIS excluded for ", sum(excluded),
    " forecast(s): median-only, incomplete/invalid quantiles, or unavailable scoring. See wis_diagnostics.")
  # Drop previously broadcast metrics if the caller re-scores an augmented frame.
  cols <- as.vector(outer(metric_names, c("Forecast","Horizon","Season","Overall"), paste, sep="_"))
  result <- data.for.evaluation[, setdiff(names(data.for.evaluation), c(cols, "season")), drop=FALSE]
  result$season <- df$season
  per <- units[c(keys, metric_names)]
  names(per)[match(metric_names,names(per))] <- paste0(metric_names,"_Forecast")
  result <- dplyr::left_join(result, per, by=keys)
  avg <- function(x) if(any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
  group_specs <- list(Horizon=c("location","horizon"), Season=c("location","season"), Overall="location")
  for(scope in names(group_specs)) {
    by <- group_specs[[scope]]
    aggregate <- units %>% dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(metric_names), avg), .groups="drop")
    names(aggregate)[match(metric_names,names(aggregate))] <- paste0(metric_names,"_",scope)
    result <- dplyr::left_join(result, aggregate, by=by)
  }
  attr(result,"trend_history") <- attr(data.for.evaluation,"trend_history")
  attr(result,"wis_quantile_grids") <- grids
  attr(result,"wis_diagnostics") <- units[c(keys,"wis_reason")]
  result
}

wis_scoring_note <- function(data) {
  diagnostics <- attr(data,"wis_diagnostics")
  if(is.null(diagnostics)) return("")
  n <- sum(diagnostics$wis_reason == "scored")
  omitted <- sum(!diagnostics$wis_reason %in% c("scored", "Missing observed truth", "Outside evaluation period or invalid forecast identity"))
  paste0("<p><strong>WIS availability:</strong> ", n, " forecasts scored; ", omitted,
    " omitted for missing or invalid quantile information. WIS uses a common symmetric grid ",
    "within each location. MAE uses every available median/observation pair; coverage ",
    "uses each available valid interval. These metrics can therefore have different sample sizes.</p>")
}
