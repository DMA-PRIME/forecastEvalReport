#' Calibrate and freeze historical weekly trend thresholds
#'
#' Applies the historical calibration framework of Mathis et al. (2025),
#' doi:10.1093/ofid/ofaf460, to the supplied data, separately by location.
#' The paper used a common FluSurv-NET reference distribution; using each
#' location's own reference distribution is an explicit local adaptation.
#' Within an epidemiological season (week 40 through week 39), differences
#' enter the reference distribution starting at the third consecutive nonzero
#' week. Missing or nonconsecutive weeks never form a weekly difference.
#' Thresholds are type-7 empirical 5/25/75/95 percentiles per 100,000,
#' retained at full numeric precision by default. Display formatting does not
#' alter classification cutoffs. The article does not specify its quantile
#' interpolation algorithm.
#'
#' @param data Historical counts: location, target_end_date (or date), and
#'   Observed (or value). Optional population supplies historical denominators.
#' @param cutoff Exclusive last date: one Date, or a data frame containing
#'   location and cutoff. Dates on/after it are never used for calibration.
#' @param population Population lookup, as for trendCallCalculation(). Used
#'   when the historical frame has no population column.
#' @param round_digits NULL (default) retains full numeric precision. An explicit
#'   integer from 0 to 10 rounds the stored cutoffs for legacy reproducibility;
#'   it changes classification, not just display. Avoid coarse rounding, which
#'   can collapse a small nonzero stability cutoff to zero. Previously saved
#'   rounded tables must be recalibrated to recover full-precision cutoffs.
#' @return A reusable data frame with location, p05/p25/p75/p95 (per 100,000),
#'   calibration_start/end, cutoff, sample size, precision metadata, and status.
#'   round_digits is NA and threshold_precision is "full" when unrounded.
#'   Insufficient or
#'   directionally invalid distributions have NA thresholds and a reason.
#' @export
calibrate_trend_thresholds <- function(data, cutoff, population = NULL,
                                     round_digits = NULL) {
  if(!is.data.frame(data)) stop("`data` must be historical observations.")
  if(!"target_end_date" %in% names(data) && "date" %in% names(data))
    data$target_end_date <- data$date
  if(!"Observed" %in% names(data) && "value" %in% names(data))
    data$Observed <- data$value
  if(!all(c("location", "target_end_date", "Observed") %in% names(data)))
    stop("Calibration requires location, target_end_date (or date), and Observed (or value).")
  if(!is.null(round_digits) && (length(round_digits) != 1L || !is.numeric(round_digits) ||
     !is.finite(round_digits) || round_digits < 0 || round_digits > 10 ||
     round_digits != floor(round_digits))) stop("`round_digits` must be NULL or an integer from 0 to 10.")
  data$location <- as.character(data$location)
  data$target_end_date <- anytime::anydate(data$target_end_date)
  data$Observed <- suppressWarnings(as.numeric(data$Observed))
  locs <- unique(data$location[!is.na(data$location)])
  if(is.data.frame(cutoff)) {
    if(!all(c("location", "cutoff") %in% names(cutoff)) ||
       anyDuplicated(as.character(cutoff$location)))
      stop("Cutoff table requires one location/cutoff row per location.")
    bounds <- stats::setNames(as.Date(cutoff$cutoff), as.character(cutoff$location))
  } else {
    cutoff <- as.Date(cutoff)
    if(length(cutoff) != 1L || is.na(cutoff)) stop("Supply one valid calibration cutoff date.")
    bounds <- stats::setNames(rep(cutoff, length(locs)), locs)
  }
  if(anyNA(bounds[locs])) stop("Every calibration location needs a cutoff date.")
  if(!"population" %in% names(data)) {
    pop <- if(is.numeric(population) && length(population) == 1L &&
              is.null(names(population)) && length(locs) == 1L)
      stats::setNames(population, locs) else resolve_population_values(locs, population)
    data$population <- unname(pop[data$location])
  }
  data$population <- suppressWarnings(as.numeric(data$population))
  result <- lapply(locs, function(loc) {
    d <- data[!is.na(data$location) & data$location == loc & !is.na(data$target_end_date) &
                data$target_end_date < bounds[loc], , drop = FALSE]
    d <- unique(d[c("target_end_date", "Observed", "population")])
    if(anyDuplicated(d$target_end_date)) stop("Conflicting historical truth for ", loc, ".")
    d <- d[order(d$target_end_date), , drop = FALSE]
    changes <- numeric()
    used_dates <- as.Date(character())
    run <- 0L; started <- FALSE; prior_season <- NA_integer_
    for(i in seq_len(nrow(d))) {
      date <- d$target_end_date[i]
      season <- lubridate::epiyear(date) - as.integer(lubridate::epiweek(date) < 40L)
      if(is.na(prior_season) || season != prior_season) {run <- 0L; started <- FALSE}
      adjacent <- i > 1L && season == prior_season &&
        as.numeric(date - d$target_end_date[i - 1L]) == 7
      valid <- is.finite(d$Observed[i]) && d$Observed[i] >= 0 &&
        is.finite(d$population[i]) && d$population[i] > 0
      run <- if(valid && d$Observed[i] > 0) if(adjacent) run + 1L else 1L else 0L
      if(run >= 3L) started <- TRUE
      if(started && adjacent && valid && is.finite(d$Observed[i-1L]) &&
         d$Observed[i-1L] >= 0 && is.finite(d$population[i-1L]) && d$population[i-1L] > 0) {
        changes <- c(changes, 1e5 * (d$Observed[i] / d$population[i] -
                                    d$Observed[i-1L] / d$population[i-1L]))
        used_dates <- c(used_dates, date)
      }
      prior_season <- season
    }
    q <- if(length(changes)) stats::quantile(changes, c(.05,.25,.75,.95),
                                           names = FALSE, type = 7)
         else rep(NA_real_, 4L)
    if(!is.null(round_digits)) q <- round(q, round_digits)
    status <- if(!length(changes)) "No eligible historical weekly changes" else
      if(any(!is.finite(q)) || q[1] >= 0 || q[2] > 0 || q[3] < 0 || q[4] <= 0)
        "Historical cutoffs do not support both increase and decrease categories" else "ok"
    if(status != "ok") q[] <- NA_real_
    data.frame(location = loc, p05=q[1], p25=q[2], p75=q[3], p95=q[4],
               calibration_start=if(nrow(d)) min(d$target_end_date) else as.Date(NA),
               calibration_end=if(nrow(d)) max(d$target_end_date) else as.Date(NA),
               cutoff=unname(bounds[loc]), n_weekly_changes=length(changes),
               rate_multiplier=1e5,
               round_digits=if(is.null(round_digits)) NA_integer_ else as.integer(round_digits),
               threshold_precision=if(is.null(round_digits)) "full" else "rounded",
               status=status)
  })
  if(!length(result)) return(data.frame())
  do.call(rbind, result)
}

# Internal: a calibration boundary precedes both issue and scored target dates.
trend_calibration_cutoffs <- function(data) {
  data %>% dplyr::group_by(location) %>% dplyr::summarise(
    cutoff = {
      dates <- as.Date(target_end_date)
      if("reference_date" %in% names(data)) dates <- c(dates, as.Date(reference_date))
      if(all(is.na(dates))) as.Date(NA) else min(dates, na.rm=TRUE)
    }, .groups="drop")
}

validate_frozen_trend_thresholds <- function(thresholds, cutoffs) {
  required <- c("location", "p05", "p25", "p75", "p95", "calibration_end")
  if(!is.data.frame(thresholds) || !all(required %in% names(thresholds)))
    stop("Frozen thresholds require location, p05, p25, p75, p95, and calibration_end.")
  thresholds$location <- as.character(thresholds$location)
  if(anyNA(thresholds$location) || anyDuplicated(thresholds$location))
    stop("Frozen thresholds require one row per location.")
  idx <- match(as.character(cutoffs$location), thresholds$location)
  if(anyNA(idx)) stop("Frozen thresholds are missing evaluation locations.")
  thresholds <- thresholds[idx, , drop=FALSE]
  if(!all(vapply(thresholds[c("p05","p25","p75","p95")], is.numeric, logical(1))))
    stop("Frozen rate thresholds must be numeric.")
  if(!"status" %in% names(thresholds)) thresholds$status <- "ok"
  if(anyNA(thresholds$status)) stop("Frozen threshold status cannot be missing.")
  q <- as.matrix(thresholds[c("p05","p25","p75","p95")])
  ok <- thresholds$status == "ok"
  valid <- apply(q, 1L, function(x) all(is.finite(x)) && all(diff(x) >= 0) &&
                   x[1] < 0 && x[2] <= 0 && x[3] >= 0 && x[4] > 0)
  if(any(ok & !valid)) stop("Frozen cutoffs must be ordered and support both change directions.")
  end <- as.Date(thresholds$calibration_end)
  if(any(ok & (is.na(end) | end >= as.Date(cutoffs$cutoff))))
    stop("Trend calibration must end before every evaluated issue and target date.")
  if("rate_multiplier" %in% names(thresholds) &&
     any(!is.finite(thresholds$rate_multiplier) | thresholds$rate_multiplier != 1e5))
    stop("Frozen cutoffs must be expressed per 100,000 population.")
  thresholds[!ok, c("p05", "p25", "p75", "p95")] <- NA_real_
  thresholds
}

trend_methods_html <- function() {
  paste0("Weekly count changes are converted to rates per 100,000 and classified ",
    "using frozen historical 5th, 25th, 75th, and 95th percentile cutoffs. ",
    "New calibrations retain full numeric precision by default; displayed values ",
    "are formatted separately and do not change classification. Saved cutoffs ",
    "are used as supplied; recalibrate previously rounded tables to recover precision. ",
    "Calibration ends before the evaluated forecasts and targets; evaluation outcomes ",
    "never set these cutoffs. Calibration begins at the third consecutive nonzero ",
    "weeks in each epidemiological season starting at week 40. The default raw-count ",
    "stability override is an absolute change below 10; this setting is separate ",
    "from the percentage-bias count floor. See the calibration metadata for the ",
    "historical dates, sample sizes, and rate thresholds. This applies the ",
    "<a href=\"https://doi.org/10.1093/ofid/ofaf460\">Mathis et al. framework</a> ",
    "to each location's supplied historical series.")
}

# Format a copy for display only. Significant digits preserve small nonzero cutoffs.
format_trend_threshold_table <- function(metadata) {
  for(column in intersect(c("p05", "p25", "p75", "p95"), names(metadata))) {
    value <- metadata[[column]]
    metadata[[column]] <- ifelse(is.na(value), NA_character_,
                                 trimws(formatC(value, digits=8, format="g")))
  }
  metadata
}

trend_calibration_metadata_html <- function(metadata, eval_config = NULL) {
  count <- if(is.null(eval_config$trend_count_threshold)) 10 else eval_config$trend_count_threshold
  details <- htmltools::tags$p(paste0("Configured raw-count stability override: absolute weekly change < ",
    count, ". Phase stratification is retrospective and uses the observed testing-period peak."))
  if(is.data.frame(metadata) && nrow(metadata)) {
    columns <- intersect(c("location", "p05", "p25", "p75", "p95",
      "calibration_start", "calibration_end", "n_weekly_changes",
      "threshold_precision", "round_digits", "status"), names(metadata))
    details <- htmltools::tagList(details,
      htmltools::HTML(knitr::kable(format_trend_threshold_table(metadata[columns]),
                                  format="html", escape=TRUE)))
  }
  htmltools::tags$details(
    htmltools::tags$summary("Historical trend calibration (rates per 100,000)"), details)
}
