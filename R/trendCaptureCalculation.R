#' Calculate trend-label capture and trend-offset across phases and trends
#'
#' Scores how well the forecasted trend call reproduced the observed trend call,
#' cross-cut by observed epidemic phase, observed trend, season, and forecast
#' horizon. Two complementary measures are produced from the same rows:
#'
#'   * **Capture** -- the share of eligible target periods where the forecasted
#'     trend label exactly matched the observed trend label.
#'   * **Offset** -- the signed distance between the two labels along the
#'     ordered trend ladder, so a miss carries both a direction (the forecast
#'     called the trend higher or lower than observed) and a size (how many
#'     categories away it landed).
#'
#' Both are derived from the `observed_trend`, `forecast_trend`, and
#' `trend_agreement` columns that `trendCallCalculation()` assigns, together
#' with the `phase` and `season` labels that
#' `trendPhasePerformanceCalculation()` assigns. Nothing is recomputed here; the
#' function only reshapes and summarizes what those two already established, so
#' the numbers agree with the rest of the testing-period section by
#' construction.
#'
#' Rows are eligible when they fall inside a transmission period and carry a
#' non-missing observed trend, forecasted trend, and phase. Non-transmission
#' periods are excluded for the same reason the Percent Agreement summaries
#' exclude them: an off-season trend call is not a meaningful test of the model.
#'
#' @param phase_data The row-level `data` frame returned by
#'   `trendPhasePerformanceCalculation()`. Must carry `location`,
#'   `observed_trend`, `forecast_trend`, and `phase`; `season`, `horizon`,
#'   `trend_agreement`, and `is_transmission` are used when present.
#' @param location_codes Optional character vector of location codes, used to
#'   order the location column in the returned tables. Defaults to the sorted
#'   codes present in the data.
#' @param location_labels Optional character vector of display labels parallel
#'   to `location_codes`. When supplied, a `location_display` column is added.
#' @param collapse_offsets Logical. Collapse offsets of two or more categories
#'   into single "2 or more" buckets in the `offset` summary? Defaults to
#'   `TRUE`, which keeps the diverging figure readable at five buckets rather
#'   than nine.
#'
#' @return A list with the following elements. Every table is empty (zero rows,
#'   correct columns) rather than `NULL` when nothing is eligible, so callers do
#'   not need to branch on structure:
#'   \describe{
#'     \item{`capture`}{Capture rate by location, season, horizon, phase, and
#'       observed trend.}
#'     \item{`confusion`}{Observed-label by forecasted-label counts and
#'       row-normalized shares, by season, horizon, and phase.}
#'     \item{`offset`}{Signed-offset distribution by season, horizon, phase, and
#'       observed trend.}
#'     \item{`offset_summary`}{Mean and median signed offset by location,
#'       season, horizon, phase, and observed trend.}
#'     \item{`trend_levels`}{The ordered trend ladder used throughout.}
#'     \item{`phase_levels`}{The ordered phase labels used throughout.}
#'     \item{`seasons`, `horizons`}{Sorted values available for the figure
#'       dropdowns, each with `"Overall"` first where pooled rows exist.}
#'   }
#'
#' @keywords internal
#' @noRd
trendCaptureCalculation <- function(phase_data,
                                    location_codes   = NULL,
                                    location_labels  = NULL,
                                    collapse_offsets = TRUE){

#------------------------------------------------------------------------------#
# Canonical label ladders ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: The trend ladder is the same five-level scale trendCallCalculation()   #
# assigns from population-adjusted weekly rate change against the p05, p25,     #
# p75, and p95 cutpoints. Order matters here: the offset measure is the         #
# difference between positions on this ladder, so the sequence defines what     #
# "one category too high" means.                                                #
#------------------------------------------------------------------------------#

  ############################
  # Ordered trend categories #
  ############################
  trend_levels <- c("large decrease", "decrease", "stable",
                    "increase", "large increase")

  ############################
  # Ordered phase categories #
  ############################
  phase_levels <- c("Ascension", "Peak", "Decline")

  ###################################################
  # Empty results, returned whenever nothing scores #
  ###################################################
  empty_results <- list(
    capture = data.frame(
      location = character(), location_display = character(),
      season = character(), horizon = character(), phase = character(),
      observed_trend = character(), n = integer(), hits = integer(),
      capture_pct = numeric(), stringsAsFactors = FALSE
    ),
    confusion = data.frame(
      season = character(), horizon = character(), phase = character(),
      observed_trend = character(), forecast_trend = character(),
      n = integer(), row_total = integer(), share = numeric(),
      stringsAsFactors = FALSE
    ),
    offset = data.frame(
      season = character(), horizon = character(), phase = character(),
      observed_trend = character(), bucket = character(), offset = numeric(),
      n = integer(), row_total = integer(), share = numeric(),
      stringsAsFactors = FALSE
    ),
    offset_summary = data.frame(
      location = character(), location_display = character(),
      season = character(), horizon = character(), phase = character(),
      observed_trend = character(), n = integer(), mean_offset = numeric(),
      median_offset = numeric(), exact_pct = numeric(),
      too_high_pct = numeric(), too_low_pct = numeric(),
      stringsAsFactors = FALSE
    ),
    trend_levels = trend_levels,
    phase_levels = phase_levels,
    seasons      = character(),
    horizons     = character()
  )

#------------------------------------------------------------------------------#
# Guard: usable input ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: The caller passes whatever trendPhasePerformanceCalculation() managed  #
# to produce, which is an empty frame when the trend call itself could not be   #
# scored. Missing columns are treated the same as missing rows so a partial     #
# upstream failure degrades to an empty section rather than an error.           #
#------------------------------------------------------------------------------#

  ############################
  # Input must be a data frame #
  ############################
  if(is.null(phase_data) || !is.data.frame(phase_data) ||
     nrow(phase_data) == 0L){

    return(empty_results)

  }

  ############################
  # Required columns must exist #
  ############################
  required_columns <- c("observed_trend", "forecast_trend", "phase")

  if(!all(required_columns %in% names(phase_data))){

    return(empty_results)

  }

#------------------------------------------------------------------------------#
# Normalizing the scored rows --------------------------------------------------
#------------------------------------------------------------------------------#
# About: Fills in the optional grouping columns so every downstream summary can #
# assume they exist. A report with a single season still needs a season column  #
# for the dropdown to have something to show, and a frame with no horizon       #
# column is treated as a single pooled horizon.                                 #
#------------------------------------------------------------------------------#

  ##################################
  # Working copy of the input rows #
  ##################################
  scored <- phase_data

  ###################################
  # Filling in the optional columns #
  ###################################

  # Location, when the upstream frame did not carry one
  if(!"location" %in% names(scored)){
    scored$location <- "All"
  }

  # Season, when the report covers a single unlabeled season
  if(!"season" %in% names(scored)){
    scored$season <- "All seasons"
  }

  # Horizon, when the frame is not broken out by lead time
  if(!"horizon" %in% names(scored)){
    scored$horizon <- "Overall"
  }

  # Transmission flag, when absent every row is treated as in-season
  if(!"is_transmission" %in% names(scored)){
    scored$is_transmission <- TRUE
  }

  ##########################################
  # Coercing the grouping columns to text  #
  ##########################################
  scored$location       <- as.character(scored$location)
  scored$season         <- as.character(scored$season)
  scored$horizon        <- as.character(scored$horizon)
  scored$phase          <- as.character(scored$phase)
  scored$observed_trend <- as.character(scored$observed_trend)
  scored$forecast_trend <- as.character(scored$forecast_trend)

#------------------------------------------------------------------------------#
# Restricting to eligible rows -------------------------------------------------
#------------------------------------------------------------------------------#
# About: A row can only test the trend call when both labels exist and the      #
# target period sits inside a transmission window and a resolved phase. Labels  #
# outside the canonical ladder are dropped rather than silently ordered last,   #
# so an unexpected value cannot distort the offset measure.                     #
#------------------------------------------------------------------------------#

  ##########################
  # Keeping scorable rows  #
  ##########################
  eligible <- scored[
    scored$is_transmission %in% TRUE &
      !is.na(scored$observed_trend) &
      !is.na(scored$forecast_trend) &
      !is.na(scored$phase) &
      scored$observed_trend %in% trend_levels &
      scored$forecast_trend %in% trend_levels &
      scored$phase %in% phase_levels,
    ,
    drop = FALSE
  ]

  ############################################
  # Returning empty when nothing is scorable #
  ############################################
  if(nrow(eligible) == 0L){

    return(empty_results)

  }

#------------------------------------------------------------------------------#
# Deriving capture and signed offset -------------------------------------------
#------------------------------------------------------------------------------#
# About: Capture reuses trend_agreement when the upstream frame carries it, so  #
# this section cannot drift from the definition used elsewhere in the report;   #
# it is recomputed by label comparison only when that column is absent. The     #
# offset is the forecast's ladder position minus the observed position, so a    #
# positive value means the model called the trend higher than it turned out.    #
#------------------------------------------------------------------------------#

  #################################
  # Exact-match indicator per row #
  #################################
  if("trend_agreement" %in% names(eligible)){

    # Reusing the upstream agreement flag
    eligible$captured <- as.logical(eligible$trend_agreement)

  }else{

    # Falling back to a direct label comparison
    eligible$captured <- eligible$forecast_trend == eligible$observed_trend

  }

  # Rows whose agreement could not be resolved are not scorable
  eligible <- eligible[!is.na(eligible$captured), , drop = FALSE]

  if(nrow(eligible) == 0L){

    return(empty_results)

  }

  #############################
  # Signed ladder offset per row #
  #############################
  eligible$offset <-
    match(eligible$forecast_trend, trend_levels) -
    match(eligible$observed_trend, trend_levels)

#------------------------------------------------------------------------------#
# Adding pooled horizon rows ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: Every summary reports each horizon separately plus an "Overall" row    #
# pooling all horizons, matching how the Percent Agreement and Forecast Bias    #
# sections present their horizon summaries. Pooling is done by duplicating the  #
# eligible rows under the Overall label so a single grouped summarise covers    #
# both, rather than summarizing twice and binding.                              #
#------------------------------------------------------------------------------#

  ####################################
  # Duplicating rows as Overall rows #
  ####################################
  pooled_horizon <- eligible
  pooled_horizon$horizon <- "Overall"

  # Only pool when there is more than one horizon to pool
  by_horizon <- if(length(unique(eligible$horizon)) > 1L){

    rbind(eligible, pooled_horizon)

  }else{

    eligible

  }

  ##################################
  # Duplicating rows as All seasons #
  ##################################
  # A report spanning several seasons also gets a pooled season, so the reader
  # can ask the same question of the whole testing period at once.
  pooled_season <- by_horizon
  pooled_season$season <- "All seasons"

  scored_rows <- if(length(unique(by_horizon$season)) > 1L){

    rbind(by_horizon, pooled_season)

  }else{

    by_horizon

  }

#------------------------------------------------------------------------------#
# Capture rate by location, season, horizon, phase, and trend ------------------
#------------------------------------------------------------------------------#
# About: The headline measure for question one: of the target periods that      #
# actually sat in a given phase under a given observed trend, what share did    #
# the model label correctly. Location is retained here because the tables       #
# report per-geography detail even though the figures pool across locations.    #
#------------------------------------------------------------------------------#

  #########################
  # Capture summary table #
  #########################
  capture <- stats::aggregate(
    captured ~ location + season + horizon + phase + observed_trend,
    data = scored_rows,
    FUN  = function(x) c(n = length(x), hits = sum(x))
  )

  # aggregate() returns a matrix column; splitting it into plain columns
  capture_counts    <- as.data.frame(capture$captured)
  capture$captured  <- NULL
  capture$n         <- as.integer(capture_counts$n)
  capture$hits      <- as.integer(capture_counts$hits)
  capture$capture_pct <- ifelse(
    capture$n > 0L,
    100 * capture$hits / capture$n,
    NA_real_
  )

#------------------------------------------------------------------------------#
# Observed-by-forecast confusion counts ----------------------------------------
#------------------------------------------------------------------------------#
# About: The figure for question one. Counting every observed-label by          #
# forecasted-label pair gives the diagonal (captured) and the off-diagonal      #
# mass (which label the model reached for instead). Shares are normalized       #
# within each observed label so a rare trend is still readable next to a        #
# common one.                                                                   #
#------------------------------------------------------------------------------#

  ###########################
  # Confusion pair counts   #
  ###########################
  scored_rows$pair_count <- 1L

  confusion <- stats::aggregate(
    pair_count ~ season + horizon + phase + observed_trend + forecast_trend,
    data = scored_rows,
    FUN  = sum
  )

  names(confusion)[names(confusion) == "pair_count"] <- "n"

  ###################################
  # Row totals per observed label   #
  ###################################
  row_totals <- stats::aggregate(
    n ~ season + horizon + phase + observed_trend,
    data = confusion,
    FUN  = sum
  )

  names(row_totals)[names(row_totals) == "n"] <- "row_total"

  confusion <- merge(
    confusion, row_totals,
    by = c("season", "horizon", "phase", "observed_trend"),
    all.x = TRUE
  )

  confusion$share <- ifelse(
    confusion$row_total > 0L,
    100 * confusion$n / confusion$row_total,
    NA_real_
  )

#------------------------------------------------------------------------------#
# Signed offset distribution ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: The figure for question two. Bucketing the signed offset keeps the     #
# diverging bars readable: exact matches sit at zero, one-category misses sit   #
# immediately either side, and everything further out collapses into the two    #
# outer buckets. Shares are normalized within each observed label so each bar   #
# reads as a full one hundred percent.                                          #
#------------------------------------------------------------------------------#

  ##########################
  # Bucketing the offsets  #
  ##########################
  if(isTRUE(collapse_offsets)){

    # Five buckets: two or more low, one low, exact, one high, two or more high
    scored_rows$bucket <- dplyr::case_when(
      scored_rows$offset <= -2 ~ "2 or more too low",
      scored_rows$offset == -1 ~ "1 too low",
      scored_rows$offset ==  0 ~ "Exact match",
      scored_rows$offset ==  1 ~ "1 too high",
      scored_rows$offset >=  2 ~ "2 or more too high",
      TRUE                     ~ NA_character_
    )

  }else{

    # One bucket per distinct offset value
    scored_rows$bucket <- dplyr::case_when(
      scored_rows$offset < 0 ~ paste0(abs(scored_rows$offset), " too low"),
      scored_rows$offset == 0 ~ "Exact match",
      scored_rows$offset > 0 ~ paste0(scored_rows$offset, " too high"),
      TRUE                    ~ NA_character_
    )

  }

  ###############################
  # Offset distribution counts  #
  ###############################
  offset <- stats::aggregate(
    cbind(n = pair_count, offset_total = offset) ~
      season + horizon + phase + observed_trend + bucket,
    data = scored_rows,
    FUN  = sum
  )

  # Representative offset per bucket, used only to order the stack
  offset$offset <- ifelse(
    offset$n > 0L,
    offset$offset_total / offset$n,
    NA_real_
  )

  offset$offset_total <- NULL

  ###################################
  # Row totals per observed label   #
  ###################################
  offset_totals <- stats::aggregate(
    n ~ season + horizon + phase + observed_trend,
    data = offset,
    FUN  = sum
  )

  names(offset_totals)[names(offset_totals) == "n"] <- "row_total"

  offset <- merge(
    offset, offset_totals,
    by = c("season", "horizon", "phase", "observed_trend"),
    all.x = TRUE
  )

  offset$share <- ifelse(
    offset$row_total > 0L,
    100 * offset$n / offset$row_total,
    NA_real_
  )

#------------------------------------------------------------------------------#
# Per-location offset summary --------------------------------------------------
#------------------------------------------------------------------------------#
# About: The tabular companion to the offset figure. Mean signed offset shows   #
# whether misses lean high or low on balance; reporting the too-high and        #
# too-low shares alongside it keeps a symmetric spread from being mistaken for  #
# accuracy, since opposing misses average toward zero.                          #
#------------------------------------------------------------------------------#

  ##############################
  # Offset summary statistics  #
  ##############################
  offset_summary <- stats::aggregate(
    offset ~ location + season + horizon + phase + observed_trend,
    data = scored_rows,
    FUN  = function(x) c(
      n             = length(x),
      mean_offset   = mean(x),
      median_offset = stats::median(x),
      exact_pct     = 100 * mean(x == 0),
      too_high_pct  = 100 * mean(x > 0),
      too_low_pct   = 100 * mean(x < 0)
    )
  )

  # Splitting the matrix column that aggregate() returns
  offset_stats <- as.data.frame(offset_summary$offset)
  offset_summary$offset <- NULL

  offset_summary$n             <- as.integer(offset_stats$n)
  offset_summary$mean_offset   <- offset_stats$mean_offset
  offset_summary$median_offset <- offset_stats$median_offset
  offset_summary$exact_pct     <- offset_stats$exact_pct
  offset_summary$too_high_pct  <- offset_stats$too_high_pct
  offset_summary$too_low_pct   <- offset_stats$too_low_pct

#------------------------------------------------------------------------------#
# Attaching display labels and ordering ----------------------------------------
#------------------------------------------------------------------------------#
# About: Orders every categorical column onto its canonical ladder so tables    #
# and figure axes read in epidemiological order rather than alphabetically,     #
# where "large decrease" would otherwise sort between "increase" and "stable".  #
#------------------------------------------------------------------------------#

  ##################################
  # Location display label lookup  #
  ##################################
  attach_display <- function(df){

    if(!"location" %in% names(df)) return(df)

    if(!is.null(location_codes) && !is.null(location_labels) &&
       length(location_codes) == length(location_labels)){

      # Mapping code to display label
      lookup <- stats::setNames(
        as.character(location_labels),
        as.character(location_codes)
      )

      df$location_display <- unname(lookup[df$location])

      # Falling back to the raw code when no label was supplied
      df$location_display[is.na(df$location_display)] <-
        df$location[is.na(df$location_display)]

    }else{

      # No crosswalk supplied, so the code is the label
      df$location_display <- df$location

    }

    df

  }

  capture        <- attach_display(capture)
  offset_summary <- attach_display(offset_summary)

  #################################
  # Ordering the label columns    #
  #################################
  order_labels <- function(df){

    if("observed_trend" %in% names(df)){
      df$observed_trend <- factor(df$observed_trend, levels = trend_levels)
    }

    if("forecast_trend" %in% names(df)){
      df$forecast_trend <- factor(df$forecast_trend, levels = trend_levels)
    }

    if("phase" %in% names(df)){
      df$phase <- factor(df$phase, levels = phase_levels)
    }

    df

  }

  capture        <- order_labels(capture)
  confusion      <- order_labels(confusion)
  offset         <- order_labels(offset)
  offset_summary <- order_labels(offset_summary)

  ###############################
  # Sorting into reading order  #
  ###############################
  capture <- capture[
    order(capture$location_display, capture$season, capture$horizon,
          capture$phase, capture$observed_trend), , drop = FALSE]

  confusion <- confusion[
    order(confusion$season, confusion$horizon, confusion$phase,
          confusion$observed_trend, confusion$forecast_trend), , drop = FALSE]

  offset <- offset[
    order(offset$season, offset$horizon, offset$phase,
          offset$observed_trend, offset$offset), , drop = FALSE]

  offset_summary <- offset_summary[
    order(offset_summary$location_display, offset_summary$season,
          offset_summary$horizon, offset_summary$phase,
          offset_summary$observed_trend), , drop = FALSE]

  # Dropping the stale row numbers so kable() does not print them as a column
  rownames(capture)        <- NULL
  rownames(confusion)      <- NULL
  rownames(offset)         <- NULL
  rownames(offset_summary) <- NULL

#------------------------------------------------------------------------------#
# Dropdown values --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: The values the figure dropdowns offer, ordered so the pooled option    #
# leads. Horizons are sorted numerically where they parse as numbers, so lead   #
# time 10 follows lead time 9 rather than lead time 1.                          #
#------------------------------------------------------------------------------#

  ####################
  # Available seasons #
  ####################
  seasons <- unique(as.character(scored_rows$season))
  seasons <- c(
    seasons[seasons == "All seasons"],
    sort(seasons[seasons != "All seasons"])
  )

  #####################
  # Available horizons #
  #####################
  horizons <- unique(as.character(scored_rows$horizon))
  numbered <- suppressWarnings(as.numeric(horizons[horizons != "Overall"]))

  horizons <- c(
    horizons[horizons == "Overall"],
    if(all(!is.na(numbered))){
      horizons[horizons != "Overall"][order(numbered)]
    }else{
      sort(horizons[horizons != "Overall"])
    }
  )

#------------------------------------------------------------------------------#
# Returning the scored summaries -----------------------------------------------
#------------------------------------------------------------------------------#

  list(
    capture        = capture,
    confusion      = confusion,
    offset         = offset,
    offset_summary = offset_summary,
    trend_levels   = trend_levels,
    phase_levels   = phase_levels,
    seasons        = seasons,
    horizons       = horizons
  )

}
