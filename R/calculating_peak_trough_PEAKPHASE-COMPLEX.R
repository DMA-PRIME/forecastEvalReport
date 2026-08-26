#' Calculate peak-phase timing and magnitude evaluation metrics
#'
#' Derives peak-phase indicators by identifying, for each season and location,
#' the contiguous "peak phase" window around the observed global maximum -- the
#' unbroken run of observed values that stay within `peak_window` percent of
#' that maximum. For each forecast that captures the peak phase, it computes a
#' rank-matched magnitude offset (the forecast's top-m predictions paired,
#' sorted high-to-low, with the m observed peak-phase values) and a phase-window
#' timing offset (signed step distance from the forecast's peak date to the
#' observed phase window). These per-forecast metrics are summarized -- mean,
#' range, median, quantiles, and a hit rate -- by distance-from-peak horizon
#' label and overall per location, weighted one row per forecast. Operates on
#' the testing evaluation dataset from prepare_testing_evaluation_data() and
#' processes all locations present.
#'
#' @param data.for.evaluation Testing evaluation data frame from
#'   `prepare_testing_evaluation_data()`. Must contain `reference_date`,
#'   `location`, `target_end_date`, `value`, `horizon`, and `Observed`.
#' @param season_start_day_month Character. Season start as "Month DD"
#'   (e.g. "August 01"). Only the month is used to assign seasons.
#' @param peak_window Numeric. Tolerance around the observed global max, as a
#'   PERCENT of that max (e.g. 20 = within 20% of the peak), used to define the
#'   contiguous peak-phase window. Default 20.
#' @param non_transmission_months Integer vector of calendar months (1-12)
#'   treated as the non-transmission season; excluded from peak detection and
#'   aggregates only (rows are kept). Default c(5, 6, 7) (May-July).
#' @param time_step Integer. The data's dominant time step in days (e.g. 7 for
#'   weekly) from `eval_meta$time_step`. Used to convert horizons and date
#'   differences into step units. Default 7.
#' @param timing_tol_steps Numeric. Tolerance, in time steps, within which a
#'   timing miss still counts as "On Time" in the directional labels (e.g. 1 =
#'   peaking within one step of the observed phase is on-time). Default 1.
#' @param mag_tol Numeric between 0 and 1. Minimum rank-matched min/max
#'   agreement (offMagnitudePA) at or above which a forecast's peak magnitude
#'   counts as "On Target" in the directional labels (0.80 ~ within ~20% of the
#'   observed peak height). Default 0.80.
#'
#' @return A data frame of per-row peak-phase indicators with summary columns,
#'   or an empty data frame when no usable rows are present or a stage fails.
#'
#' @keywords internal
#' @noRd
calculating_peak_trough_PEAKPHASE <- function(data.for.evaluation,
                                              season_start_day_month = "August 01",
                                              peak_window = 20,
                                              non_transmission_months = c(5, 6, 7),
                                              time_step = 7,
                                              timing_tol_steps = 1L,
                                              mag_tol = 0.80){

#------------------------------------------------------------------------------#
# Confirming function should be run --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms that the evaluation data is not empty, and is   #
# a data frame. If it does not satisfy that criteria, an empty data frame is   #
# returned by the function immediately as no metrics can then be calculated.   #
#------------------------------------------------------------------------------#

  #######################################################
  # Triggered if there is an issue with evaluation data #
  #######################################################
  if(is.null(data.for.evaluation) ||
     !is.data.frame(data.for.evaluation) ||
     nrow(data.for.evaluation) == 0){

    # Returning an empty data frame
    return(data.for.evaluation)

  }

  #########################################################
  # Empty-result sentinel returned whenever a stage bails #
  #########################################################

  # Returning 0 for every column of a row when error occurs
  empty_result <- data.for.evaluation[0, , drop = FALSE]

  # Point metrics use the median forecast only (the 0.5 quantile)
  data.for.evaluation <- data.for.evaluation %>%
    dplyr::filter(output_type_id == 0.5)

#------------------------------------------------------------------------------#
# Checking for required columns ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks for all columns required to conduct the peak      #
# phase analysis. If any are missing, this section returns an empty result     #
# so the rest of the report keeps running and the users are aware of what      #
# columns are missing.                                                         #
#------------------------------------------------------------------------------#

  ###############################
  # Columns needed for analysis #
  ###############################
  needed <- c("reference_date", "location", "target_end_date",
              "value", "horizon", "Observed")

  ############################
  # Flagging missing columns #
  ############################
  missing_cols <- setdiff(needed, names(data.for.evaluation))

  ######################################
  # Triggered if an error occurs above #
  ######################################
  if(length(missing_cols) > 0){

    # Message to show to users
    message(
      "calculating_peak_trough_PEAKPHASE(): input is missing required ",
      "column(s), so an empty result is returned.\n",
      "  - Missing: ", paste(missing_cols, collapse = ", "), "\n",
      "  - Columns present: ", paste(names(data.for.evaluation), collapse = ", "), "\n",
      "  - Expected: ", paste(needed, collapse = ", ")
    )

    # Returning the empty result
    return(empty_result)

  }

#------------------------------------------------------------------------------#
# Preparing the grouped data set -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the initial data for use in the peak            #
# calculations below. This includes renaming variables, flagging the 'off'     #
# period for a given disease, and keeping only the forecast dates (i.e, the    #
# week the forecast was conducted) that has the complete set of horizons       #
# associated with it. Therefore, this will often trim some of the early        #
# forecasts.                                                                   #
#------------------------------------------------------------------------------#

  #########################################
  # Trying to create the grouped data set #
  #########################################
  groupedData <- tryCatch({

    # Preparing the data
    data.for.evaluation %>%

      # Renaming columns and flagging 'non-transmission' months
      dplyr::mutate(
        forecastValue   = value,
        targetValue     = Observed,
        is_transmission = !lubridate::month(target_end_date) %in% non_transmission_months
      ) %>%

      # Grouping by date forecast was conducted & location
      dplyr::group_by(reference_date, location) %>%

      # Determining the size of a 'complete' horizon count
      dplyr::mutate(horizonCount = dplyr::n()) %>%

      # Ungrouping the date for easy later on
      dplyr::ungroup() %>%

      # Selecting the needed variables
      dplyr::select(reference_date, target_end_date, horizon, location,
                    forecastValue, targetValue, is_transmission) %>%

      # Sorting the data set
      dplyr::arrange(reference_date)

  ########################################################################
  # Triggering an error if something happens with the above calculations #
  ########################################################################
  }, error = function(e){

    # Message to show to users
    message(
      "calculating_peak_trough_PEAKPHASE(): failed while preparing the grouped ",
      "data set, so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - Columns present: ", paste(names(data.for.evaluation), collapse = ", "), "\n",
      "  - target_end_date class: ",
      paste(class(data.for.evaluation$target_end_date), collapse = "/"),
      " (must be Date for lubridate::month)\n",
      "  - Rows in: ", nrow(data.for.evaluation)
    )

    # Returning NULL for this data set
    NULL

  })

  ##########################################
  # Bail cleanly if the stage failed/empty #
  ##########################################

  # Returning the row of 0
  if(is.null(groupedData))      return(empty_result)

  # Returning the empty data set
  if(nrow(groupedData) == 0)    return(groupedData)

  # Number of unique horizons drives the single-horizon timing-NA logic below
  n_horizons <- length(unique(groupedData$horizon))

#------------------------------------------------------------------------------#
# Assigning a season -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the user inputted season start string (M/D) to      #
# assign the reference dates to seasons. A season is considered a year         #
# (e.g. "2025-2026") starting on the date provided by the user. The default    #
# in case of a season string being unparsable is August 1.                     #
#------------------------------------------------------------------------------#

  ########################################################
  # Trying to parse the user provided season to a number #
  ########################################################
  season_month <- match(sub(" .*", "", season_start_day_month), month.name)

  # Defaults to August 1
  if(is.na(season_month)) season_month <- 8L

  #################################################################
  # Trying to assign a season to each location/reference date row #
  #################################################################
  dataWSeason <- tryCatch({

    groupedData %>%

      dplyr::mutate(

        # Parsing the reference date to 'date'
        reference_date = anytime::anydate(reference_date),

        # Parsing the target end date to date
        target_end_date = anytime::anydate(target_end_date),

        ######################
        # Assigning a season #
        ######################
        season = ifelse(

          # Condition: If reference date comes before the start of the season
          as.numeric(format(reference_date, "%m")) < season_month,

          # Return season as {CURRENT YEAR - 1} - {CURRENT YEAR}
          paste0(as.numeric(format(reference_date, "%Y")) - 1, "-",
                 format(reference_date, "%Y")),

          # Return season as {CURRENT YEAR} - {CURRENT YEAR + 1}
          paste0(format(reference_date, "%Y"), "-",
                 as.numeric(format(reference_date, "%Y")) + 1)

        )

      )

  #####################################
  # Triggers if an error occurs above #
  #####################################
  }, error = function(e){

    # Message to show to users
    message(
      "calculating_peak_trough_PEAKPHASE(): failed while assigning seasons, ",
      "so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - season_start_day_month supplied: ", season_start_day_month, "\n",
      "  - resolved season month: ", season_month, "\n",
      "  - reference_date class: ",
      paste(class(groupedData$reference_date), collapse = "/"),
      " (must be coercible to Date)"
    )

    # Returning 'dataWSeason' as NULL item
    NULL

  })

  ##########################################
  # Bail cleanly if the stage failed/empty #
  ##########################################
  if(is.null(dataWSeason)) return(empty_result)

#------------------------------------------------------------------------------#
# Finding the seasonal 'global' max --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section finds the global max of each season/location combination #
# where each season and location Each location also get's its own max to       #
# ensure that unique location behaviors are captured.                          #
#------------------------------------------------------------------------------#

  #################################
  # Trying to find the season max #
  #################################
  seasonMax <- tryCatch({

    #########################################
    # Updating the data set with season max #
    #########################################
    dataWSeason %>%

      # One max per season/location
      dplyr::group_by(season, location) %>%

      # For each group, finding global max
      dplyr::summarise(

        # Calculating global max
        globalMax = {

          # Pulling the "in season/not-low transmission" indicator
          vals <- targetValue[is_transmission]

          # Finding max for not-low transmission dates
          if(all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)

        },

        # Dropping repeat rows
        .groups = "drop"

      )

  ######################################
  # Triggered if an error occurs above #
  ######################################
  }, error = function(e){

    # Pulling the number location/season combos
    n_groups <- tryCatch(
      dplyr::n_groups(dplyr::group_by(dataWSeason, season, location)),
      error = function(e2) NA)

    # Message to show to user if error occurs above
    message(
      "calculating_peak_trough_PEAKPHASE(): failed while computing the per-season ",
      "global maximum, so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - season x location groups: ", n_groups, "\n",
      "  - targetValue class: ", paste(class(dataWSeason$targetValue), collapse = "/")
    )

    # Returning NULL for data set
    NULL

  })

  ##########################################
  # Bail cleanly if the stage failed/empty #
  ##########################################
  if(is.null(seasonMax)) return(empty_result)

#------------------------------------------------------------------------------#
# Identifying the contiguous peak-phase window ---------------------------------
#------------------------------------------------------------------------------#
# About: For each season/location, this section determines the peak phase, the #
# window around the "peak" used for peak timing and magnitude calculations.    #
# The peak phase is anchored at the observed peak and only includes the        #
# unbroken run of observed dates that stay within peak_window of that peak;    #
# a date is dropped if the values between it and the peak fall outside the     #
# window. This section works on the observed values only; the forecasts are    #
# sorted relative to this window later, when the phase labels are assigned.    #
#------------------------------------------------------------------------------#

  ########################################
  # Trying to find the peak phase window #
  ########################################
  peakPhaseDates <- tryCatch({

    # Creating the data set
    dataWSeason %>%

      # Adding the season max values for groups
      dplyr::left_join(seasonMax, by = c("season", "location")) %>%

      # Keeping the distinct groups
      dplyr::distinct(season, location, target_end_date, targetValue,
                      globalMax, is_transmission) %>%

      # Sorting by season, location, and target end date
      dplyr::arrange(season, location, target_end_date) %>%

      # Grouping by season and location
      dplyr::group_by(season, location) %>%

      dplyr::mutate(

        ####################################################
        # Checking if the date falls within the peak phase #
        ####################################################
        withinThreshold =

          # Conditions:

          # 1. Its within the transmission window
          is_transmission &

          # 2. A target value is available
          !is.na(targetValue) &

          # 3. A global max is provided
          !is.na(globalMax) &

          # 4. Checking if value is within the peak window of the global peak
          abs(targetValue - globalMax) <= (peak_window / 100) * globalMax,

        #############################################################
        # Checking the index of the global max for a forecast group #
        #############################################################
        peakIdx = {

          # Looking for matches by condition
          hit <- which(

            # Conditions:

            # 1. Date is in transmission window
            is_transmission &

              # 2. The target value is not NA
              !is.na(targetValue) &

              # 3. The global max is not NA
              !is.na(globalMax) &

              # 4. Finding the global max
              targetValue == globalMax)

          # Handling no hits and ties
          if(length(hit) == 0) NA_integer_ else hit[1]
        },

        # Adding row numbers
        rowIdx = dplyr::row_number()) %>%

      dplyr::mutate(

        ######################################################
        # Determining which dates fall within the peak phase #
        ######################################################
        inPeakPhase = purrr::map_lgl(rowIdx, function(i){

          # Handling if peak is NA
          if(is.na(peakIdx[1])) return(FALSE)

          # Peak row (global peak) is always in the peak phase
          if(i == peakIdx[1]) return(TRUE)

          # List of indexes between peak and index value
          path <- if(i < peakIdx[1]) i:peakIdx[1] else peakIdx[1]:i

          # Checking for continuous indexes that are within peak phase
          all(withinThreshold[path])

        })) %>%

      # Ungrouping variables
      dplyr::ungroup() %>%

      # Filtering to only keep rows that are within the peak phase
      dplyr::filter(inPeakPhase) %>%

      # Selecting the needed variables for the cross walk
      dplyr::select(season, location, target_end_date) %>%

      # Peak phase indicator (i.e., the date falls in the peak phase)
      dplyr::mutate(isPeakPhaseDate = 1L)

  #######################################
  # Triggered if an error happens above #
  #######################################
  }, error = function(e){

    # Number of groups
    n_groups <- tryCatch(
      dplyr::n_groups(dplyr::group_by(dataWSeason, season, location)),
      error = function(e2) NA)

    # Message to show to users
    message(
      "calculating_peak_trough_PEAKPHASE(): failed while identifying the ",
      "contiguous peak-phase window, so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - peak_window supplied: ", peak_window, "\n",
      "  - season x location groups: ", n_groups, "\n",
      "  - Tip: this stage maps purrr::map_lgl over per-group indices; an error ",
      "here usually means an empty group or an unexpected NA in ",
      "targetValue/globalMax."
    )

    # Returning the data set created as NULL
    NULL

  })

  ##########################################
  # Bail cleanly if the stage failed/empty #
  ##########################################
  if(is.null(peakPhaseDates)) return(empty_result)

#------------------------------------------------------------------------------#
# Creating the reference date flags --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section joins the peak-phase flags back on, derives the season's #
# peak-phase start/end dates, and labels each reference date relative to the   #
# peak phase (at peak, N steps from peak, decreasing, etc.).                   #
#------------------------------------------------------------------------------#

  ###################################
  # Trying to create the peak flags #
  ###################################
  with_cut_off <- tryCatch({

    # Creating the data frame with flags
    dataWSeason %>%

      # Combining the season/location max from above with forecast data
      dplyr::left_join(seasonMax, by = c("season", "location")) %>%

      # Combining the peak phase dates from above with the forecast data
      dplyr::left_join(peakPhaseDates,
                       by = c("season", "location", "target_end_date")) %>%

      # Flag indicator that any NA dates (dates in forecast not in peakPhaseDates) as 0
      dplyr::mutate(isPeakPhaseDate = tidyr::replace_na(isPeakPhaseDate, 0L)) %>%

      # Grouping by reference date and location
      dplyr::group_by(reference_date, location) %>%

      # Determining forecast dates that are in peak phase
      dplyr::mutate(containsPeakPhase = as.integer(any(isPeakPhaseDate == 1))) %>%

      # Ungroup by location and reference date
      dplyr::ungroup() %>%

      # Group by location and season
      dplyr::group_by(season, location) %>%

    ###########################################
    # Creating the peak phase reference flags #
    ###########################################
    dplyr::mutate(

      # Length (in time steps) of peak phase
      peakPhaseLen = length(unique(target_end_date[isPeakPhaseDate == 1])),

      # Determining the first reference date that could capture the peak phase
      firstMaxDate = if(any(containsPeakPhase == 1))
        suppressWarnings(min(reference_date[containsPeakPhase == 1], na.rm = TRUE))
      else as.Date(NA),

      # Determining the last forecast date that captures the peak phase
      lastMaxDate = if(any(isPeakPhaseDate == 1))
        suppressWarnings(max(target_end_date[isPeakPhaseDate == 1], na.rm = TRUE))
      else as.Date(NA),

      # Determining the start date of the peak phase (the first observed
      # peak-phase target date; mirrors lastMaxDate so both borders live on
      # the target-date axis and borderStart <= borderEnd always holds)
      borderStart = if(any(isPeakPhaseDate == 1))
        suppressWarnings(min(target_end_date[isPeakPhaseDate == 1], na.rm = TRUE))
      else as.Date(NA),

      # Determining the end date of the peak phase
      borderEnd   = lastMaxDate,

      # Flagging forecasts that occur during the incline (pre peak)
      phaseIndicator = ifelse(!is.na(firstMaxDate) & reference_date < firstMaxDate,
                              0, containsPeakPhase),

      # Flagging forecasts that occur during the decline (post peak)
      phaseIndicator = ifelse(!is.na(lastMaxDate) &
                                reference_date > lastMaxDate &
                                as.numeric(format(reference_date, "%m")) < season_month,
                              2, phaseIndicator),

      ###############################################################
      # Creating the horizon labels ({Time Step} from Peak {Phase}) #
      ###############################################################
      horizonLabel = dplyr::case_when(

        # At the window: "At Peak Phase" if multi-date, else "At Peak"
        containsPeakPhase == 1 &
          !is.na(borderStart) & !is.na(borderEnd) &
          reference_date >= borderStart & reference_date <= borderEnd ~
          (if(peakPhaseLen[1] > 1) "At Peak Phase" else "At Peak"),

        # Approaching: measure the step distance and label by phase length
        phaseIndicator == 1 & !is.na(borderStart) ~ {
          step_val    <- as.numeric((borderStart - reference_date) / time_step)
          unit_word   <- if(time_step == 7) "Week" else if(time_step == 1) "Day" else "Step"
          anchor_word <- if(peakPhaseLen[1] > 1) "Peak Phase Start" else "Peak"
          paste0(step_val,
                 ifelse(step_val == 1, paste0(" ", unit_word),
                        paste0(" ", unit_word, "s")),
                 " from ", anchor_word)
        },

        # Returning NA if neither is true
        TRUE ~ NA_character_)

    ) %>%

      # Ungroup by location and season
      dplyr::ungroup()

  ###############################################
  # Triggered if error occurs in the above code #
  ###############################################
  }, error = function(e){

    # Error message to show to users
    message(
      "calculating_peak_trough_PEAKPHASE(): failed while building phase / ",
      "horizon labels, so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - time_step supplied: ", time_step, "\n",
      "  - resolved season month: ", season_month, "\n",
      "  - Tip: this stage does date arithmetic (borderStart/borderEnd) and ",
      "case_when labeling; confirm target_end_date and reference_date are Dates."
    )

    # Returning NULL for this data frame
    NULL

  })

  ##########################################
  # Bail cleanly if the stage failed/empty #
  ##########################################
  if(is.null(with_cut_off)) return(empty_result)

#------------------------------------------------------------------------------#
# Helpers for the peak metrics and summaries -----------------------------------
#------------------------------------------------------------------------------#
# About: This section defines the helpers used by the peak timing, magnitude,  #
# and summary code below: a 'safe' extreme that returns NA rather than warning #
# on all-NA input; two top-n magnitude helpers that pair the forecast's OWN n  #
# highest predictions (wherever they fall) with the n highest observed peak-   #
# phase values, each side sorted high-to-low and paired by rank, so that a     #
# timing miss is not charged a second time as a magnitude error; and the       #
# quantile grid, stat names, and one-metric summary builder (mean, range,      #
# median, quantiles).                                                          #
#------------------------------------------------------------------------------#

  ##############################################################
  # Extreme (max) that returns NA instead of warning on all-NA #
  ##############################################################
  safe_extreme <- function(x, fun){

    # Returning NA if all inputs are NA
    if(all(is.na(x))) return(NA_real_)

    # Running the inputted function
    fun(x, na.rm = TRUE)

  }

  ################################
  # Top-n raw magnitude function #
  ################################
  rank_matched_raw <- function(fc, obs, isp){

    # Observed peak-phase dates this forecast could overlap (its possible hits)
    keep <- isp == 1 & !is.na(obs)

    # n = number of those overlapping peak-phase dates
    n <- sum(keep)

    # No overlap with the observed peak phase -> NA
    if(n == 0) return(NA_real_)

    # Usable (non-NA) forecast predictions across this forecast's horizons
    fc_all <- fc[!is.na(fc)]

    # Too few forecast values to fill n ranks -> NA
    if(length(fc_all) < n) return(NA_real_)

    # The n highest observed peak-phase values, sorted high-to-low
    obs_pk <- sort(obs[keep], decreasing = TRUE)

    # The forecast's OWN n highest predictions, wherever they fall, high-to-low
    fc_pk  <- sort(fc_all, decreasing = TRUE)[seq_len(n)]

    # Rank-paired mean signed difference (+ over-forecast, - under-forecast)
    mean(fc_pk - obs_pk)

  }

  #####################################
  # Top-n magnitude percent agreement #
  #####################################
  rank_matched_pa <- function(fc, obs, isp){

    # Observed peak-phase dates this forecast could overlap (its possible hits)
    keep <- isp == 1 & !is.na(obs)

    # n = number of those overlapping peak-phase dates
    n <- sum(keep)

    # No overlap with the observed peak phase -> NA
    if(n == 0) return(NA_real_)

    # Usable (non-NA) forecast predictions across this forecast's horizons
    fc_all <- fc[!is.na(fc)]

    # Too few forecast values to fill n ranks -> NA
    if(length(fc_all) < n) return(NA_real_)

    # The n highest observed peak-phase values, sorted high-to-low
    obs_pk <- sort(obs[keep], decreasing = TRUE)

    # The forecast's OWN n highest predictions, wherever they fall, high-to-low
    fc_pk  <- sort(fc_all, decreasing = TRUE)[seq_len(n)]

    # Per-rank denominator for PA
    denom <- pmax(fc_pk, obs_pk)

    # Per-rank min/max ratio (undefined where the denominator is 0)
    ratio <- ifelse(denom == 0, NA_real_, pmin(fc_pk, obs_pk) / denom)

    # Mean over ranks (NA only if every rank was undefined)
    if(all(is.na(ratio))) NA_real_ else mean(ratio, na.rm = TRUE)

  }

  #################################################
  # Quantile grid (same set as percent agreement) #
  #################################################
  peak_quantile_probs <- c(0.10, 0.20, 0.25, 0.30, 0.40,
                           0.60, 0.70, 0.75, 0.80, 0.90, 0.95)

  ###################################################
  # Stat order = column order from peak_summary_one #
  ###################################################
  peak_stat_names <- c("mean", "min", "max", "median",
                       paste0("q", peak_quantile_probs * 100))

  ######################
  # One-metric summary #
  ######################
  peak_summary_one <- function(x, metric, suffix){

    # Looking for and then removing all NAs
    x <- x[!is.na(x)]

    # Handling an empty data set
    if(length(x) == 0){

      # Returning row of NA
      vals <- rep(NA_real_, length(peak_stat_names))

    # Handling a summary row
    }else{

      # Creating the summary row
      vals <- c(mean(x), min(x), max(x), stats::median(x),
                stats::quantile(x, probs = peak_quantile_probs, na.rm = TRUE, names = FALSE))

    }

    # Adding the names of the variables
    names(vals) <- paste0(peak_stat_names, metric, suffix)

    # Returning the resulting data frame
    as.data.frame(as.list(vals), check.names = FALSE, stringsAsFactors = FALSE)

  }

#------------------------------------------------------------------------------#
# Peak timing, magnitude, and peak-phase summaries -----------------------------
#------------------------------------------------------------------------------#
# About: For each forecast (reference_date x season x location) that can       #
# capture the peak, this section scores the forecast against the OBSERVED peak #
# phase on two axes:                                                           #
#                                                                              #
#   - Magnitude: top-n. With n the number of peak-phase dates this forecast    #
#     could overlap (its possible hits), the forecast's OWN n highest          #
#     predictions -- wherever they fall in its horizon set -- are paired,      #
#     sorted high-to-low, with the n highest observed peak-phase values; the   #
#     per-rank score is averaged (raw difference, plus a 0-1 min/max ratio).   #
#     n = 0 -> NA. Comparing the forecast's own peak heights (not the forecast #
#     sampled on the observed dates) keeps magnitude timing-independent, so a  #
#     pure timing miss is charged once (to timing), not twice. The single-peak #
#     case is just n = 1: forecast max vs observed max.                        #
#   - Timing: point-to-window. The forecast's peak date (the midpoint of the   #
#     horizons achieving its max) is tested against the observed phase window  #
#     [firstPeakDate, lastPeakDate]: a hit (offTime = 0) if it falls inside,   #
#     else the signed step gap to the nearest bound -- early if before, late   #
#     if after. A single-date phase reduces to peak date vs peak date.         #
#                                                                              #
# It then summarizes these per-forecast metrics -- mean, range, median,        #
# quantiles, plus a hit rate and the number of matched weeks -- grouped by     #
# distance-from-peak label (horizonLabel x location: how we do 3 weeks out, 2  #
# weeks out, at peak) and overall per location. Summaries use one row per      #
# forecast so horizon count cannot bias them, and are broadcast back via joins.#
#------------------------------------------------------------------------------#

  #################################################
  # Trying to build the per-forecast peak metrics #
  #################################################
  peakIndicators <- tryCatch({

    ##########################################################
    # Phase profile per season/location: the observed window #
    ##########################################################
    phase_profile <- dataWSeason %>%

      # Keep only the observed peak-phase dates
      dplyr::semi_join(peakPhaseDates, by = c("season", "location", "target_end_date")) %>%

      # One row per observed peak-phase date
      dplyr::distinct(season, location, target_end_date) %>%

      # Group by season and location
      dplyr::group_by(season, location) %>%

      # Collapse to the observed phase window per season/location
      dplyr::summarise(

        # First date in peak phase
        firstPeakDate = min(target_end_date, na.rm = TRUE),

        # Last date in peak phase
        lastPeakDate  = max(target_end_date, na.rm = TRUE),

        # Dropping repeat rows
        .groups = "drop"

      )

    ##########################################################
    # Row-level metrics: forecast peak vs the observed phase #
    ##########################################################
    row_level <- with_cut_off %>%

      # Attach each location's observed phase window
      dplyr::left_join(phase_profile, by = c("season", "location")) %>%

      # Group by forecast (one reference date's full set of horizons)
      dplyr::group_by(reference_date, season, location) %>%

      dplyr::mutate(

        # Forecast peak height = the forecast's own maximum over its horizons
        peakForecastValue = safe_extreme(forecastValue, max),

        # Forecast peak date = midpoint of the horizons achieving that max, so a
        # flat or tied forecast resolves to the centre of its max plateau rather
        # than to an arbitrary first horizon
        forecastPeakDate = {

          # Horizons (target dates) achieving the forecast max
          idx <- which(forecastValue == peakForecastValue)

          # NA when there are no usable forecast values, else the midpoint date
          if(length(idx) == 0 || all(is.na(forecastValue))) as.Date(NA)
          else as.Date(round(stats::median(as.numeric(target_end_date[idx]))),
                       origin = "1970-01-01")

        },

        # Magnitude: forecast's own top-n vs the observed peak-phase top-n
        offMagnitude = rank_matched_raw(forecastValue, targetValue, isPeakPhaseDate),

        # Top-n percent agreement (0-1; x100 at table time)
        offMagnitudePA = rank_matched_pa(forecastValue, targetValue, isPeakPhaseDate),

        # Number of peak-phase dates this forecast could overlap (the n behind it)
        nMatched = sum(isPeakPhaseDate == 1 & !is.na(targetValue)),

        # Hit: the forecast's peak date falls within the observed phase window
        hit = dplyr::if_else(

          # Handling NA rows (no forecast peak date or no observed phase window)
          is.na(forecastPeakDate) | is.na(firstPeakDate) | is.na(lastPeakDate),
          NA,

          # Point-in-window test: peak date between the phase bounds (inclusive)
          forecastPeakDate >= firstPeakDate & forecastPeakDate <= lastPeakDate

        ),

        # Timing offset (steps): 0 inside the window, else signed gap to the
        # nearest bound -- early (negative) if before, late (positive) if after
        offTime = dplyr::case_when(

          # Handling NA rows
          is.na(forecastPeakDate) | is.na(firstPeakDate) | is.na(lastPeakDate) ~ NA_real_,

          # Forecast peaks before the phase -> early (negative)
          forecastPeakDate < firstPeakDate ~ as.numeric(forecastPeakDate - firstPeakDate) / time_step,

          # Forecast peaks after the phase -> late (positive)
          forecastPeakDate > lastPeakDate ~ as.numeric(forecastPeakDate - lastPeakDate) / time_step,

          # Peak date inside the phase window -> hit
          TRUE ~ 0)) %>%

      # Un-grouping by forecast
      dplyr::ungroup()

    ##################################################
    # Collapsing to ONE row per forecast for summary #
    ##################################################
    per_forecast <- row_level %>%
      dplyr::distinct(reference_date, season, location, horizonLabel,
                      offTime, offMagnitude, offMagnitudePA, hit, nMatched)

    #############################################################
    # Per-label summaries: how we do 3 weeks out, at peak, etc. #
    #############################################################
    label_summary <- per_forecast %>%

      # Only forecasts that can capture the peak (a real peak-phase label)
      dplyr::filter(!is.na(horizonLabel)) %>%

      # One summary per distance-from-peak label, within each location & season
      dplyr::group_by(season, horizonLabel, location) %>%

      # Re-creating the metrics data set
      dplyr::reframe(cbind(
        peak_summary_one(offTime,        "OffTime",  ""),     # timing distribution
        peak_summary_one(offMagnitude,   "OffMag",   ""),     # magnitude distribution
        peak_summary_one(offMagnitudePA, "OffMagPA", ""),     # PA-ratio distribution
        data.frame(hitRate     = mean(hit, na.rm = TRUE),     # proportion landing in phase
                   meanMatched = mean(nMatched, na.rm = TRUE),# avg overlapping weeks
                   minMatched  = min(nMatched,  na.rm = TRUE),# fewest overlapping weeks
                   maxMatched  = max(nMatched,  na.rm = TRUE))# most overlapping weeks
      ))

    ############################################################
    # Overall summaries: per location, agnostic of how far out #
    ############################################################
    overall_summary <- per_forecast %>%

      # Same restriction: only forecasts that can capture the peak
      dplyr::filter(!is.na(horizonLabel)) %>%

      # Collapse across all distance labels, within each location & season
      dplyr::group_by(season, location) %>%

      # Re-creating the metrics data set
      dplyr::reframe(cbind(
        peak_summary_one(offTime,        "OffTime",  "Overall"), # timing distribution
        peak_summary_one(offMagnitude,   "OffMag",   "Overall"), # magnitude distribution
        peak_summary_one(offMagnitudePA, "OffMagPA", "Overall"), # PA-ratio distribution
        data.frame(hitRateOverall     = mean(hit, na.rm = TRUE),     # proportion in phase
                   meanMatchedOverall = mean(nMatched, na.rm = TRUE))# avg overlapping weeks
      ))

    ##############################################
    # Broadcasting both summary levels onto rows #
    ##############################################
    row_level %>%
      dplyr::left_join(label_summary,   by = c("season", "horizonLabel", "location")) %>%
      dplyr::left_join(overall_summary, by = c("season", "location"))

  ###############################################################
  # Triggered if the peak timing / summary stage fails to build #
  ###############################################################
  }, error = function(e){

    # Diagnostic group count (guarded so the handler can't itself error)
    n_label_groups <- tryCatch(
      dplyr::n_groups(dplyr::group_by(with_cut_off, horizonLabel, location)),
      error = function(e2) NA)

    # Message to show to users
    message(
      "calculating_peak_trough_PEAKPHASE(): failed while computing peak ",
      "timing / magnitude and their summaries, so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - time_step supplied: ", time_step, "\n",
      "  - horizonLabel x location groups: ", n_label_groups, "\n",
      "  - n_horizons: ", n_horizons
    )

    # Returning nothing if error occurs
    NULL

  })

  ###############################################
  # Bail to an empty result if the stage failed #
  ###############################################
  if(is.null(peakIndicators)) return(empty_result)

#------------------------------------------------------------------------------#
# Directional and magnitude labels ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates human-readable timing direction                  #
# (early/late/on-time) and magnitude direction (over/under/on-target), each    #
# with a tolerance band so a near-miss still counts: a timing offset within    #
# timing_tol_steps of the observed window is 'On Time', and a magnitude whose  #
# min/max agreement is at least mag_tol is 'On Target'. These are cosmetic,    #
# so a failure here degrades softly: the indicators are returned without the   #
# labels.                                                                      #
#------------------------------------------------------------------------------#

  #################################
  # Trying to add the soft labels #
  #################################
  peakIndicators <- tryCatch({

    peakIndicators %>%

      dplyr::mutate(

        ####################
        # Timing direction #
        ####################
        color_group = dplyr::case_when(
          is.na(offTime)              ~ NA_character_,
          offTime < -timing_tol_steps ~ "Forecast Early",
          offTime >  timing_tol_steps ~ "Forecast Late",
          TRUE                        ~ "On Time"
        ),

        #######################################################################
        # Human-readable timing offset (reports the raw steps off the window) #
        #######################################################################
        bias_label = {

          # Unit word for the message (week / day / step)
          unit_word <- if(time_step == 7) "week" else if(time_step == 1) "day" else "step"

          # Creating the timing offset label
          dplyr::case_when(

            # Handling NAs
            is.na(offTime)              ~ NA_character_,

            # Handling early forecasts
            offTime < -timing_tol_steps ~ paste0(abs(offTime), " ",
                                                 ifelse(abs(offTime) == 1, unit_word, paste0(unit_word, "s")),
                                                 " early"),

            # Handling late forecasts
            offTime >  timing_tol_steps ~ paste0(offTime, " ",
                                                 ifelse(offTime == 1, unit_word, paste0(unit_word, "s")),
                                                 " late"),

            # Handling on time forecasts
            TRUE                        ~ "On time")

        },

        #######################
        # Magnitude direction #
        #######################
        mag_group = dplyr::case_when(

          # Handling NAs
          is.na(offMagnitude)                                ~ NA_character_,

          # Handling on time in cushion
          !is.na(offMagnitudePA) & offMagnitudePA >= mag_tol ~ "On Target",

          # Handling large overestimates
          offMagnitude > 0                                   ~ "Overestimate",

          # Handling small overestimates
          offMagnitude < 0                                   ~ "Underestimate",

          # Handling true perfect matches
          TRUE                                               ~ "On Target")

      )

  ######################################
  # Triggered if an error occurs above #
  ######################################
  }, error = function(e){

    # Message to show to users
    message(
      "calculating_peak_trough_PEAKPHASE(): failed while adding directional / ",
      "magnitude labels; returning the indicators computed so far without the ",
      "label columns.\n",
      "  - Reason: ", conditionMessage(e)
    )

    # Returning the indicators computed so far
    peakIndicators

  })

#------------------------------------------------------------------------------#
# Single-horizon timing suppression --------------------------------------------
#------------------------------------------------------------------------------#
# About: With only one horizon a forecast has no trajectory, so its "peak" is  #
# just its single value and timing is not meaningful. All timing fields (the   #
# per-row offset, the full OffTime summary set at both levels, the hit flag    #
# and hit rates, and the directional labels) are set to NA. Magnitude stays.   #
#------------------------------------------------------------------------------#

  #########################################
  # Checking if there is only one horizon #
  #########################################
  if(n_horizons == 1){

    #####################################
    # List of columns related to timing #
    #####################################
    timing_cols <- c("offTime",
                     paste0(peak_stat_names, "OffTime"),
                     paste0(peak_stat_names, "OffTime", "Overall"),
                     "hitRate", "hitRateOverall")

    ################################
    # Making all timing columns NA #
    ################################
    peakIndicators <- peakIndicators %>%
      dplyr::mutate(
        dplyr::across(dplyr::any_of(timing_cols), ~ NA_real_),
        hit         = NA,
        color_group = NA_character_,
        bias_label  = NA_character_
      )
  }

  ################################
  # Returning the metric results #
  ################################
  return(peakIndicators)

}
