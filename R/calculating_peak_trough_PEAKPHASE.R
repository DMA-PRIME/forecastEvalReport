#' Calculate per-horizon peak timing and magnitude evaluation metrics
#'
#' For each season and location, evaluates how each forecast HORIZON (the
#' forecast horizon `target_end_date - reference_date`, with reference dates
#' collapsed within a horizon) captured the observed GLOBAL peak. The observed
#' global peak is the in-transmission season maximum (value `observedPeakValue`,
#' first-instance date `observedPeakDate`). Two steps are scored per horizon:
#'
#'   - `predictedPeak*` (the "M1" measure, argmax): per horizon, the highest
#'     forecast value and the target date it lands on -- i.e. how high and WHEN
#'     the lead-h projections peaked -- taken over that horizon's full in-season
#'     target span. Gives `predictedPeakTimingOff` (predicted peak date minus
#'     observed peak date, in time steps; early negative, late positive) and
#'     `predictedPeakMagnitudeOff` (predicted minus observed peak value). M1 is
#'     reported for every horizon; `horizonReachesPeak` flags whether the
#'     horizon's span actually contains the peak, and `predictedPeakNote`
#'     warns when it does not (the trajectory max may be edge-censored rather
#'     than a true peak prediction). Note that because each horizon's argmax is
#'     taken over its own target span, cross-horizon M1 magnitude comparison is
#'     not support-matched; for height comparisons across horizons prefer the
#'     `peakWeek*` (M2) measure, which is pinned to the peak week.
#'   - `peakWeek*` (the "M2" measure, value at the true peak week): per horizon,
#'     the forecast whose target IS the observed peak date, giving
#'     `peakWeekMagnitudeOff` (that value minus the observed peak value). It has
#'     no timing (the week is pinned to the truth). Where no testing forecast
#'     targets the peak week at a horizon, `peakWeekForecastExists` is FALSE,
#'     the magnitude is NA, and `peakWeekNote` explains. If the observed peak
#'     falls outside testing entirely, `peakWeekReachedInTesting` is FALSE and
#'     `peakNote` flags it.
#'   - `sameDay*` (the "M3" measure, predicted peak vs same-day truth): per
#'     horizon, the predicted-peak height minus the value actually observed on
#'     the predicted-peak date, giving `sameDayMagnitudeOff` (with
#'     `observedAtPredictedPeak` the truth on that day and `sameDayAccuracy` the
#'     min/max ratio). This isolates over- or under-shoot at the forecast's own
#'     timing; it is NA when the predicted-peak date has no observed value.
#'
#' Naming: `observedPeak*` = the truth, `predictedPeak*` = the argmax/"M1"
#' measure, `peakWeek*` = the value-at-the-true-peak-week/"M2" measure,
#' `sameDay*` = the predicted-peak-vs-same-day-truth/"M3" measure.
#'
#' Peak-phase logic is intentionally not used: the anchor is the single global
#' peak. Operates on the testing evaluation dataset from
#' `prepare_testing_evaluation_data()` and processes all locations present.
#'
#' @param data.for.evaluation Testing evaluation data frame from
#'   `prepare_testing_evaluation_data()`. Must contain `reference_date`,
#'   `location`, `target_end_date`, `value`, `horizon`, and `Observed`.
#' @param season_start_day_month Character. Season start as "Month DD"
#'   (e.g. "August 01"). Only the month is used to assign seasons.
#' @param peak_window Numeric. Retained for call-signature compatibility; no
#'   longer used now that evaluation anchors on the single global peak rather
#'   than a peak-phase window. Default 20.
#' @param non_transmission_months Integer vector of calendar months (1-12)
#'   treated as the non-transmission season; excluded from peak detection and
#'   aggregates only (rows are kept). Default c(5, 6, 7) (May-July).
#' @param time_step Integer. The data's dominant time step in days (e.g. 7 for
#'   weekly) from `eval_meta$time_step`. Used to convert horizons and date
#'   differences into step units. Default 7.
#' @param timing_tol_steps Numeric. Tolerance, in time steps, within which a
#'   timing miss still reads as "On Target" in `predictedPeakTimingLabel`.
#'   Default 0, so only an exact hit (0 steps off) reads as "On Target" and any
#'   nonzero miss is labelled early/late. Raise it to widen the on-target band
#'   (e.g. 1 = within one step still reads as on target).
#' @param mag_tol Numeric between 0 and 1. Retained for call-signature
#'   compatibility; not used by the per-horizon measures. Default 0.80.
#'
#' @return A data frame with one row per location x season x horizon:
#'   `horizon`, `horizonName`, `observedPeakDate`, `observedPeakValue`,
#'   `peakWeekReachedInTesting`, `peakNote`, `horizonReachesPeak`,
#'   `predictedPeakNote`, `predictedPeakDate`, `predictedPeakValue`,
#'   `predictedPeakTimingOff`, `predictedPeakTimingLabel`,
#'   `predictedPeakMagnitudeOff`, `predictedPeakAccuracy`,
#'   `observedAtPredictedPeak`, `sameDayMagnitudeOff`, `sameDayAccuracy`,
#'   `peakWeekForecastValue`, `peakWeekForecastExists`, `peakWeekMagnitudeOff`,
#'   `peakWeekAccuracy`, `peakWeekNote`; or an empty data frame when no usable
#'   rows are present or a stage fails.
#'
#' @keywords internal
#' @noRd
calculating_peak_trough_PEAKPHASE <- function(data.for.evaluation,
                                              season_start_day_month = "August 01",
                                              peak_window = 20,
                                              non_transmission_months = c(5, 6, 7),
                                              time_step = 7,
                                              timing_tol_steps = 0L,
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
# calculations below. This includes renaming variables and flagging the 'off'  #
# period for a given disease.                                                  #
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
  if(is.null(groupedData)) return(empty_result)

  # Returning the empty data set
  if(nrow(groupedData) == 0) return(groupedData)

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
  observedPeakByGroup <- tryCatch({

    #########################################
    # Updating the data set with season max #
    #########################################
    dataWSeason %>%

      # Collapsing to one observed value per target date
      dplyr::distinct(season, location, target_end_date,
                      targetValue, is_transmission) %>%

      # One peak per season/location
      dplyr::group_by(season, location) %>%

      # For each group, finding the global max and the date it occurs
      dplyr::summarise(

        # Calculating global max
        observedPeakValue = {

          # Pulling the "in season/not-low transmission" indicator
          vals <- targetValue[is_transmission]

          # Finding max for not-low transmission dates
          if(all(is.na(vals))) NA_real_ else max(vals, na.rm = TRUE)

        },

        # Date of the global max (first instance by date when values tie)
        observedPeakDate = {

          # In-transmission, non-missing observed candidates
          keep <- is_transmission & !is.na(targetValue)

          ################################################################
          # NA when nothing usable, else earliest date achieving the max #
          ################################################################
          if(!any(keep)){as.Date(NA)

          # Using earliest date achieving the max
          }else{
            d <- target_end_date[keep]
            v <- targetValue[keep]
            o <- order(d)
            d <- d[o]; v <- v[o]
            d[which(v == max(v))[1]]
          }

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
  if(is.null(observedPeakByGroup)) return(empty_result)

#------------------------------------------------------------------------------#
# Per-horizon predicted peak (Step 1, argmax) ----------------------------------
#------------------------------------------------------------------------------#
# About: This section scores, for each season/location/horizon, the horizon's  #
# own predicted peak: the highest forecast value across that horizon's target  #
# dates (reference dates collapsed within the horizon) and the target date it  #
# falls on. Ties on the forecast max resolve to the earliest such target date. #
# Only in-transmission targets are considered so an off-season blip cannot win.#
#------------------------------------------------------------------------------#

  #####################################################
  # Unit words used in horizon names and timing label #
  #####################################################

  # Pulling the time unit word
  unit_word <- if(time_step == 7) "Week" else if(time_step == 1) "Day" else "Step"

  # Lower case version of time step label
  unit_low  <- tolower(unit_word)

  ###############################################
  # Trying to build the per-horizon metric grid #
  ###############################################
  peakIndicators <- tryCatch({

    ##############################################################
    # Usable forecast rows: median point, in-transmission target #
    ##############################################################
    usableForecasts <- dataWSeason %>%
      dplyr::filter(!is.na(forecastValue), is_transmission) %>%
      dplyr::mutate(horizon = suppressWarnings(as.integer(horizon)))

    ################################################
    # Bail to an empty result if nothing is usable #
    ################################################
    if(nrow(usableForecasts) == 0) return(empty_result)

    ###################################################################
    # Per-horizon target coverage and whether it reaches the peak     #
    # (informational only: M1 is reported for every horizon, but a    #
    # horizon whose span does not contain the peak yields a trajectory#
    # max that may be edge-censored, flagged via horizonReachesPeak)  #
    ###################################################################
    horizonCoverage <- usableForecasts %>%

      # Each horizon's own target-date span
      dplyr::group_by(season, location, horizon) %>%
      dplyr::summarise(horizonStart = min(target_end_date),
                       horizonEnd   = max(target_end_date), .groups = "drop") %>%

      # Does this horizon's coverage contain the observed peak date?
      dplyr::left_join(dplyr::select(observedPeakByGroup, season, location, observedPeakDate),
                       by = c("season", "location")) %>%
      dplyr::mutate(horizonReachesPeak = !is.na(observedPeakDate) &
                      horizonStart <= observedPeakDate &
                      observedPeakDate <= horizonEnd) %>%
      dplyr::select(season, location, horizon, horizonReachesPeak)

    ####################################################
    # Step 1: per-horizon predicted peak (date, value) #
    # argmax over each horizon's full in-season span    #
    ####################################################
    predictedPeakByHorizon <- usableForecasts %>%

      # One trajectory per season/location/horizon
      dplyr::group_by(season, location, horizon) %>%

      # Highest forecast first; earliest target breaks value ties
      dplyr::arrange(dplyr::desc(forecastValue), target_end_date, .by_group = TRUE) %>%

      # Pull the horizon's predicted peak (value and the date it lands on)
      dplyr::summarise(predictedPeakValue = dplyr::first(forecastValue),
                       predictedPeakDate  = dplyr::first(target_end_date),
                       .groups = "drop")

    ##################################################################
    # Step 2: Pulling the horizon's forecast AT the global-peak date #
    ##################################################################
    peakWeekForecastByHorizon <- usableForecasts %>%

      # Attach each group's global peak (value and date)
      dplyr::left_join(observedPeakByGroup, by = c("season", "location")) %>%

      # Keep only forecasts whose target IS the global-peak date
      dplyr::filter(!is.na(observedPeakDate), target_end_date == observedPeakDate) %>%

      # One value per season/location/horizon (a horizon hits G at most once)
      dplyr::group_by(season, location, horizon) %>%

      # Pulling value at global peak
      dplyr::summarise(peakWeekForecastValue = dplyr::first(forecastValue), .groups = "drop")

    ##########################################################
    # Step 2b: observed value ON the predicted-peak date, for #
    # the same-day magnitude comparison (keyed to join on     #
    # predictedPeakDate below)                                #
    ##########################################################
    observedByDate <- dataWSeason %>%
      dplyr::filter(is_transmission, !is.na(targetValue)) %>%
      dplyr::distinct(season, location, target_end_date, targetValue) %>%
      dplyr::transmute(season, location,
                       predictedPeakDate       = target_end_date,
                       observedAtPredictedPeak = targetValue)

    ###################################################
    # Assemble per-horizon rows and derive the metrics#
    ###################################################
    horizonGrid <- usableForecasts %>% dplyr::distinct(season, location, horizon)

    horizonGrid %>%

      # Bring in the per-horizon peak-reach flag
      dplyr::left_join(dplyr::select(horizonCoverage, season, location,
                                     horizon, horizonReachesPeak),
                       by = c("season", "location", "horizon")) %>%

      # Bring in the per-horizon predicted peak (full-span argmax)
      dplyr::left_join(predictedPeakByHorizon, by = c("season", "location", "horizon")) %>%

      # Bring in the value observed on the predicted-peak date
      dplyr::left_join(observedByDate,
                       by = c("season", "location", "predictedPeakDate")) %>%

      # Bring in the global peak (value + date)
      dplyr::left_join(observedPeakByGroup, by = c("season", "location")) %>%

      # Bring in the value-at-peak-week measure where it exists
      dplyr::left_join(peakWeekForecastByHorizon, by = c("season", "location", "horizon")) %>%

      ######################################################################
      # Season-level coverage: does ANY horizon reach the global-peak date #
      ######################################################################
      dplyr::group_by(season, location) %>%

      # Pulling horizon dates that overlap peak
      dplyr::mutate(peakWeekReachedInTesting = any(!is.na(peakWeekForecastValue))) %>%

      # Un-grouping rows
      dplyr::ungroup() %>%

      ###############################
      # Calculating summary metrics #
      ###############################
      dplyr::mutate(

        # Treat a missing reach flag as FALSE (no usable peak for the group)
        horizonReachesPeak = !is.na(horizonReachesPeak) & horizonReachesPeak,

        ##########################################################
        # Step 1 timing: Predicted-Peak Date vs Global-Peak Date #
        ##########################################################
        predictedPeakTimingOff = ifelse(
          is.na(predictedPeakDate) | is.na(observedPeakDate),
          NA_real_,
          as.numeric(predictedPeakDate - observedPeakDate) / time_step),

        ###################################################################
        # M1 magnitude: predicted-peak height minus global-peak value     #
        ###################################################################
        predictedPeakMagnitudeOff = predictedPeakValue - observedPeakValue,

        ##########################################
        # Step 3: Percent Accuracy for Intensity #
        ##########################################
        predictedPeakAccuracy = ifelse(
          pmax(predictedPeakValue, observedPeakValue) == 0, NA_real_,
          pmin(predictedPeakValue, observedPeakValue) / pmax(predictedPeakValue, observedPeakValue)),

        ##############################################################
        # Step 4: Coverage flag (a forecast landed on the peak date) #
        ##############################################################
        peakWeekForecastExists = !is.na(peakWeekForecastValue),

        ###############################################################
        # Step 5: Magnitude Value AT the Peak Date - True Peak Height #
        ###############################################################
        peakWeekMagnitudeOff = peakWeekForecastValue - observedPeakValue,

        #######################################
        # Step 6: Percent Accuracy for Step 5 #
        #######################################
        peakWeekAccuracy = ifelse(
          is.na(peakWeekForecastValue) | pmax(peakWeekForecastValue, observedPeakValue) == 0, NA_real_,
          pmin(peakWeekForecastValue, observedPeakValue) / pmax(peakWeekForecastValue, observedPeakValue)),

        ###################################################################
        # Step 7: Same-day magnitude - predicted-peak height minus the    #
        # value actually observed on the predicted-peak date (overshoot   #
        # or undershoot at the forecast's own timing)                     #
        ###################################################################
        sameDayMagnitudeOff = predictedPeakValue - observedAtPredictedPeak,

        #######################################
        # Step 8: Percent Accuracy for Step 7 #
        #######################################
        sameDayAccuracy = ifelse(
          is.na(observedAtPredictedPeak) | pmax(predictedPeakValue, observedAtPredictedPeak) == 0, NA_real_,
          pmin(predictedPeakValue, observedAtPredictedPeak) / pmax(predictedPeakValue, observedAtPredictedPeak)),

        #########################
        # Readable Horizon Name #
        #########################
        horizonName = dplyr::case_when(
          horizon <  0 ~ paste0(unit_word, " ", horizon, " (Nowcast)"),
          horizon == 0 ~ paste0(unit_word, " 0 (Nowcast)"),
          horizon == 1 ~ paste0(horizon, " ", unit_word, " Ahead"),
          TRUE         ~ paste0(horizon, " ", unit_word, "s Ahead")),

        #########################
        # Readable timing label #
        #########################
        predictedPeakTimingLabel = dplyr::case_when(
          is.na(predictedPeakTimingOff)               ~ NA_character_,
          predictedPeakTimingOff < -timing_tol_steps  ~ paste0(abs(predictedPeakTimingOff), " ",
              ifelse(abs(predictedPeakTimingOff) == 1, unit_low, paste0(unit_low, "s")), " early"),
          predictedPeakTimingOff >  timing_tol_steps  ~ paste0(predictedPeakTimingOff, " ",
              ifelse(predictedPeakTimingOff == 1, unit_low, paste0(unit_low, "s")), " late"),
          TRUE                           ~ "On Target"),

        #############################################################
        # Season-level note when the global peak is outside testing #
        #############################################################
        peakNote = ifelse(
          peakWeekReachedInTesting, NA_character_,
          "Observed global peak does not fall within the testing period; timing and magnitude are censored."),

        #######################################################################
        # Per-horizon note: M1 is reported for every horizon, but flag the    #
        # ones whose span does not contain the peak (trajectory max may be    #
        # edge-censored rather than a true peak prediction)                   #
        #######################################################################
        predictedPeakNote = dplyr::case_when(
          is.na(observedPeakDate) ~
            "No observed peak identified for this season/location.",
          !horizonReachesPeak ~
            "Horizon span does not contain the observed peak; predicted peak is its trajectory max and may be edge-censored.",
          TRUE ~ NA_character_),

        #######################################################################
        # Per-horizon note when the peak date is not forecast at this horizon #
        #######################################################################
        peakWeekNote = ifelse(
          peakWeekForecastExists, NA_character_,
          "No testing forecast targets the global peak date at this horizon.")

      ) %>%

      ############################
      # Final per-horizon schema #
      ############################
      dplyr::transmute(
        location, season, horizon, horizonName,
        observedPeakDate,
        observedPeakValue,
        peakWeekReachedInTesting, peakNote,
        horizonReachesPeak, predictedPeakNote,
        predictedPeakDate, predictedPeakValue,
        predictedPeakTimingOff, predictedPeakTimingLabel,
        predictedPeakMagnitudeOff, predictedPeakAccuracy,
        observedAtPredictedPeak, sameDayMagnitudeOff, sameDayAccuracy,
        peakWeekForecastValue, peakWeekForecastExists, peakWeekMagnitudeOff, peakWeekAccuracy, peakWeekNote) %>%

      # Stable ordering for the report
      dplyr::arrange(season, location, horizon)

  ####################################################
  # Triggered if the per-horizon metric stage fails  #
  ####################################################
  }, error = function(e){

    # Message to show to users
    message(
      "calculating_peak_trough_PEAKPHASE(): failed while computing the ",
      "per-horizon peak metrics, so an empty result is returned.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - time_step supplied: ", time_step, "\n",
      "  - n_horizons: ", n_horizons
    )

    # Returning NULL so the bail below fires
    NULL

  })

  ###############################################
  # Bail to an empty result if the stage failed #
  ###############################################
  if(is.null(peakIndicators)) return(empty_result)

  ################################
  # Returning the metric results #
  ################################
  return(peakIndicators)

}
