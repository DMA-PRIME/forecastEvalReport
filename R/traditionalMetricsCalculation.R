#' Calculate traditional forecast evaluation metrics (WIS, MAE, coverage)
#'
#' Computes scoringutils-based evaluation metrics for the single evaluation
#' model: the weighted interval score (WIS), the absolute error of the median
#' (MAE = ae_median), and 50% and 95% interval coverage. Metrics are scored per
#' forecast from the full quantile distribution, summarised by horizon x
#' location and overall per location, then broadcast back onto every row.
#' Aggregates use transmission-season rows only. Operates on the testing
#' evaluation dataset from `prepare_testing_evaluation_data()` (all quantile
#' rows, i.e. before the median filter the point-based functions apply).
#'
#' @param data.for.evaluation Testing evaluation data frame from
#'   `prepare_testing_evaluation_data()`, with one row per quantile level. Must
#'   contain `location`, `reference_date`, `target_end_date`, `horizon`,
#'   `output_type_id` (quantile level in 0-1), `value` (predicted), and
#'   `Observed`.
#' @param non_transmission_months Integer vector of calendar months (1-12)
#'   treated as the non-transmission season; excluded from the aggregate scores
#'   only (rows are kept). Default c(5, 6, 7) (May-July).
#' @param season_month Integer month (1-12) on which a transmission season is
#'   considered to start, used to label each row's `season` from its
#'   `reference_date` (e.g. `"2024-2025"`). Default 8 (August), matched to the
#'   rest of the package so seasons align across sections.
#'
#' @return The input data frame with a `season` label and horizon-level,
#'   per-season, and overall summary columns (mean WIS, MAE, 50% and 95%
#'   coverage) broadcast across each group, or the input with those columns as
#'   NA when no usable rows are present or a stage fails. A single metric is
#'   also NA for a group that lacks the data to compute it: WIS without
#'   intervals beyond the median, or coverage without the relevant interval
#'   bounds.
#'
#' @keywords internal
#' @noRd
traditionalMetricsCalculation <- function(data.for.evaluation,
                                          non_transmission_months = c(6, 7),
                                          season_month            = 8) {

#------------------------------------------------------------------------------#
# Confirming function should be run --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms the evaluation data exists, is a data frame,    #
# and has at least one row. If not, there is nothing to score, so the input is #
# returned unchanged and the rest of the report keeps running.                 #
#------------------------------------------------------------------------------#

  #######################################################
  # Triggered if there is an issue with evaluation data #
  #######################################################
  if(is.null(data.for.evaluation) ||
     !is.data.frame(data.for.evaluation) ||
     nrow(data.for.evaluation) == 0){

    # Returning the (empty / unusable) input unchanged
    return(data.for.evaluation)

  }

#------------------------------------------------------------------------------#
# Expected columns + Graceful NA Fallback --------------------------------------
#------------------------------------------------------------------------------#
# About: This section lists the summary columns this function adds and         #
# provides a fallback that fills any missing ones with NA. If a stage fails,   #
# the frame is still returned with the expected columns present so downstream  #
# tabulation does not choke on a missing column; the message says why they are #
# NA.                                                                          #
#------------------------------------------------------------------------------#

  ###############################
  # Aggregate (summary) columns #
  ###############################
  num_summary_cols <- c(
    "WIS_Horizon", "MAE_Horizon", "Cov50_Horizon", "Cov95_Horizon",
    "WIS_Season",  "MAE_Season",  "Cov50_Season",  "Cov95_Season",
    "WIS_Overall", "MAE_Overall", "Cov50_Overall", "Cov95_Overall"
  )

  ###################################################
  # Fills any missing expected column with NA_real_ #
  ###################################################
  add_missing_as_na <- function(df){

    # Numeric summary columns -> NA_real_
    for(col in num_summary_cols){
      if(!col %in% names(df)) df[[col]] <- NA_real_
    }

    # Returning the back-filled frame
    df

  }

#------------------------------------------------------------------------------#
# Checking for required columns ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms the columns that scoringutils needs are         #
# present. If any are missing,  the metric columns are returned as NA          #
# (via the fallback) and a message lists what is missing, so the report keeps  #
# running.                                                                     #
#------------------------------------------------------------------------------#

  ###############################
  # Columns needed for scoring  #
  ###############################
  needed <- c("location", "reference_date", "target_end_date",
              "horizon", "output_type_id", "value", "Observed")

  ############################
  # Flagging missing columns #
  ############################
  missing_cols <- setdiff(needed, names(data.for.evaluation))

  #######################################
  # Triggered if any columns are missing#
  #######################################
  if(length(missing_cols) > 0){

    # Message to show to users
    message(
      "traditionalMetricsCalculation(): input is missing required column(s), ",
      "so the metric columns are returned as NA.\n",
      "  - Missing: ", paste(missing_cols, collapse = ", "), "\n",
      "  - Columns present: ",
      paste(names(data.for.evaluation), collapse = ", "), "\n",
      "  - Expected: ", paste(needed, collapse = ", ")
    )

    # Returning the NA-filled frame
    return(add_missing_as_na(data.for.evaluation))
  }

#------------------------------------------------------------------------------#
# Labelling each row's season --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section tags every row with the transmission season it belongs   #
# to, derived from its reference_date around the season_month cut-off. The     #
# label (e.g. "2024-2025") matches the rest of the package so the per-season   #
# summaries here line up with the other sections.                              #
#------------------------------------------------------------------------------#

  ######################################
  # Season label from a reference date #
  ######################################
  assign_season <- function(ref){

    # Month and year of the reference date
    ref_month <- as.numeric(format(as.Date(ref), "%m"))
    ref_year  <- as.numeric(format(as.Date(ref), "%Y"))

    # Before the cut-off belongs to the season that started the prior year
    ifelse(ref_month < season_month,
           paste0(ref_year - 1, "-", ref_year),
           paste0(ref_year, "-", ref_year + 1))

  }

  # Tagging every row with its season
  data.for.evaluation$season <-
    assign_season(data.for.evaluation$reference_date)

#------------------------------------------------------------------------------#
# scoringutils availability ----------------------------------------------------
#------------------------------------------------------------------------------#

  ###########################################
  # scoringutils must be available to score #
  ###########################################
  if(!requireNamespace("scoringutils", quietly = TRUE)){

    # Message to show to users
    message(
      "traditionalMetricsCalculation(): the 'scoringutils' package is not ",
      "installed, so the metric columns are returned as NA.\n",
      "  - Install it with install.packages('scoringutils')."
    )

    # Returning the NA-filled frame
    return(add_missing_as_na(data.for.evaluation))

  }

#------------------------------------------------------------------------------#
# Deciding whether WIS is meaningful -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section decides, per location, whether WIS carries probabilistic #
# information. WIS only means something when quantiles beyond the median are   #
# present; with the 0.5 quantile alone it reduces exactly to the absolute      #
# error of the median, so WIS is returned as NA (a dash) for those locations   #
# and only MAE (and coverage, where available) stands.                         #
#------------------------------------------------------------------------------#

  ########################################
  # Per-location presence of an interval #
  ########################################
  loc_has_intervals <- tryCatch({

    # One row per location, flagged when any non-median quantile is present
    data.for.evaluation %>%
      dplyr::filter(
        !lubridate::month(target_end_date) %in% non_transmission_months,
        !is.na(value), !is.na(output_type_id)) %>%
      dplyr::group_by(location) %>%
      dplyr::summarise(
        wis_ok = any(abs(output_type_id - 0.5) > 1e-6), .groups = "drop")

  ##################################################
  # If we can't tell, keep WIS rather than hide it #
  ##################################################
  }, error = function(e){

    # Fall back to keeping WIS for every location present
    data.frame(location = unique(data.for.evaluation$location), wis_ok = TRUE)

  })

  ###############################################
  # Per-location/season presence of an interval #
  ###############################################
  loc_season_intervals <- tryCatch({

    # One row per location/season, flagged when a non-median quantile is present
    data.for.evaluation %>%
      dplyr::filter(
        !lubridate::month(target_end_date) %in% non_transmission_months,
        !is.na(value), !is.na(output_type_id)) %>%
      dplyr::group_by(location, season) %>%
      dplyr::summarise(
        wis_ok = any(abs(output_type_id - 0.5) > 1e-6), .groups = "drop")

  ##################################################
  # If we can't tell, keep WIS rather than hide it #
  ##################################################
  }, error = function(e){

    # Fall back to keeping WIS for every location/season present
    us <- unique(data.for.evaluation[, c("location", "season")])
    us$wis_ok <- TRUE
    us

  })

  ###################################################
  # Letting users know when WIS is being suppressed #
  ###################################################
  if(any(!loc_has_intervals$wis_ok)){

    # Locations that only carry the median quantile
    med_only <- loc_has_intervals$location[!loc_has_intervals$wis_ok]

    # Message to show to users
    message(
      "traditionalMetricsCalculation(): only the median (0.5) quantile is ",
      "present for some location(s), so WIS would equal the MAE; WIS is ",
      "returned as NA (a dash) there and only MAE is reported.\n",
      "  - Location(s): ", paste(med_only, collapse = ", ")
    )

  }

#------------------------------------------------------------------------------#
# Scoring each forecast (WIS and MAE) ------------------------------------------
#------------------------------------------------------------------------------#
# About: Builds a scoringutils quantile forecast object from the               #
# transmission-season rows (one forecast = one location x reference_date x     #
# horizon, across its quantile levels) and scores WIS and the absolute error   #
# of the median (MAE). Only those two come from scoringutils -- the 50% and    #
# 95% interval coverage are computed by hand in the summary stage, since the   #
# scoringutils coverage-metric names vary by version and get silently dropped  #
# on a mismatch. WIS is built from whatever symmetric quantile pairs are       #
# present, so ragged quantile sets (not all 23 levels) do not break scoring.   #
#------------------------------------------------------------------------------#

  ##################################
  # Trying to score every forecast #
  ##################################
  scores <- tryCatch({

    ################################################
    # Preparing the data for scoringutils function #
    ################################################
    su_input <- data.for.evaluation %>%

      # Filtering the data
      dplyr::filter(
        !lubridate::month(target_end_date) %in% non_transmission_months,
        !is.na(value), !is.na(Observed), !is.na(output_type_id)) %>%

      # Getting data in shape for the function
      dplyr::transmute(
        location, reference_date, target_end_date, horizon,
        observed       = Observed,
        predicted      = value,
        quantile_level = output_type_id
      )

    # Nothing left to score after filtering
    if(nrow(su_input) == 0){

      # Error to show to users
      stop("no transmission-season rows with non-missing predicted/observed ",
           "values remain to score.")

    }

    #########################################################
    # Validating / formatting as a quantile forecast object #
    #########################################################
    forecast_obj <- scoringutils::as_forecast_quantile(

      # Data input
      su_input,

      # Grouping
      forecast_unit = c("location", "reference_date",
                        "target_end_date", "horizon")

    )

    ################################################################
    # Calculating the standard metrics available with scoringutils #
    ################################################################

    # Pulling all available metrics
    metrics_list <- scoringutils::get_metrics(forecast_obj)

    # Pulling out the WIS and MAE metrics
    metrics_list <- metrics_list[names(metrics_list) %in% c("wis", "ae_median")]

    # Scoring every forecast
    scoringutils::score(forecast_obj, metrics = metrics_list)

  ##############################################################
  # Triggered if an error occurs while scoring the forecasts   #
  ##############################################################
  }, error = function(e){

    # Diagnostic quantile-level count (guarded so the handler can't error)
    n_levels <- tryCatch(
      length(unique(data.for.evaluation$output_type_id)),
      error = function(e2) NA)

    # Message to show to users
    message(
      "traditionalMetricsCalculation(): failed while scoring forecasts with ",
      "scoringutils, so the metric columns are returned as NA.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - distinct quantile levels (output_type_id): ", n_levels, "\n",
      "  - target_end_date class: ",
      paste(class(data.for.evaluation$target_end_date), collapse = "/"),
      " (must be Date for the season filter)\n",
      "  - Tip: scoringutils needs numeric quantile levels in 0-1 and numeric ",
      "predicted/observed values."
    )

    # Returning NULL so the bail below can fire
    NULL

  })

  ##########################################
  # Bail to NA columns if scoring failed   #
  ##########################################
  if(is.null(scores)) return(add_missing_as_na(data.for.evaluation))

#------------------------------------------------------------------------------#
# Creating the summary metrics -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section averages the per forecast scores into mean WIS, MAE, and #
# 50/95% coverage rates per horizon and location, per season and location, and #
# per location. All NAs are removed, so missing coverage is ignored. The last  #
# step broadcasts every level back onto each row; a failure here keeps the row #
# data and returns the summary columns as NA.                                  #
#------------------------------------------------------------------------------#

  ###########################################
  # Trying to summarise and join the scores #
  ###########################################
  forecast.metrics <- tryCatch({

    ################################################
    # Mean helper: NA when nothing is assessable   #
    ################################################
    avg_or_na <- function(x){
      if(all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    }

    #############################################################
    # WIS + MAE: summarise the scoringutils per-forecast scores #
    #############################################################

    # Horizon + Location summary (WIS dashed where a location has no interval)
    wis_mae_horizon <- scoringutils::summarise_scores(
      scores, by = c("horizon", "location"), na.rm = TRUE) %>%
      as.data.frame() %>%
      dplyr::left_join(loc_has_intervals, by = "location") %>%
      dplyr::transmute(horizon, location,
                       WIS_Horizon = dplyr::if_else(wis_ok, wis, NA_real_),
                       MAE_Horizon = ae_median)

    # Location only summary (WIS dashed where a location has no interval)
    wis_mae_overall <- scoringutils::summarise_scores(
      scores, by = "location", na.rm = TRUE) %>%
      as.data.frame() %>%
      dplyr::left_join(loc_has_intervals, by = "location") %>%
      dplyr::transmute(location,
                       WIS_Overall = dplyr::if_else(wis_ok, wis, NA_real_),
                       MAE_Overall = ae_median)

    # Per-forecast scores as a frame, tagged with season for the season summary
    scores_df        <- as.data.frame(scores)
    scores_df$season <- assign_season(scores_df$reference_date)

    # Season + Location summary (WIS dashed where that season has no interval)
    wis_mae_season <- scores_df %>%
      dplyr::group_by(location, season) %>%
      dplyr::summarise(WIS_Season = avg_or_na(wis),
                       MAE_Season = avg_or_na(ae_median), .groups = "drop") %>%
      dplyr::left_join(loc_season_intervals, by = c("location", "season")) %>%
      dplyr::mutate(
        WIS_Season = dplyr::if_else(wis_ok, WIS_Season, NA_real_)) %>%
      dplyr::select(-wis_ok)

    #####################################################################
    # Coverage: one row per forecast with the 50% / 95% interval bounds #
    #####################################################################
    per_forecast_cov <- data.for.evaluation %>%

      # Removing non-transmission month rows
      dplyr::filter(
        !lubridate::month(target_end_date) %in% non_transmission_months,
        !is.na(Observed), !is.na(value), !is.na(output_type_id)) %>%

      # Group by forecast
      dplyr::group_by(location, season, reference_date, target_end_date,
                      horizon) %>%

      # Creating the summary table of PIs
      dplyr::summarise(

        # Pulling the observed rows
        Observed = dplyr::first(Observed),

        # Pulling the .25 quantile: 50% PI
        lo50 = value[which(abs(output_type_id - 0.25)  < 1e-6)][1],

        # Pulling the .75 quantile: 50% PI
        hi50 = value[which(abs(output_type_id - 0.75)  < 1e-6)][1],

        # Pulling the .025 quantile: 95% PI
        lo95 = value[which(abs(output_type_id - 0.025) < 1e-6)][1],

        # Pulling the 0.975 quantile: 95% PI
        hi95 = value[which(abs(output_type_id - 0.975) < 1e-6)][1],

        # Dropping the repeat rows
        .groups = "drop") %>%

      ############################
      # Calculating the coverage #
      ############################
      dplyr::mutate(

        # In 50% PI check
        in50 = dplyr::if_else(is.na(lo50) | is.na(hi50) | is.na(Observed),
                              NA, Observed >= lo50 & Observed <= hi50),

        # In 95% PI check
        in95 = dplyr::if_else(is.na(lo95) | is.na(hi95) | is.na(Observed),
                              NA, Observed >= lo95 & Observed <= hi95)

      )

    #########################################
    # Coverage rates per horizon / location #
    #########################################
    cov_horizon <- per_forecast_cov %>%

      # Grouping
      dplyr::group_by(horizon, location) %>%

      # Summary across group
      dplyr::summarise(Cov50_Horizon = avg_or_na(in50),
                       Cov95_Horizon = avg_or_na(in95), .groups = "drop")

    ###############################
    # Coverage rates per season   #
    ###############################
    cov_season <- per_forecast_cov %>%

      # Grouping
      dplyr::group_by(location, season) %>%

      # Summary across group
      dplyr::summarise(Cov50_Season = avg_or_na(in50),
                       Cov95_Season = avg_or_na(in95), .groups = "drop")

    ###############################
    # Coverage rates per location #
    ###############################
    cov_overall <- per_forecast_cov %>%

      # Grouping
      dplyr::group_by(location) %>%

      # Summary across group
      dplyr::summarise(Cov50_Overall = avg_or_na(in50),
                       Cov95_Overall = avg_or_na(in95), .groups = "drop")

    # Broadcasting all summary blocks back onto every row
    data.for.evaluation %>%
      dplyr::left_join(wis_mae_horizon, by = c("horizon", "location")) %>%
      dplyr::left_join(cov_horizon,     by = c("horizon", "location")) %>%
      dplyr::left_join(wis_mae_season,  by = c("location", "season")) %>%
      dplyr::left_join(cov_season,      by = c("location", "season")) %>%
      dplyr::left_join(wis_mae_overall, by = "location") %>%
      dplyr::left_join(cov_overall,     by = "location")

  ######################################
  # Triggered if an error occurs above #
  ######################################
  }, error = function(e){

    # Diagnostic group count (guarded so the handler can't itself error)
    n_h_groups <- tryCatch(
      dplyr::n_groups(dplyr::group_by(data.for.evaluation, horizon, location)),
      error = function(e2) NA)

    # Message to show to users
    message(
      "traditionalMetricsCalculation(): scoring succeeded but summarising / ",
      "joining the scores failed; the metric columns are returned as NA.\n",
      "  - Reason: ", conditionMessage(e), "\n",
      "  - horizon x location groups: ", n_h_groups, "\n",
      "  - Tip: confirm 'horizon' and 'location' are present and not all-NA."
    )

    # Keeping the row data, filling summary columns as NA
    add_missing_as_na(data.for.evaluation)

  })

  ################################
  # Returning the metric results #
  ################################
  return(forecast.metrics)

}
