#' Create an evaluation configuration for forecast evaluation reports
#'
#' Builds a named list of the tuning settings used by the testing-period
#' evaluation metrics. All arguments have sensible defaults that match the
#' standard report behavior. Users who want to customize the evaluation create
#' their own config object by calling this function with overrides, then source
#' it in their options file and pass it to [generate_report()]. The settings
#' flow through to `build_testing_evaluation()` and on to the individual metric
#' helpers (percent agreement, forecast bias, peak phase).
#'
#' @param non_transmission_months Integer vector of calendar months (1-12)
#'   treated as the off-season. These rows are kept for plotting but excluded
#'   from every aggregate metric. Default `c(5, 6, 7)` (May-July).
#' @param season_start_day_month Character "Month DD" (e.g. `"August 01"`)
#'   marking the season start. Only the month is used, to assign each row to a
#'   season for the peak-phase metrics. Default `"August 01"`.
#' @param peak_window Numeric. Tolerance around the observed seasonal max, as a
#'   PERCENT of that max, used to define the contiguous peak phase. Default
#'   `20`.
#' @param stable_threshold Numeric. Observed-count floor at/above which a row is
#'   counted as "stable" for the forecast-bias percentage-error summaries.
#'   Default `10`.
#' @param pct_error_cushion Numeric. Percentage-error band (in points) around
#'   zero within which a forecast-bias row counts as "Within Range"; beyond
#'   +/- this it is "Overestimate" / "Underestimate". Default `20`.
#' @param timing_tol_steps Numeric. Tolerance, in time steps, within which a
#'   peak-phase timing miss still counts as "On Time". Default `1`.
#' @param mag_tol Numeric between 0 and 1. Minimum rank-matched agreement
#'   at/above which a peak-phase magnitude counts as "On Target". Default
#'   `0.80`.
#'
#' @return A named list of evaluation settings for use by
#'   `build_testing_evaluation()`.
#'
#' @export
create_evaluation_config <- function(

#------------------------------------------------------------------------------#
# Selecting the non-transmission months ----------------------------------------
#------------------------------------------------------------------------------#
# About: This section is where users can specify the "non transmission"        #
# months. These are the months where transmission for the given disease is     #
# low, and should not be included in the summary calculations.                 #
#------------------------------------------------------------------------------#
  non_transmission_months = c(5, 6, 7),

#------------------------------------------------------------------------------#
# Peak-Phase Settings ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets the season start date and the window around the     #
# global peak to calculate the peak accuracy. Thus, the peak window is the     #
# size of the window to possible include in the peak phase. The default        #
# setting is August 1 and 20 weeks.                                            #
#------------------------------------------------------------------------------#

  #################################
  # Setting the season start date #
  #################################
  season_start_day_month = "August 01",

  ################################
  # Setting the peak window size #
  ################################
  peak_window            = 20,

#------------------------------------------------------------------------------#
# Setting the Forecast Bias limits ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets the forecast bias settings, including what is not   #
# included as part of the % change calculations and what is considered a       #
# stable trend. For now, this is set to 10 (i.e., counts less then 10 are not  #
# included in % change calculations) and 20 (i.e., a change of <= 20 is        #
# considered stable).                                                          #
#------------------------------------------------------------------------------#

  ####################
  # Stable threshold #
  ####################
  stable_threshold  = 10,

  #######################################
  # Setting the stable change threshold #
  #######################################
  pct_error_cushion = 20,

#------------------------------------------------------------------------------#
# Peak-phase directional-label tolerances --------------------------------------
#------------------------------------------------------------------------------#
# About: This section sets the settings around assigning direction labels to   #
# the peak phase metrics. This included the cushion around setting the         #
# perfect timing label, and the percent agreement limit that is considered a   #
# on target label.                                                             #
#------------------------------------------------------------------------------#

  ######################################
  # Setting cushion around peak timing #
  ######################################
  timing_tol_steps = 0L,

  #########################################
  # Setting cushion around peak magnitude #
  #########################################
  mag_tol          = 0.80

) {




#------------------------------------------------------------------------------#
# No User Input Under this Section ---------------------------------------------
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Input validation -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Confirms each field is present and correctly typed before assembling  #
# the config list. Errors accumulate so the user sees every problem at once,   #
# each specific about which field is wrong and what is expected. Catching bad  #
# values here means the metric helpers fail fast with a clear message rather   #
# than deep inside a calculation.                                              #
#------------------------------------------------------------------------------#

  #################################
  # Preparing the error functions #
  #################################

  # Empty vector to store errors
  errors <- character()

  # Function to add errors to the error vector
  add_error <- function(msg) errors <<- c(errors, msg)

#------------------------------------------------------------------------------#
# Checking each user input -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks each user input provided above against what is    #
# expected. This is to facilitate later scripts that use this information by   #
# checking for any errors that could occur early on.                           #
#------------------------------------------------------------------------------#

  ########################################################
  # Checking non-transmission months: Number is Provided #
  ########################################################
  if(!is.numeric(non_transmission_months)){

    # Error to return to users
    add_error("`non_transmission_months` must be a numeric vector of months (0-12).")

  ####################################################
  # Checking non-transmission months: Number is 0-12 #
  ####################################################
  }else if(length(non_transmission_months) > 0 &&
           (any(non_transmission_months < 0)||
            any(non_transmission_months > 12) ||
            any(non_transmission_months != floor(non_transmission_months)))){

    # Error to return to users
    add_error(paste0(
      "`non_transmission_months` must contain whole numbers between 1 and 12; got: ",
      paste(non_transmission_months, collapse = ", "), "."
    ))

  }

  #######################################################################
  # Checking the format of the season start date: Checking if Character #
  #######################################################################
  if(!is.character(season_start_day_month) ||
     length(season_start_day_month) != 1 ||
     is.na(season_start_day_month) ||
     !nzchar(season_start_day_month)){

    # Error to return to users
    add_error("`season_start_day_month` must be a single non-empty string like \"August 01\".")

  ##########################################################################
  # Checking the format of the season start date: Valid Month/Day Provided #
  ##########################################################################
  }else{

    # Extracting the month
    first_token <- sub("[[:space:]].*$", "", trimws(season_start_day_month))

    # Checking month is correct format
    if(!tolower(first_token) %in% tolower(c(month.name, month.abb))){

      # Error to return to users
      add_error(paste0(
        "`season_start_day_month` must start with a month name, e.g. \"August 01\"; got: \"",
        season_start_day_month, "\"."
      ))

    }
  }

  ##############################################
  # Checking the peak_window: Positive Percent #
  ##############################################
  if(!is.numeric(peak_window) || length(peak_window) != 1 ||
     is.na(peak_window) || peak_window <= 0){

    # Error to return to user
    add_error("`peak_window` must be a single positive number (percent of the seasonal max).")

  }

  ######################################################
  # Checking the stable_threshold: Non-Negative Number #
  ######################################################
  if(!is.numeric(stable_threshold) || length(stable_threshold) != 1 ||
     is.na(stable_threshold) || stable_threshold < 0){

    # Error to return to user
    add_error("`stable_threshold` must be a single non-negative number.")

  }

  #######################################################
  # Checking the pct_error_cushion: Non-Negative Number #
  #######################################################
  if(!is.numeric(pct_error_cushion) || length(pct_error_cushion) != 1 ||
     is.na(pct_error_cushion) || pct_error_cushion < 0){

    # Error to return to user
    add_error("`pct_error_cushion` must be a single non-negative number (percentage points).")

  }

  ############################################################
  # Checking the timing_tol_steps: Non-Negative Whole Number #
  ############################################################
  if(!is.numeric(timing_tol_steps) || length(timing_tol_steps) != 1 ||
     is.na(timing_tol_steps) || timing_tol_steps < 0 ||
     timing_tol_steps != floor(timing_tol_steps)){

    # Error to return to user
    add_error("`timing_tol_steps` must be a single non-negative whole number of time steps.")

  }

  #######################################
  # Checking the mag_tol: Number in 0-1 #
  #######################################
  if(!is.numeric(mag_tol) || length(mag_tol) != 1 ||
     is.na(mag_tol) || mag_tol < 0 || mag_tol > 1){

    # Error to return to user
    add_error("`mag_tol` must be a single number between 0 and 1.")

  }

  #######################
  # Abort if any errors #
  #######################
  if(length(errors) > 0){

    # Stopping script and returning the errors
    stop(
      "create_evaluation_config failed with ", length(errors), " error(s):\n  - ",
      paste(errors, collapse = "\n  - "),
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Assembling the EVAL_CONFIG list ----------------------------------------------
#------------------------------------------------------------------------------#
# About: The returned list keys each setting by the argument name              #
# build_testing_evaluation() expects, so the report can spread these straight  #
# into that call. Whole-number fields are coerced to integer for tidiness.     #
#------------------------------------------------------------------------------#

  list(
    non_transmission_months = as.integer(non_transmission_months),
    season_start_day_month  = season_start_day_month,
    peak_window             = peak_window,
    stable_threshold        = stable_threshold,
    pct_error_cushion       = pct_error_cushion,
    timing_tol_steps        = as.integer(timing_tol_steps),
    mag_tol                 = mag_tol
  )

}
