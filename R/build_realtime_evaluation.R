#' Assemble the real-time evaluation bundle
#'
#' Real-time (operational) twin of `build_testing_evaluation()`. Prepares the
#' evaluation frame from the archived operational forecasts joined to observed
#' truth, then runs the percent-agreement, forecast-bias, and traditional-metric
#' helpers on it. Peak timing/magnitude is intentionally omitted -- peak phase
#' is a testing-period concept and is not meaningful for rolling real-time
#' forecasts.
#'
#' A training series provided in the options file can stand in for the outcome
#' truth when scoring real-time forecasts. The four cases:
#' \enumerate{
#'   \item Training provided and it covers any real-time target (has an observed
#'     value there): use the training series in place of the outcome truth and
#'     score against it.
#'   \item Training provided, different from the outcome (by file path / data
#'     label), and it covers no real-time target: do not do any real-time
#'     evaluation -- the section is skipped (`has_data = FALSE`).
#'   \item Training identical to the outcome (same file path / data label):
#'     proceed with the outcome truth as normal.
#'   \item No training provided: use the outcome truth and apply the coverage
#'     check as before.
#' }
#' "Covers" means the chosen truth series has an observed value at a forecast's
#' (location, `target_end_date`); targets with no observed value carry
#' `Observed = NA` and drop out downstream. When nothing is covered the metric
#' elements are returned as `NULL`, each `section_realtime_*()` trips its own
#' presence guard, and the whole real-time block drops out together.
#'
#' By default no transmission-season filter is applied
#' (`non_transmission_months = numeric(0)`), so every realized week contributes;
#' set it to the testing default `c(5, 6, 7)` to restrict to the transmission
#' season.
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`. Supplies
#'   the `Forecasts/` archive path, location crosswalk, and time step.
#' @param master_data Assembled master data frame from `assemble_report_data()`.
#' @param variables_crosswalk Optional crosswalk from
#'   `validate_variables_crosswalk()`. When supplied, the training-vs-outcome
#'   "same/different" check compares source `file`, `data_source`, and
#'   `variable`; without it the check falls back to `master_data`'s
#'   `data_source` and `variable` labels (no file path).
#' @param training_type,outcome_type `variable_type` tags used to pull the
#'   training and outcome truth series out of `master_data`. Defaults
#'   `"training_data"` and `"outcome_data"`.
#' @param locations Optional named vector mapping raw codes to display names.
#'   Falls back to `impl_meta$locations`.
#' @param time_step Optional integer days per step. Falls back to
#'   `impl_meta$time_step`, then 7.
#' @param non_transmission_months Integer months to exclude from scoring.
#'   Default `numeric(0)` (score every realized week).
#' @param stable_threshold,pct_error_cushion Forecast-bias tuning parameters,
#'   passed through to `forecastBiasCalculation()`.
#'
#' @return A named list with `data` (the scored evaluation frame),
#'   `percentAgreement`, `forecastBias`, `traditional`, and `has_data`. When
#'   there is nothing to score the three metric elements are `NULL` and
#'   `has_data` is `FALSE`; otherwise they hold the computed metric frames and
#'   `has_data` is `TRUE`.
#'
#' @keywords internal
#' @noRd
build_realtime_evaluation <- function(impl_meta,
                                      master_data,
                                      variables_crosswalk     = NULL,
                                      training_type           = "training_data",
                                      outcome_type            = "outcome_data",
                                      locations               = NULL,
                                      time_step               = NULL,
                                      non_transmission_months = numeric(0),
                                      stable_threshold        = 10,
                                      pct_error_cushion       = 20) {

#------------------------------------------------------------------------------#
# Resolving inputs from impl_meta ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds in checks for the time step and location. It      #
# specifically looks for NULL time steps and NULL locations. If either is      #
# TRUE, then the default is set to 7 and NULL, respectively.                   #
#------------------------------------------------------------------------------#

  ##########################
  # Checking the time step #
  ##########################
  if(is.null(time_step)){

    # Checking the time step, defaulting to 7
    time_step <- if(!is.null(impl_meta$time_step)) impl_meta$time_step else 7

  }

  ##########################
  # Checking the locations #
  ##########################
  if(is.null(locations)){

    # Checking the locations, defaulting to NULL
    locations <- if(!is.null(impl_meta$locations)) impl_meta$locations else NULL

  }

#------------------------------------------------------------------------------#
# Step 2: Choose the truth source (training-vs-outcome rules) ------------------
#------------------------------------------------------------------------------#
# About: When a training series is provided in the options file we may score   #
# against it instead of the outcome truth. [1] training covers any real-time   #
# target -> use it in place of the outcome truth; [2] training differs from the#
# outcome (file paths / data labels) and covers nothing -> no real-time eval;  #
# [3] training identical to the outcome -> use the outcome; [4] no training -> #
# use the outcome. same/different is metadata only; coverage is tested after   #
# the join.                                                                    #
#------------------------------------------------------------------------------#

  ###############################################
  # Source fingerprint (paths + labels) by type #
  ###############################################
  source_meta <- function(master_type, xwalk_type){

    ##################################################################
    # Prefer the crosswalk: it carries the file path plus the labels #
    ##################################################################
    if(!is.null(variables_crosswalk) &&
       is.data.frame(variables_crosswalk) &&
       "variable_type" %in% names(variables_crosswalk)){

      # Pulling the data based on type: Training vs Outcome
      r <- variables_crosswalk[!is.na(variables_crosswalk$variable_type) &
                                 variables_crosswalk$variable_type == xwalk_type,
                               , drop = FALSE]

      # Pulling the columns to compare
      cols <- intersect(c("file", "data_source"), names(r))

    ##########################################################################
    # Fall back to master_data labels (data_source + variable, no file path) #
    ##########################################################################
    }else if(!is.null(master_data) && is.data.frame(master_data) &&
             "variable_type" %in% names(master_data)){

      # Pulling the data based on type: Training vs Outcome
      r <- master_data[!is.na(master_data$variable_type) &
                         master_data$variable_type == master_type, , drop = FALSE]

      # Pulling the columns to compare
      cols <- intersect(c("data_source"), names(r))

    #########################
    # Nothing to compare on #
    #########################
    }else{return(NULL)}

    ##################################
    # Check if nothing returns above #
    ##################################
    if(nrow(r) == 0 || length(cols) == 0) return(NULL)

    ############################
    # Preparing the final data #
    ############################
    sort(unique(apply(r[, cols, drop = FALSE], 1,
                      function(x) paste(x, collapse = "\r"))))

  }

  ###############################################################
  # Pulling the outcome data information: File Path, Data label #
  ###############################################################
  outcome_meta  <- source_meta(outcome_type, "outcome")

  ################################################################
  # Pulling the training data information: File Path, Data label #
  ################################################################
  training_meta <- source_meta(training_type, "training")

  ##############################
  # Checking for training data #
  ##############################
  training_present <- !is.null(training_meta)

  ############################################
  # Comparing training and outcome meta data #
  ############################################
  same_source <- training_present && !is.null(outcome_meta) &&
    identical(training_meta, outcome_meta)

  ############################################
  # Checking if training data should be used #
  ############################################
  use_training <- training_present && !same_source

  ####################################
  # Determining the final truth data #
  ####################################
  truth_type <- if(use_training) training_type else outcome_type

#------------------------------------------------------------------------------#
# Prepared evaluation frame ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section prepares the real-time data frame, joining Observed from #
# the chosen truth source (training when a distinct series was provided, else  #
# outcome).                                                                    #
#------------------------------------------------------------------------------#

  ######################################
  # Building the evaluation data frame #
  ######################################
  data.for.evaluation <- prepare_realtime_evaluation_data(
    impl_meta   = impl_meta,
    master_data = master_data,
    locations   = locations,
    time_step   = time_step,
    truth_type  = truth_type
  )

#------------------------------------------------------------------------------#
# Coverage gate (applies all four rules) ---------------------------------------#
#------------------------------------------------------------------------------#
# About: "Covered" means at least one archived target carries an observed value #
# from the chosen truth source. [1] distinct training that covers -> proceed;   #
# [2] distinct training that covers nothing -> skip; [3]/[4] outcome truth ->   #
# proceed when covered, skip when nothing is observed yet. Uncovered targets    #
# carry Observed = NA and drop out downstream in the metric helpers.            #
#------------------------------------------------------------------------------#

  covered <- is.data.frame(data.for.evaluation) &&
    nrow(data.for.evaluation) > 0 &&
    "Observed" %in% names(data.for.evaluation) &&
    any(!is.na(data.for.evaluation$Observed))

  ######################################################
  # Nothing to score -> omit the whole real-time block #
  ######################################################
  if(!covered){

    # Why there is no truth to score against
    reason <- if(use_training){

      # [2] Distinct training series that reaches none of the targets
      paste0("a training series was provided but it does not cover any ",
             "real-time forecast target")

    }else{

      # [3]/[4] Outcome truth, but no target has been observed yet
      "no forecast target has an observed value yet"

    }

    # Tell the user, then hand back an empty bundle
    message("build_realtime_evaluation(): ", reason,
            "; omitting the real-time evaluation section.")

    return(list(
      data             = data.for.evaluation,
      percentAgreement = NULL,
      forecastBias     = NULL,
      traditional      = NULL,
      has_data         = FALSE
    ))

  }

#------------------------------------------------------------------------------#
# Step 3: Metric calculations --------------------------------------------------
#------------------------------------------------------------------------------#
# About: The same context-agnostic helpers used for the testing period; each    #
# guards its own input, so an empty frame yields empty/NA output rather than    #
# an error. Peak timing/magnitude is intentionally not computed.                #
#------------------------------------------------------------------------------#

  percentAgreement.data <- percentAgreementCalculation(
    data.for.evaluation,
    non_transmission_months = non_transmission_months
  )

  forecastBias.data <- forecastBiasCalculation(
    data.for.evaluation,
    non_transmission_months = non_transmission_months,
    stable_threshold        = stable_threshold,
    pct_error_cushion       = pct_error_cushion
  )

  traditional.data <- traditionalMetricsCalculation(
    data.for.evaluation,
    non_transmission_months = non_transmission_months
  )

#------------------------------------------------------------------------------#
# Returning the bundle ---------------------------------------------------------
#------------------------------------------------------------------------------#

  list(
    data             = data.for.evaluation,
    percentAgreement = percentAgreement.data,
    forecastBias     = forecastBias.data,
    traditional      = traditional.data,
    has_data         = TRUE
  )
}
