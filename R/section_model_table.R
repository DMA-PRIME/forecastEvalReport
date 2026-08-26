#' Render the model development and inputs table
#'
#' Produces an interactive table summarizing the model inputs and the
#' modeling periods. The table lists the model variables (predictors) and
#' the data sources used, followed by the training, validation, testing,
#' and forecast period date ranges when each is available.
#'
#' This version reads everything from the validated `variables_crosswalk` (for
#' variables and data sources) and from `impl_meta` / `eval_meta` (for period
#' dates), matching the patterns used elsewhere in the report.
#'
#' Variables are the auxiliary model predictors: `aux_variable` rows in the
#' crosswalk, shown by `clean_name_full`. Data sources are the unique
#' `clean_name_full` values across all `data_source` rows. Rows with no
#' value (e.g. a period that is absent) are omitted so the table only shows
#' what applies.
#'
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`.
#' @param config Validated config list from `validate_report_params()`.
#' @param impl_meta Metadata list from `extract_implementation_data()`,
#'   or `NULL`. Supplies the forecast (projection) period dates.
#' @param eval_meta Metadata list from `extract_evaluation_data()`,
#'   or `NULL`. Supplies the training, validation, and testing period dates.
#'
#' @return A `DT::datatable()` object rendered inline in the report.
#'
#' @keywords internal
#' @noRd
section_model_table <- function(variables_crosswalk,
                                config,
                                impl_meta = NULL,
                                eval_meta = NULL) {

#------------------------------------------------------------------------------#
# clean a vector of names ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section drops NA, blank, and placeholder entries and returns the #
# unique set of variable names. It is used for both the variables list and the #
# data sources list so the table never shows empty of boilerplate values.      #
#------------------------------------------------------------------------------#

  ####################################
  # Function to clean variable names #
  ####################################
  clean_names <- function(x){

    # Removing NAs
    x <- x[!is.na(x)]

    # Removing white space
    x <- trimws(as.character(x))

    # Handling missing injuries
    x <- x[nchar(x) > 0 & x != "USER: provide a definition"]

    # Creating unique list of variables
    unique(x)

  }

#------------------------------------------------------------------------------#
# Variables (auxiliary model predictors) ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls the auxiliary variable rows, and subsequent clean  #
# variable names.                                                              #
#------------------------------------------------------------------------------#

  #####################################
  # Empty vector to store clean names #
  #####################################
  vars_clean <- character(0)

  ##################################################
  # Triggered if variables cross walk is available #
  ##################################################
  if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk)){

    # Pulling the auxiliary rows
    aux_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "aux_variable", ]

    # Triggered if there are auxiliary rows
    if(nrow(aux_rows) > 0){

      # Cleaning the "cleaned" variable names
      vars_clean <- clean_names(aux_rows$clean_name_full)

    }

  }

#------------------------------------------------------------------------------#
# Data sources -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls the data sources rows for all of the data from     #
# the cross walk, which are then shown in the table by the entry in            #
# 'clean_name_full'. This covers the outcome source, auxiliary sources, and    #
# and the training sources together.                                           #
#------------------------------------------------------------------------------#

  ###################################
  # Empty vector store data sources #
  ###################################
  all_data_sources <- character(0)

  ##################################################
  # Triggered if variables cross walk is available #
  ##################################################
  if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk)){

    # Pulling the data source rows
    ds_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "data_source", ]

    # Triggered if there are any data source rows
    if(nrow(ds_rows) > 0){

      # Cleaning the "cleaned" variable names
      all_data_sources <- clean_names(ds_rows$clean_name_full)

    }

  }

  ########################################################################
  # Fall back to the outcome data label if the crosswalk yielded nothing #
  ########################################################################
  if(length(all_data_sources) == 0 &&
     !is.null(config$outcome_data_label) &&
     !all(is.na(config$outcome_data_label))){

    # Pulling the list of data sources
    all_data_sources <- clean_names(config$outcome_data_label)

  }

#------------------------------------------------------------------------------#
# Model inputs summary rows ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the summary rows for the variables and data      #
# sources that shows in the tables. It uses the information generated above,   #
# and will drop any rows that do not have values.                              #
#------------------------------------------------------------------------------#

  #########################################
  # Creating the model input summary rows #
  #########################################
  model_data <- data.frame(

    # Column headers
    Field = c("Variables", "Data Sources"),

    # Values for rows
    Value = c(

      # Variable list
      if(length(vars_clean) == 0) NA_character_
      else paste(vars_clean, collapse = "; "),

      # Data source list
      if(length(all_data_sources) == 0) NA_character_
      else paste(all_data_sources, collapse = "; ")
    ),
    stringsAsFactors = FALSE
  )

  # Drop input rows with no value
  model_data <- model_data[!is.na(model_data$Value), , drop = FALSE]

#------------------------------------------------------------------------------#
# Build a period row when both dates are present -------------------------------
#------------------------------------------------------------------------------#
# About: This section returns a one-row data frame for a period, or NULL when  #
# either date is missing so bind_rows cleanly omits absent periods.            #
#------------------------------------------------------------------------------#

  #######################################################
  # Template for training, validation, and testing rows #
  #######################################################
  period_row <- function(field, start_date, end_date){

    # Returning NULL if all dates are missing
    if(is.null(start_date) || is.null(end_date) ||
       all(is.na(start_date)) || all(is.na(end_date))) return(NULL)

    # Creating the data frame with all information
    data.frame(
      Field = field,
      Value = paste0(anytime::anydate(start_date),
                     " through ",
                     anytime::anydate(end_date)),
      stringsAsFactors = FALSE
    )
  }

#------------------------------------------------------------------------------#
# Period rows ------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the training, validation, and testing dates      #
# that come from eval_meta; the forecast (projection) dates come from          #
# impl_meta. Each is included only when present.                               #
#------------------------------------------------------------------------------#

  #################
  # Training rows #
  #################
  training_row   <- period_row("Training Period",
                               eval_meta$training_start,
                               eval_meta$training_end)

  ###################
  # Validation rows #
  ###################
  validation_row <- period_row("Validation Period",
                               eval_meta$validation_start,
                               eval_meta$validation_end)

  ################
  # Testing rows #
  ################
  testing_row    <- period_row("Testing Period",
                               eval_meta$testing_start,
                               eval_meta$testing_end)

  ########################
  # Forecast period rows #
  ########################
  forecast_row   <- period_row("Forecast Period",
                               impl_meta$projection_start,
                               impl_meta$projection_end)

#------------------------------------------------------------------------------#
# Combine and render -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the final table that will show on the main page. #
# It first combines all available rows into one data frame, and then uses the  #
# data table package to create the final table that is exported to the report. #
#------------------------------------------------------------------------------#

  ##################################
  # Creating the combined data set #
  ##################################
  combined_data <- dplyr::bind_rows(
    model_data,
    training_row,
    validation_row,
    testing_row,
    forecast_row
  )

  ##################################
  # Final table to return to users #
  ##################################
  DT::datatable(
    combined_data,
    rownames = FALSE,
    colnames = NULL,
    options  = list(
      ordering   = FALSE,
      paging     = FALSE,
      searching  = FALSE,
      info       = FALSE,
      columnDefs = list(
        list(width = "200px", targets = 0)
      )
    )
  )

}
