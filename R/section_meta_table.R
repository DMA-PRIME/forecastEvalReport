#' Render the metadata summary table for the report
#'
#' Produces an interactive metadata table showing the point of contact,
#' email, model type, target data source, and forecast target. Uses
#' `DT::datatable()` with ordering, paging, searching, and info display
#' disabled so it renders as a clean static summary.
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`,
#'   or `NULL`.
#' @param eval_meta Metadata list from `extract_evaluation_data()`,
#'   or `NULL`.
#' @param config Validated config list from `validate_report_params()`.
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`.
#'
#' @return A `DT::datatable()` object rendered inline in the report.
#'
#' @keywords internal
#' @noRd
section_meta_table <- function(impl_meta,
                               eval_meta,
                               config,
                               variables_crosswalk = NULL) {

#------------------------------------------------------------------------------#
# Extracting information for table ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls all of the information needed for the metadata     #
# table from the main configuration file (options file), the implementation    #
# and/or evaluation meta data tables, and the variables crosswalk.             #
#------------------------------------------------------------------------------#

  ################
  # Contact name #
  ################
  contact_display <- if(!is.null(config$contact_name) &&
                        !is.na(config$contact_name)){

    # Extracting the contact name
    config$contact_name

  #######################################
  # No contact name could be identified #
  #######################################
  }else{"Unknown"}

  #########
  # Email #
  #########
  email_display <- if(!is.null(config$contact_email) &&
                      !is.na(config$contact_email)){

    # Extracting the email provided
    config$contact_email

  ################################
  # No email could be identified #
  ################################
  }else{"Unknown"}

  ##############
  # Model type #
  ##############
  model_display <- if(!is.null(config$general_model_type) &&
                      !is.na(config$general_model_type)){

    # Extracting the model type
    config$general_model_type

  ####################################
  # No model type could be extracted #
  ####################################
  }else{"Unknown"}

  #########################################
  # Target data: From Variable Cross Walk #
  #########################################
  target_data_display <- if(!is.null(variables_crosswalk)){

    # Get the data_source value from the outcome rows
    outcome_ds_value <- unique(variables_crosswalk$data_source[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "outcome"
    ])

    # Filter data_source rows to only the one matching the outcome's data_source
    ds_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "data_source" &
        variables_crosswalk$variable %in% outcome_ds_value, ]

    # Triggered if there are rows from the above filtering
    if(nrow(ds_rows) > 0){

      # Pulling the unique list of clean full names
      clean_names <- unique(ds_rows$clean_name_full)

      # Standardizing the clean name
      clean_names <- clean_names[
        !is.na(clean_names) &
          nchar(trimws(clean_names)) > 0
      ]

      # Handling multiple data sources: Printing with ,
      if(length(clean_names) > 0){paste(clean_names, collapse = ", ")

      # Handling one or unknown data source
      }else{if(!is.null(config$outcome_data_label)) config$outcome_data_label else "Unknown"}

    # Handling one or unknown data source
    }else{if(!is.null(config$outcome_data_label)) config$outcome_data_label else "Unknown"}

  # Handling one data source
  }else if(!is.null(config$outcome_data_label)){config$outcome_data_label

  # Handling unknown data source
  }else{"Unknown"}

  ##############################
  # Clean forecast target name #
  ##############################
  outcome_display <- if(!is.null(variables_crosswalk)){

    # Pulling the outcome rows
    outcome_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "outcome", ]

    # Running if there are outcome rows
    if(nrow(outcome_rows) > 0){

      # Pulling the outcome clean names provided by users
      clean_names <- unique(outcome_rows$clean_name_full)

      # Cleaning up clean names
      clean_names <- clean_names[
        !is.na(clean_names) &
          nchar(trimws(clean_names)) > 0 &
          clean_names != "USER: provide a definition"
      ]

      # Handling if there are clean names
      if(length(clean_names) > 0){paste(clean_names, collapse = ", ")

      # Pulling the evaluation data
      }else{

        # Evaluation data
        meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

        # Pulling clean names
        if(!is.null(meta$outcome)) paste(meta$outcome, collapse = ", ") else "Unknown"

      }

    # Pulling evaluation data
    }else{

      # Evaluation data
      meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

      # Pulling clean names
      if(!is.null(meta$outcome)) paste(meta$outcome, collapse = ", ") else "Unknown"
    }

  # Pulling evaluation data
  }else{

    # Evaluation data
    meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

    # Pulling clean names
    if(!is.null(meta$outcome)) paste(meta$outcome, collapse = ", ") else "Unknown"

  }

#------------------------------------------------------------------------------#
# Building the metadata data frame ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the meta data frame from the information pulled   #
# above. This shows on every report, and provides a brief overview of the      #
# information a user or reader might need about a model.                       #
#------------------------------------------------------------------------------#

  ################################
  # Creating the meta data table #
  ################################
  meta_data <- data.frame(
    Field = c(
      "Point of Contact",
      "Email",
      "Model Type",
      "Target Data",
      "Forecast Target"
    ),
    Value = c(
      contact_display,
      email_display,
      model_display,
      target_data_display,
      outcome_display
    ),
    stringsAsFactors = FALSE
  )

  ###############################################
  # Rendering the data table to the main report #
  ###############################################
  DT::datatable(
    meta_data,
    rownames  = FALSE,
    colnames  = NULL,
    options   = list(
      ordering  = FALSE,
      paging    = FALSE,
      searching = FALSE,
      info      = FALSE
    )
  )

}
