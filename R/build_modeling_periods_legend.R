#' Build the modeling periods legend section
#'
#' Constructs a collapsible legend section for the training, validation,
#' and testing phase ribbons. Each period is included only when rows for
#' that phase exist in the evaluation data, so the legend always reflects
#' what is actually plotted. The section is collapsed by default since
#' phase ribbons serve as passive background context rather than primary
#' data traces.
#'
#' The `training_validation` column uses integer codes (1 = Training,
#' 2 = Validation, 0 = Testing) which are checked here using the same
#' mapping used in `build_phase_ribbon()`.
#'
#' Returns NULL when no evaluation model is available or when none of
#' the three phases have data, so the section is cleanly omitted.
#'
#' @param evaluation.model.file Character. Path to the evaluation model
#'   file from `config$evaluation_model_file`, or `NA`.
#' @param evaluation.temp A data frame of evaluation model data for the
#'   current location, or `NULL`.
#'
#' @return An `htmltools` tag (the legend section), or `NULL`.
#'
#' @keywords internal
#' @noRd
build_modeling_periods_legend <- function(evaluation.model.file,
                                          evaluation.temp) {

#------------------------------------------------------------------------------#
# Checks the inputs ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the function inputs to make sure that there are   #
# non-NULL inputs prior to creating the legend items.                          #
#------------------------------------------------------------------------------#

  ####################################################
  # Return NULL if no evaluation model file provided #
  ####################################################
  if(is.null(evaluation.model.file) ||
     (length(evaluation.model.file) == 1L &&
      is.na(evaluation.model.file))) return(NULL)

  ###############################################
  # Return NULL if no evaluation data available #
  ###############################################
  if(is.null(evaluation.temp) ||
     !is.data.frame(evaluation.temp) ||
     nrow(evaluation.temp) == 0) return(NULL)

#------------------------------------------------------------------------------#
# Building legend items for each available modeling period ---------------------
#------------------------------------------------------------------------------#
# About: This section checks each period individually using the integer code   #
# from the training_validation column (1 = Training, 2 = Validation,           #
# 0 = Testing). Periods with no rows in the evaluation data produce NULL which #
# htmltools ignores automatically so they are cleanly skipped.                 #
#------------------------------------------------------------------------------#

  #############################################################
  # Pulling the training period: Training Period Is Available #
  #############################################################
  training_item <- if(any(!is.na(evaluation.temp$training_validation) &
                            evaluation.temp$training_validation == 1L)){

    #####################################
    # Building the training legend item #
    #####################################
    build_legend_item(
      label        = "Training",
      trace_name   = "Training Period",
      swatch_class = "training",
      checked      = TRUE
    )

  ###############################
  # No training period provided #
  ###############################
  }else{NULL}

  #################################################################
  # Pulling the Validation Period: Validation Period is Available #
  #################################################################
  validation_item <- if(any(!is.na(evaluation.temp$training_validation) &
                              evaluation.temp$training_validation == 2L)){

    #######################################
    # Building the validation legend item #
    #######################################
    build_legend_item(
      label        = "Validation",
      trace_name   = "Validation Period",
      swatch_class = "validation",
      checked      = TRUE
    )

  #################################
  # No validation period provided #
  #################################
  }else{NULL}

  ###########################################################
  # Pulling the testing period: Testing Period is Available #
  ###########################################################
  testing_item <- if(any(!is.na(evaluation.temp$training_validation) &
                           evaluation.temp$training_validation == 0L)){

    ####################################
    # Building the testing legend item #
    ####################################
    build_legend_item(
      label        = "Testing",
      trace_name   = "Testing Period",
      swatch_class = "testing",
      checked      = TRUE
    )

  ##############################
  # No testing period provided #
  ##############################
  }else{NULL}

  #######################################
  # Return NULL if no periods have data #
  #######################################
  if(is.null(training_item) &&
     is.null(validation_item) &&
     is.null(testing_item)) return(NULL)

  ############################################
  # Building the entire model period section #
  ############################################
  build_legend_section(
    title      = "Modeling Periods",
    section_id = "modeling-periods",

    # Collapsed by default
    collapsed  = TRUE,

    # Content of the model period section
    content    = list(
      training_item,
      validation_item,
      testing_item
    )
  )

}
