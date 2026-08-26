#' Render the model description paragraph
#'
#' Produces the introductory paragraph describing how many unique models were
#' used in the analysis and the general model category they fall under. The
#' wording adapts to whether one or multiple models were used and whether
#' auxiliary variables (model inputs) are present in the report.
#'
#' This version derives the model count from `config$model_descriptions`,
#' formats the general model type with the same natural-language logic as
#' `section_model_intro()`, and detects auxiliary inputs from the
#' `aux_variable` rows in the crosswalk.
#'
#' Rendered as its own section function (returning `htmltools::HTML()`) so the
#' prose stays modular and out of the report template, matching the pattern of
#' `section_model_intro()` and `section_overview_text()`.
#'
#' @param config Validated config list from `validate_report_params()`.
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`. Used only to detect whether
#'   auxiliary model inputs are present.
#'
#' @return Called for its side effect of rendering HTML via
#'   [htmltools::HTML()].
#'
#' @keywords internal
#' @noRd
section_model_description <- function(config,
                                      variables_crosswalk = NULL) {

#------------------------------------------------------------------------------#
# Counting unique models -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines the number of unique models provided by the   #
# user. The number of unique models is the count of non-NA, non-blank entries  #
# in config$model_descriptions$model. Falls back to 1 when descriptions are    #
# absent so the prose still reads naturally.                                   #
#------------------------------------------------------------------------------#

  ###################################
  # Default is for one unique model #
  ###################################
  unique_models <- 1L

  ###############################################
  # Triggered if configuration file is provided #
  ###############################################
  if(!is.null(config$model_descriptions) &&
     is.data.frame(config$model_descriptions) &&
     "model" %in% names(config$model_descriptions)){

    # Pulling crude model names
    model_names <- config$model_descriptions$model

    # Removing NAs from model names
    model_names <- model_names[!is.na(model_names)]

    # Trimming white space from model names
    model_names <- trimws(as.character(model_names))

    # Only keeping existing model names
    model_names <- model_names[nchar(model_names) > 0]

    # Triggered if more than one model
    if(length(model_names) > 0){unique_models <- length(unique(model_names))}

  }

#------------------------------------------------------------------------------#
# Formatting the general model type --------------------------------------------
#------------------------------------------------------------------------------#
# About: This section uses the same natural-language list formatting as        #
# section_model_intro(): a single type is used as-is, two are joined with      #
# "and", three or more use comma separation with "and" before the final item.  #
#------------------------------------------------------------------------------#

  ############################################################
  # Pulling the raw model names: Configuration File Provided #
  ############################################################
  model_type_raw <- if(!is.null(config$general_model_type) &&
                          !is.na(config$general_model_type) &&
                          nchar(trimws(config$general_model_type)) > 0){

    # Raw Model Names
    config$general_model_type

  ###############################################################
  # Pulling the raw model names: No Configuration File Provided #
  ###############################################################
  }else{"Unknown"}

  ##############################
  # Breaking raw models by `,` #
  ##############################
  model_parts <- trimws(strsplit(model_type_raw, ",")[[1]])

  ##############################################
  # Creating the model display name: One Model #
  ##############################################
  model_type_display <- if(length(model_parts) == 1){model_parts

  ###############################################
  # Creating the model display name: Two Models #
  ###############################################
  }else if(length(model_parts) == 2){paste(model_parts, collapse = " and ")

  #########################################################
  # Creating the model display name: Three or More Models #
  #########################################################
  }else{

    # Creating the model display name
    paste0(
      paste(model_parts[-length(model_parts)], collapse = ", "),
      " and ",
      model_parts[length(model_parts)]
    )

  }

#------------------------------------------------------------------------------#
# Detecting auxiliary inputs ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: This question detects and extracts the auxiliary variable rows in the #
# cross walk. When present, the prose notes that input descriptions are        #
# included below.                                                              #
#------------------------------------------------------------------------------#

  ####################################
  # Default: No Inputs for Auxiliary #
  ####################################
  has_inputs <- FALSE

  ################################################################
  # Checking for auxiliary rows: Variable Cross Walk is Provided #
  ################################################################
  if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk)){

    # Extracting the auxiliary rows
    aux_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "aux_variable", ]

    # Boolean for exisitance of auxiliary rows
    has_inputs <- nrow(aux_rows) > 0

  }

#------------------------------------------------------------------------------#
# Building the paragraph -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the paragraph that will show in the reports.      #
# The wording differs for one vs. multiple models, and notes the model inputs  #
# only when auxiliary variables are present.                                   #
#------------------------------------------------------------------------------#

  ##########################################
  # Creating the paragraph: Only one model #
  ##########################################
  if(unique_models > 1){

    # Paragraph to show in report
    paragraph <- paste0(
      "For this project, <strong>", unique_models,
      " unique models</strong> were used as part of the analysis. ",
      "All models fall within the general model category <strong>",
      model_type_display,
      "</strong>, but may differ in structure, assumptions, and data inputs. ",
      "Brief descriptions of the models",
      if(has_inputs) " and their corresponding inputs" else "",
      " are provided below."
    )

  ###############################################
  # Creating the paragraph: More than one model #
  ###############################################
  }else{

    # Paragraph to show in report
    paragraph <- paste0(
      "For this project, <strong>", unique_models,
      " unique model</strong> was used as part of the analysis. ",
      "The model falls within the general model category <strong>",
      model_type_display,
      "</strong>, though its specific structure, assumptions, and data inputs ",
      "are tailored to the forecasting objective. ",
      "A brief description of the model",
      if(has_inputs) " and its corresponding inputs" else "",
      " is provided below."
    )

  }

  #####################################
  # Creating the HTML version to show #
  #####################################
  html <- paste0(
    "<p>", paragraph, "</p>",
    # Bottom spacer matching the old chunk's 1.4em margin
    "<div style=\"margin-top: 1.4em;\"></div>"
  )

  # Rendering the HTML
  htmltools::HTML(html)

}
