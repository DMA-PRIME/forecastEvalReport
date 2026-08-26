#' Render the model development section intro paragraph
#'
#' Produces the section header and opening prose paragraph for the Model
#' Development and Forecast Visualization section. Describes the modeling
#' approach used to generate the forecast using the general model type
#' from the config. Multiple model types are formatted as a natural
#' language list with "and" before the final item.
#'
#' This section appears immediately before the forecast plots in the
#' report. A top margin spacer is included at the end to separate this
#' section from the plots below.
#'
#' @param config Validated config list from `validate_report_params()`.
#'
#' @return Called for its side effect of rendering HTML via
#'   [htmltools::HTML()].
#'
#' @keywords internal
#' @noRd
section_model_intro <- function(config) {

#------------------------------------------------------------------------------#
# Resolving the model type display string --------------------------------------
#------------------------------------------------------------------------------#
# About: This section formats the general_model_type from the config into a    #
# natural language list. Single model types are used as-is. Two model types    #
# are joined with "and". Three or more use comma separation with "and" before  #
# the last item. Matches the original stringr::str_replace logic using         #
# base R only.                                                                 #
#------------------------------------------------------------------------------#

  #################################################
  # Raw model type string: Pulled from cross walk #
  #################################################
  model_type_raw <- if(!is.null(config$general_model_type) &&
                          !is.na(config$general_model_type) &&
                          nchar(trimws(config$general_model_type)) > 0){

    # Pulling the general model
    config$general_model_type

  ########################################
  # Model string could not be determined #
  ########################################
  }else{"Unknown"}

  ###################################
  # Format as natural language list #
  ###################################

  # Pulling out the model names
  model_parts <- trimws(strsplit(model_type_raw, ",")[[1]])

  #############################################
  # Determining what model to show: One Model #
  #############################################
  model_display <- if(length(model_parts) == 1){model_parts

  #############################################
  # Determining what model to show: Two Model #
  #############################################
  }else if(length(model_parts) == 2){paste(model_parts, collapse = " and ")

  ###################################################
  # Determining what model to show: Multiple Models #
  ###################################################
  }else{

    # Showing the multiple models
    paste0(
      paste(model_parts[-length(model_parts)], collapse = ", "),
      " and ",
      model_parts[length(model_parts)]
    )

  }

#------------------------------------------------------------------------------#
# Building the full section HTML -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the full section's HTML. It includes the H1       #
# section header, the intro paragraph, and a top margin spacer div at the end  #
# to visually separate this section from the forecast plots below.             #                                                    #
#------------------------------------------------------------------------------#

  #############################
  # Building the section HTML #
  #############################
  html <- paste0(

    # Section header
    "<h1>Model Development &amp; Forecast Visualization</h1>",

    # Intro paragraph
    "<p>",
    "The forecast was generated using a ",
    "<strong>", model_display, "</strong>",
    "\u2013based modeling approach. ",
    "Additional methodological details are provided below, ",
    "including descriptions of the model structure, input variables, ",
    "and procedures used for model fitting and forecast generation.",
    "</p>",

    # Bottom spacer to separate from forecast plots
    "<div style=\"margin-top: 2em;\"></div>"

  )

  ######################
  # Returning the HTML #
  ######################
  htmltools::HTML(html)

}
