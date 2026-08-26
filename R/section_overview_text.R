#' Render the overview text paragraph for the report
#'
#' Produces the opening prose paragraph summarising the forecast submission
#' week, disease, outcome, spatial scale, model type(s), and temporal
#' resolution. The temporal resolution is derived from the detected time
#' step in the evaluation model when available, otherwise defaults to
#' weekly.
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`,
#'   or `NULL`.
#' @param eval_meta Metadata list from `extract_evaluation_data()`,
#'   or `NULL`.
#' @param config Validated config list from `validate_report_params()`.
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`.
#'
#' @return Called for its side effect of rendering HTML via
#'   [htmltools::HTML()].
#'
#' @keywords internal
#' @noRd
section_overview_text <- function(impl_meta,
                                  eval_meta,
                                  config,
                                  variables_crosswalk = NULL) {

#------------------------------------------------------------------------------#
# Resolving metadata -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls the values needed for the opening prose paragraph  #
# of the report. It will default to pulling the values from the implementation #
# model file, falling back to the evaluation model file when no implementation #
# model was provided by the user.                                              #
#------------------------------------------------------------------------------#

  ###########################
  # Primary metadata source #
  ###########################
  meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

  ##################
  # Reference date #
  ##################
  ref_date_display <- if(!is.null(meta$reference_date)){

    # Formatting reference date as date
    format(max(meta$reference_date), "%B %d, %Y")

  ##########################################
  # Reference date could not be identified #
  ##########################################
  }else{"Unknown"}

  ###########
  # Disease #
  ###########
  disease_display <- {

    # Empty disease vector to auto-populate with disease
    dcl <- NULL

    # Triggering if variable cross walk was provided
    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk) &&
       all(c("variable_type", "disease_name_clean") %in% names(variables_crosswalk))){

      # Pulling the outcome rows
      drows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "outcome", ]

      # Handling more than one outcome row
      if(nrow(drows) > 0){

        # Pulling unique clean disease names
        v <- unique(drows$disease_name_clean)

        # Pulling only valid disease names from list
        v <- v[!is.na(v) & nzchar(v)]

        # Auto-populating disease vector
        if(length(v) > 0) dcl <- paste(v, collapse = ", ")
      }
    }

    # Crosswalk clean name
    if(!is.null(dcl)){dcl

    # Crude disease name
    }else if(!is.null(config$disease) && !is.na(config$disease)){config$disease

    # No disease could be determined
    }else{"Unknown"}

  }

  #####################################
  # Clean outcome name from crosswalk #
  #####################################
  outcome_display <- if(!is.null(variables_crosswalk)){

    # Extracting the outcome rows from the cross walk
    outcome_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "outcome", ]

    # Triggered if there is at least one outcome row
    if(nrow(outcome_rows) > 0){

      # Extracting the unique clean names
      clean_names <- unique(outcome_rows$clean_name_full)

      # Determining if valid clean names
      clean_names <- clean_names[
        !is.na(clean_names) &
          nchar(trimws(clean_names)) > 0 &
          clean_names != "USER: provide a definition"
      ]

      # Triggering if more than one clean name
      if(length(clean_names) > 0){

        # Collapsing on ","
        paste(clean_names, collapse = ", ")

      ###########################################################
      # Clean name could not be found: Use primary outcome name #
      ###########################################################
      }else{if(!is.null(meta$outcome)) paste(meta$outcome, collapse = ", ") else "Unknown"}

    ###########################################################
    # Clean name could not be found: Use primary outcome name #
    ###########################################################
    }else{if(!is.null(meta$outcome)) paste(meta$outcome, collapse = ", ") else "Unknown"}

  ###########################################################
  # Clean name could not be found: Use primary outcome name #
  ###########################################################
  }else if(!is.null(meta$outcome) && length(meta$outcome) > 0){paste(meta$outcome, collapse = ", ")

  ##################################
  # No outcome name could be found #
  ##################################
  }else{"Unknown"}

  #################
  # Spatial scale #
  #################
  scale_raw <- if(!is.null(meta$spatial_scale) &&
                  !is.na(meta$spatial_scale)){

    # Extracting the spatial scale from meta data
    meta$spatial_scale

  ############################
  # Spatial scale is unknown #
  ############################
  }else{"unknown"}

  ######################################################
  # "region" gets special treatment per original logic #
  ######################################################
  scale_display <- if(tolower(trimws(scale_raw)) == "region"){"regional"

  # Not region spatial scale
  }else{tolower(scale_raw)}

  #################
  # Model type(s) #
  #################
  model_type_raw <- if(!is.null(config$general_model_type) &&
                       !is.na(config$general_model_type)){

    # Extracting the type of model
    config$general_model_type

  #####################################
  # No model type could get extracted #
  #####################################
  }else{"Unknown"}

  # Split on commas in case multiple model types are listed
  model_parts <- trimws(strsplit(model_type_raw, ",")[[1]])

  ####################################################
  # Preparing the model display: Only one model type #
  ####################################################
  model_display <- if(length(model_parts) == 1){model_parts

  ################################################
  # Preparing the model display: Two model types #
  ################################################
  }else if(length(model_parts) == 2){paste(model_parts, collapse = " and ")

  ##########################################################
  # Preparing the model display: Three or more model types #
  ##########################################################
  }else{

    # Text to show
    paste0(
      paste(model_parts[-length(model_parts)], collapse = ", "),
      " and ",
      model_parts[length(model_parts)]
    )

  }

  ######################################
  # Temporal resolution from time step #
  ######################################
  time_step <- if(!is.null(eval_meta$time_step)){

    # Using the calculated time step
    eval_meta$time_step

  ######################################
  # Default to weekly if no eval model #
  ######################################
  }else{7L}

  ###########################################
  # Creating the temporal resoluation label #
  ###########################################
  temporal_resolution <- switch(
    as.character(time_step),
    "1"  = "daily",
    "7"  = "weekly",
    "28" = ,
    "29" = ,
    "30" = ,
    "31" = "monthly",
    paste0(time_step, "-day")   # fallback for unusual intervals
  )

#------------------------------------------------------------------------------#
# Building the paragraph HTML --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section constructs the overview paragraph as a raw HTML string   #
# so that bold formatting is preserved exactly as in the original report.      #
#------------------------------------------------------------------------------#

  # HTML text to return
  html <- paste0(
    "<h1>Overview</h1>",
    "<p>",
    "This report summarizes the forecast submitted during the week ending on ",
    "<strong>", ref_date_display, "</strong>",
    " for ",
    "<strong>", disease_display, " ", outcome_display, "</strong>",
    " at the ",
    "<strong>", scale_display, " scale</strong>. ",
    "Forecasts were generated using ",
    "<strong>", model_display, "</strong>",
    " and are reported at a ",
    "<strong>", temporal_resolution, " temporal resolution</strong>. ",
    "Additional methodological details are provided below.",
    "</p>"
  )

  # Returning as actual HTML
  htmltools::HTML(html)

}
