#' Render the report header section
#'
#' Produces the HTML header block displayed at the top of the forecast
#' evaluation report. Shows the last updated date, disease, forecast
#' target, location (when a single geography is present), spatial scale,
#' target population, and current reference date.
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`,
#'   or `NULL`.
#' @param eval_meta Metadata list from `extract_evaluation_data()`,
#'   or `NULL`.
#' @param config Validated config list from `validate_report_params()`.
#'
#' @return Called for its side effect of rendering HTML via
#'   [htmltools::tagList()].
#'
#' @keywords internal
#' @noRd
section_header <- function(impl_meta, eval_meta, config, variables_crosswalk = NULL) {

#------------------------------------------------------------------------------#
# Pulling the meta data for the header -----------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls the meta data for the header box and cleans it     #
# to show the user-facing versions of all included meta data.                  #
#------------------------------------------------------------------------------#

  ####################################################################
  # Determining what meta data to pull: Implementation vs Evaluation #
  ####################################################################
  meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

  ###############################
  # Pulling the disease to show #
  ###############################
  disease_display <- {

    # Empty disease vector
    dcl <- NULL

    # Auto-populating vector if variable crosswalk is provided
    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk) &&
       all(c("variable_type", "disease_name_clean") %in% names(variables_crosswalk))){

      # Pulling the outcome rows from the crosswalk
      drows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "outcome", ]

      # Triggered if outcome rows are present
      if(nrow(drows) > 0){

        # Pulling unique diseases
        v <- unique(drows$disease_name_clean)

        # Pulling valid disease rows
        v <- v[!is.na(v) & nzchar(v)]

        # Assigning disease to vector
        if(length(v) > 0) dcl <- paste(v, collapse = ", ")
      }
    }

    # Crosswalk clean name -> raw config disease -> "Unknown"
    if(!is.null(dcl)){
      dcl
    }else if(!is.null(config$disease) && !is.na(config$disease)){
      config$disease
    }else{
      "Unknown"
    }
  }

  ##################################################################
  # Look up the clean outcome name from the crosswalk if available #
  ##################################################################
  outcome_display <- if(!is.null(variables_crosswalk)){

    # Filter to outcome rows only
    outcome_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "outcome", ]

    # Triggering if the above data set returned rows
    if(nrow(outcome_rows) > 0){

      # Use clean_name_full -- take unique values across groups
      clean_names <- unique(outcome_rows$clean_name_full)

      # Drop any that are still placeholders or NA
      clean_names <- clean_names[
        !is.na(clean_names) &
          nchar(trimws(clean_names)) > 0 &
          clean_names != "USER: provide a definition"
      ]

      # Creating outcome list: Multiple outcomes exist
      if(length(clean_names) > 0){

        # Collapse list on ","
        paste(clean_names, collapse = ", ")

      # Creating outcome list: Using meta data raw value
      }else{

        # Collapse list on ","
        paste(meta$outcome, collapse = ", ")

      }

    # Creating outcome list: No cleaned names found
    }else{

      # Collapse list on ","
      paste(meta$outcome, collapse = ", ")

    }

  # Using Outcome from meta data
  }else if(!is.null(meta$outcome) && length(meta$outcome) > 0){

    # Collapse list on ","
    paste(meta$outcome, collapse = ", ")

  # Outcome could not be identified
  }else{"Unknown"}

  ##########################################
  # Preparing the spatial scale for header #
  ##########################################
  scale_display <- if(!is.null(meta$spatial_scale) &&
                      !is.na(meta$spatial_scale)){

    # Extracting the spatial scale
    meta$spatial_scale

  ########################################
  # Spatial scale could not be extracted #
  ########################################
  }else{"Unknown"}

  #############################################
  # Preparing the population label for header #
  #############################################
  pop_display <- if(!is.null(meta$population_label) &&
                    !is.na(meta$population_label) &&
                    nchar(trimws(meta$population_label)) > 0){

    # Extracting the population label
    meta$population_label

  #############################
  # Cleaning population label #
  #############################
  }else if(!is.null(meta$target_population) &&
           !is.na(meta$target_population)){

    # Extracting and cleaning population label
    tools::toTitleCase(gsub("_", " ", meta$target_population))

  ############################################
  # Population label could not be identified #
  ############################################
  }else{"Unknown"}

  ###############################################
  # Preparing the reference date for the header #
  ###############################################
  ref_date_display <- if(!is.null(meta$reference_date)){

    # Formatting the date for the header
    format(max(meta$reference_date), "%B %d, %Y")

  #####################################
  # Reference date could not be found #
  #####################################
  }else{"Unknown"}

  #############################################
  # Preparing location (if needed) for header #
  #############################################
  locations <- if(!is.null(meta$locations)) meta$locations else character(0)

#------------------------------------------------------------------------------#
# Building the optional single-location row ------------------------------------
#------------------------------------------------------------------------------#
# About: Given that the report takes multi-locations as well, there is not     #
# always a need to show the locations in the header nor a way to do it.        #
# Therefore, location will only show if the report is for a single location    #
# rather than multiple.                                                        #
#------------------------------------------------------------------------------#

  ##############################################################
  # Creating the location row: Only one location is identified #
  ##############################################################
  location_row <- if(length(locations) == 1){

    # Content for row
    paste0(
      '<h4 style="margin: 5px 0; font-size: 1.40em;">',
      '<strong>Location:</strong> ', locations[[1]],
      '</h4>'
    )

  ########################################################
  # Creating an empty row: Multiple locations identified #
  ########################################################
  }else{""}

#------------------------------------------------------------------------------#
# Building the full header HTML ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the full header HTML from the input meta data    #
# and all of the customization above.                                          #
#------------------------------------------------------------------------------#

  # Header structure
  html <- paste0(
    # Last updated
    '<div style="text-align: center;">',
    '<h4>Last Updated: ', format(Sys.Date(), "%B %d, %Y"), '</h4>',
    '</div>',
    '<br>',

    # Info box
    '<div style="background-color: #f0f0f0; padding: 10px; border-radius: 5px;',
    ' width: 380px; margin: 0 auto; text-align: left;">',

    '<h4 style="margin: 5px 0; font-size: 1.40em;">',
    '<strong>Disease:</strong> ', disease_display,
    '</h4>',

    '<h4 style="margin: 5px 0; font-size: 1.40em;">',
    '<strong>Forecast Target:</strong> ', outcome_display,
    '</h4>',

    location_row,

    '<h4 style="margin: 5px 0; font-size: 1.40em;">',
    '<strong>Spatial Scale:</strong> ', scale_display,
    '</h4>',

    '<h4 style="margin: 5px 0; font-size: 1.40em;">',
    '<strong>Target Population:</strong> ', pop_display,
    '</h4>',

    '<h4 style="margin: 5px 0; font-size: 1.40em;">',
    '<strong>Current Week:</strong> ', ref_date_display,
    '</h4>',

    '</div>',
    '<br>'
  )

  # Returning the header as HTML
  htmltools::HTML(html)

}
