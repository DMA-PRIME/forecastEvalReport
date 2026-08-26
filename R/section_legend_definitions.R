#' Render the legend definitions accordion below the forecast plot
#'
#' Produces an accordion containing definitions for the figure legend
#' items. Only terms whose corresponding data is present in the report
#' are included. Definitions are pulled from the `general_term` rows in
#' the variables crosswalk — the same definitions used in the main
#' definitions section — so there is a single source of truth for all
#' term definitions.
#'
#' This section appears immediately below the forecast plot and before
#' the next report section.
#'
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`.
#' @param config Validated config list from `validate_report_params()`.
#' @param impl_meta Metadata list from `extract_implementation_data()`,
#'   or `NULL`.
#' @param eval_meta Metadata list from `extract_evaluation_data()`,
#'   or `NULL`.
#'
#' @return Called for its side effect of rendering HTML via `cat()`.
#'
#' @keywords internal
#' @noRd
section_legend_definitions <- function(variables_crosswalk,
                                       config,
                                       impl_meta = NULL,
                                       eval_meta = NULL) {

#------------------------------------------------------------------------------#
# Input check ------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the inputs to make sure they are in the correct   #
# format for the remainder of the script. If the variable crosswalk is not     #
# provided, this will return NULL. Otherwise, the remainder of the script will #
# run like normal.                                                             #
#------------------------------------------------------------------------------#

  #####################################################
  # Checking if variables crosswalk has been provided #
  #####################################################
  if(is.null(variables_crosswalk) || nrow(variables_crosswalk) == 0){

    # Creating the empty accordion
    cat(paste0(
      '<details class="accordion">',
      '<summary><strong>Additional Information</strong></summary>',
      '<div class="accordion-body">',
      '<p>No crosswalk provided. Legend definitions are not available.</p>',
      '</div>',
      '</details>'
    ))

    # Returning NULL
    return(invisible(NULL))

  }

#------------------------------------------------------------------------------#
# Resolving context for conditional term inclusion -----------------------------
#------------------------------------------------------------------------------#
# About: This section determines what data is present in the report so only    #
# relevant legend terms are shown. Uses the same logic as                      #
# section_definitions() to ensure consistency between the two definition       #
# tables.                                                                      #
#------------------------------------------------------------------------------#

  ###########################
  # Primary metadata source #
  ###########################
  meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

  ############################
  # Has implementation model #
  ############################
  has_impl <- !is.null(impl_meta)

  ###########################
  # Has current projections #
  ###########################
  has_projections <- has_impl &&
    !is.null(impl_meta$projection_start) &&
    !is.na(impl_meta$projection_start)

  ############################
  # Has historical estimates #
  ############################
  has_historical <- has_impl &&
    !is.null(impl_meta$historical_data) &&
    is.data.frame(impl_meta$historical_data) &&
    nrow(impl_meta$historical_data) > 0

  ########################
  # Has evaluation model #
  ########################
  has_eval <- !is.null(eval_meta)

  #######################
  # Has training period #
  #######################
  has_training <- has_eval &&
    !is.null(eval_meta$training_data) &&
    is.data.frame(eval_meta$training_data) &&
    nrow(eval_meta$training_data) > 0

  #########################
  # Has validation period #
  #########################
  has_validation <- has_eval &&
    !is.null(eval_meta$validation_data) &&
    is.data.frame(eval_meta$validation_data) &&
    nrow(eval_meta$validation_data) > 0

  ######################
  # Has testing period #
  ######################
  has_testing <- has_eval &&
    !is.null(eval_meta$testing_data) &&
    is.data.frame(eval_meta$testing_data) &&
    nrow(eval_meta$testing_data) > 0

#------------------------------------------------------------------------------#
# Building the term inclusion map and display order ----------------------------
#------------------------------------------------------------------------------#
# About: This section maps each legend term to its inclusion condition. Terms  #
# are ordered to match the visual order of the legend: target data first, then #
# model components, then evaluation periods. Only terms with TRUE conditions   #
# are included in the final table.                                             #
#------------------------------------------------------------------------------#

  ################################
  # Ordered term-condition pairs #
  ################################

  # Using a list of lists to preserve insertion order
  legend_terms <- list(
    list(term = "Target Data",          include = TRUE),
    list(term = "Implementation Model", include = has_impl),
    list(term = "Current Projections",  include = has_projections),
    list(term = "Historical Estimates", include = has_historical),
    list(term = "Evaluation Model",     include = has_eval),
    list(term = "Horizon",              include = has_eval),
    list(term = "Training Period",      include = has_training),
    list(term = "Validation Period",    include = has_validation),
    list(term = "Testing Period",       include = has_testing)
  )

  # Filter to only terms that should be included
  included_terms <- Filter(function(x) isTRUE(x$include), legend_terms)

  # Extract just the term names in order
  term_names <- vapply(included_terms, `[[`, character(1), "term")

  ####################################
  # Return early if no terms to show #
  ####################################
  if(length(term_names) == 0){

    # Warning message to show to users
    cat(paste0(
      '<details class="accordion">',
      '<summary><strong>Additional Information</strong></summary>',
      '<div class="accordion-body">',
      '<p>No legend definitions available for this report.</p>',
      '</div>',
      '</details>'
    ))

    # Returning NULL
    return(invisible(NULL))

  }

#------------------------------------------------------------------------------#
# Looking up definitions from the crosswalk ------------------------------------
#------------------------------------------------------------------------------#
# About: This section looks up the definition from the general_term rows in    #
# the crosswalk for each included term. Terms whose definition is missing or   #
# empty are included with a placeholder so the user knows the term exists      #
# but needs a definition added to the crosswalk.                               #
#------------------------------------------------------------------------------#

  ######################################
  # Helper to check for non-empty text #
  ######################################
  is_nonempty <- function(x){
    !is.na(x) & nchar(trimws(as.character(x))) > 0
  }

  ###############################
  # Filter to general_term rows #
  ###############################
  gt_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "general_term", ]

  ##################################
  # Look up each term's definition #
  ##################################
  all_terms <- data.frame(
    Term       = character(0),
    Definition = character(0),
    stringsAsFactors = FALSE
  )

  #########################
  # Looping through terms #
  #########################
  for(term in term_names){

    # Find the matching general_term row
    match_row <- gt_rows[
      !is.na(gt_rows$clean_name_full) &
        gt_rows$clean_name_full == term, ]

    ###################################################
    # Extract the definition: Definition can be Found #
    ###################################################
    def <- if(nrow(match_row) > 0 && is_nonempty(match_row$definition[1])){

      # Pulling the row with the definition
      match_row$definition[1]

    #######################################################
    # Extract the definition: Definition can not be found #
    #######################################################
    }else{

      # Fallback: no definition found in crosswalk
      "Definition not provided. Add a general_term row to the crosswalk."

    }

    #######################
    # Append to the table #
    #######################
    all_terms <- rbind(all_terms, data.frame(
      Term       = term,
      Definition = def,
      stringsAsFactors = FALSE
    ))

  }

  ##################################
  # Return early if table is empty #
  ##################################
  if(nrow(all_terms) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Rendering the kable table ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the definitions table using knitr::kable() and   #
# kableExtra::kable_styling() to match the formatting of the main definitions  #
# section and the original report.                                             #
#------------------------------------------------------------------------------#

  ############################
  # Creating the kable table #
  ############################
  table_html <- knitr::kable(
    all_terms,
    format    = "html",
    col.names = c("Term", "Definition"),
    align     = c("l", "l"),
    escape    = TRUE
  )

  ###################
  # Style for table #
  ###################
  table_html <- kableExtra::kable_styling(
    table_html,
    full_width        = FALSE,
    bootstrap_options = c("bordered", "striped"),
    position          = "left"
  )

  #################################
  # Converting table to character #
  #################################
  table_html <- as.character(table_html)

#------------------------------------------------------------------------------#
# Wrapping in accordion and rendering ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the HTML accordion with the included table and   #
# then renders it to the main report. This includes the header, text, and the  #
# table itself.                                                                #
#------------------------------------------------------------------------------#

  ##########################
  # Creating the accordion #
  ##########################
  cat(paste0(
    '<details class="accordion">',
    '<summary><strong>Additional Information</strong></summary>',
    '<div class="accordion-body">',

    '<p>This section provides definitions for the elements displayed in the ',
    'figure legends. These definitions clarify how each legend item, such as ',
    'modeling periods and model related components, corresponds to the data ',
    'shown in the figure. Definitions are provided to clarify terminology ',
    'used in the legend and do not imply causal interpretation.</p>',

    '<h3>Legend Definitions</h3>',

    table_html,

    '</div>',
    '</details>',

    '<div style="margin-top: 1em;"></div>'
  ))

  invisible(NULL)

}
