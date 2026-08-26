#' Render the definitions and abbreviations accordion section
#'
#' Produces an accordion-wrapped definitions table compiled from the
#' variables crosswalk. Includes general_term rows (filtered by context),
#' outcome rows, and data_source rows that have a non-empty definition.
#' General terms are conditionally included based on what data is present
#' in the report and are displayed in a fixed meaningful order. The table
#' is rendered using `knitr::kable()` with `kableExtra` styling, wrapped
#' in an HTML accordion.
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
section_definitions <- function(variables_crosswalk,
                                config,
                                impl_meta = NULL,
                                eval_meta = NULL) {

#------------------------------------------------------------------------------#
# Input check ------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the function inputs to ensure they are in the     #
# correct format. If they are not in the correct format, the user is notified  #
# and the remainder of the script does not run.                                #
#------------------------------------------------------------------------------#

  ##########################################################
  # Checking if the variables cross walk has been provided #
  ##########################################################
  if(is.null(variables_crosswalk) || nrow(variables_crosswalk) == 0){

    # Message to print to users
    cat(paste0(
      '<details class="accordion">',
      '<summary><strong>Additional Information (Definitions and ',
      'Abbreviations)</strong></summary>',
      '<div class="accordion-body">',
      '<p>No crosswalk provided. Definitions are not available.</p>',
      '</div>',
      '</details>'
    ))

    # Returning NULL
    return(invisible(NULL))

  }

#------------------------------------------------------------------------------#
# Resolving context for conditional term filtering -----------------------------
#------------------------------------------------------------------------------#
# About: This section determines what is present in the report so general_term #
# rows can be filtered to only show relevant terms. Each flag corresponds to   #
# one or more glossary terms that should only appear under certain conditions. #
#------------------------------------------------------------------------------#

  #########################################################
  # Primary metadata source: Implementation or Evaluation #
  #########################################################
  meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

  ###########################################
  # Pulling the Spatial scale: Non-Regional #
  ###########################################
  spatial_scale_raw <- if(!is.null(meta$spatial_scale) &&
                          !is.na(meta$spatial_scale)){

    # Spatial scale is pulled
    meta$spatial_scale

  # Spatial scale can not be determined
  }else{""}

  # Regional spatial scale flag
  is_regional <- grepl("region", spatial_scale_raw, ignore.case = TRUE)

  ###############################
  # CDC(NHSN) data source check #
  ###############################
  is_cdc_nhsn <- !is.null(config$outcome_data_label) &&
    !is.na(config$outcome_data_label) &&
    grepl("CDC\\(NHSN\\)", config$outcome_data_label, fixed = TRUE)

  ###########################
  # Has current projections #
  ###########################
  has_projections <- !is.null(impl_meta) &&
    !is.null(impl_meta$projection_start) &&
    !is.na(impl_meta$projection_start)

  ############################
  # Has historical estimates #
  ############################
  has_historical <- !is.null(impl_meta) &&
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

  ###########################
  # Has auxiliary variables #
  ###########################
  has_aux <- !is.null(variables_crosswalk) &&
    any(!is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "aux_variable")

#------------------------------------------------------------------------------#
# Building the term inclusion map ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section maps each standard general term to its inclusion         #
# condition. Terms not in this map are user-added custom general terms and     #
# will always be included in the report.                                       #
#------------------------------------------------------------------------------#

  #####################################
  # Creating the term conditions list #
  #####################################
  term_conditions <- list(
    "Target Data"                        = TRUE,
    "Implementation Model"               = !is.null(impl_meta),
    "Current Projections"                = has_projections,
    "Historical Estimates"               = has_historical,
    "Evaluation Model"                   = has_eval,
    "Horizon"                            = TRUE,
    "Training Period"                    = has_training,
    "Validation Period"                  = has_validation,
    "Testing Period"                     = has_testing,
    "Auxiliary Variables"                = has_aux,
    "Auxiliary Variables (CDC Adjusted)" = has_aux && is_regional && is_cdc_nhsn
  )

#------------------------------------------------------------------------------#
# Defining the display sort order ----------------------------------------------
#------------------------------------------------------------------------------#
# About: Terms are displayed in a fixed meaningful order rather than           #
# alphabetically. Outcome definition always comes first (dynamic name).        #
# User-added custom general_terms not in this list are appended at the end     #
# in alphabetical order.                                                       #
#------------------------------------------------------------------------------#

  ##########################
  # Order of terms to show #
  ##########################
  standard_order <- c(

    # What are we forecasting
    "__OUTCOME__",

    # Model terms: Implementation Model
    "Implementation Model",

    # Model terms: Evaluation Model
    "Evaluation Model",

    # Model terms: Current Projections
    "Current Projections",

    # Model terms: Historical Estimates
    "Historical Estimates",

    # Time and methodology
    "Horizon",

    # Target Data
    "Target Data",

    # Auxiliary Variables
    "Auxiliary Variables",

    # Adj. Auxiliary Variables
    "Auxiliary Variables (CDC Adjusted)",

    # Evaluation periods: Training Period
    "Training Period",

    # Evaluation periods: Validation Period
    "Validation Period",

    # Evaluation periods: Testing Periods
    "Testing Period")

#------------------------------------------------------------------------------#
# Building the definitions data frame ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the definitions data frame that will show in the  #
# main report. It takes all of the information extracted above to create the   #
# accordion that will show in the main report.                                 #
#------------------------------------------------------------------------------#

  ##############################################
  # Function to check information is available #
  ##############################################
  is_nonempty <- function(x){
    !is.na(x) & nchar(trimws(as.character(x))) > 0
  }

  ################
  # Outcome rows #
  ################

  # Sub-setting the outcome rows
  out_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "outcome" &
      is_nonempty(variables_crosswalk$definition) &
      variables_crosswalk$definition != "USER: provide a definition", ]

  # Creating the outcome table
  out_table <- if(nrow(out_rows) > 0){

    # Data for outcome
    data.frame(
      Term       = unique(out_rows$clean_name_full),
      Definition = unique(out_rows$definition),
      sort_key   = "__OUTCOME__",
      stringsAsFactors = FALSE
    )

  # No outcome data available
  }else{

    # Empty data frame
    data.frame(Term = character(0), Definition = character(0),
               sort_key = character(0), stringsAsFactors = FALSE)

  }

  #####################
  # general_term rows #
  #####################

  # Pulling the general term rows
  gt_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "general_term" &
      is_nonempty(variables_crosswalk$definition), ]

  # Running only if there are general terms
  gt_table <- if(nrow(gt_rows) > 0){

    # Apply conditional filtering
    keep <- vapply(seq_len(nrow(gt_rows)), function(i){

      # Pulling the term
      term <- gt_rows$clean_name_full[i]

      # Determining if its a standard general term
      if(term %in% names(term_conditions)){

        # Checking if term should show
        isTRUE(term_conditions[[term]])

      # Pulling the user-added custom term
      }else{TRUE}

    }, logical(1))

    # Filtering variable cross walk to keep general terms
    gt_filtered <- gt_rows[keep, ]

    # Creating data frame if general terms are present
    if(nrow(gt_filtered) > 0){

      # Data Frame
      data.frame(
        Term       = gt_filtered$clean_name_full,
        Definition = gt_filtered$definition,
        sort_key   = gt_filtered$clean_name_full,
        stringsAsFactors = FALSE
      )

    # No general terms to include
    }else{

      # Empty data frame
      data.frame(Term = character(0), Definition = character(0),
                 sort_key = character(0), stringsAsFactors = FALSE)

    }

  # No general terms to include
  }else{

    # Empty data frame
    data.frame(Term = character(0), Definition = character(0),
               sort_key = character(0), stringsAsFactors = FALSE)

  }

  ####################
  # data_source rows #
  ####################

  # Get only the data source referenced by the outcome rows
  outcome_ds_value <- unique(variables_crosswalk$data_source[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "outcome"
  ])

  # Pulling data sources reference by non-outcome rows
  ds_rows <- variables_crosswalk[
    !is.na(variables_crosswalk$variable_type) &
      variables_crosswalk$variable_type == "data_source" &
      variables_crosswalk$variable %in% outcome_ds_value &
      is_nonempty(variables_crosswalk$definition), ]

  # Creating data set of data sources
  ds_table <- if(nrow(ds_rows) > 0){

    # Creating data frame
    data.frame(
      Term       = unique(ds_rows$clean_name_full),
      Definition = unique(ds_rows$definition),
      sort_key   = unique(ds_rows$clean_name_full),
      stringsAsFactors = FALSE
    )

  # No definitions to return
  }else{

    # Returning empty data frame
    data.frame(Term = character(0), Definition = character(0),
               sort_key = character(0), stringsAsFactors = FALSE)

  }

  ##########################
  # Stack and de-duplicate #
  ##########################
  all_terms <- rbind(out_table, gt_table, ds_table)

  # Drop rows missing Term or Definition
  all_terms <- all_terms[
    is_nonempty(all_terms$Term) &
      is_nonempty(all_terms$Definition), ]

  # Deduplicate on Term
  all_terms <- all_terms[!duplicated(all_terms$Term), ]

  # No terms are generated/available after de-duplicating
  if(nrow(all_terms) == 0){

    # Message to show to users
    cat(paste0(
      '<details class="accordion">',
      '<summary><strong>Additional Information (Definitions and ',
      'Abbreviations)</strong></summary>',
      '<div class="accordion-body">',
      '<p>No definitions found in the crosswalk.</p>',
      '</div>',
      '</details>'

    ))

    # Returning NULL
    return(invisible(NULL))

  }

#------------------------------------------------------------------------------#
# Applying the custom sort order -----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section applies the custom sort order to the row for the data    #
# frame. Rows are sorted by their position in standard_order. Terms not in     #
# the standard order (user-added custom terms) are appended alphabetically     #
# after all standard terms.                                                    #
#------------------------------------------------------------------------------#

  #########################################################
  # Position of each row's sort_key in the standard order #
  #########################################################
  order_idx <- match(all_terms$sort_key, standard_order)

  #################################################################
  # Rows not in standard_order get a high index so they sort last #
  #################################################################

  # Pulling the max index
  max_idx <- length(standard_order) + 1L

  # Assigning max index to non-standard rows
  order_idx[is.na(order_idx)] <- max_idx

  #####################################################################
  # Secondary sort: alphabetical within the "custom" group at the end #
  #####################################################################
  all_terms <- all_terms[
    order(order_idx, all_terms$Term), ]

  ####################################################
  # Drop the sort_key helper column before rendering #
  ####################################################

  # Dropping the sort key
  all_terms$sort_key <- NULL

  # Dropping the row names
  rownames(all_terms) <- NULL

#------------------------------------------------------------------------------#
# Rendering the kable table ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the kable table, which includes the terms and    #
# definitions created above. This section creates the base table and applies   #
# the HTML styling.                                                            #
#------------------------------------------------------------------------------#

  ###########################
  # Creating the base table #
  ###########################
  table_html <- knitr::kable(
    all_terms,
    format    = "html",
    col.names = c("Term", "Definition"),
    align     = c("l", "l"),
    escape    = TRUE
  )

  ##########################################
  # Creating the styles for the base table #
  ##########################################
  table_html <- kableExtra::kable_styling(
    table_html,
    full_width        = FALSE,
    bootstrap_options = c("bordered", "striped"),
    position          = "left"
  )

  ###########################################
  # Returning the full table as a character #
  ###########################################
  table_html <- as.character(table_html)

#------------------------------------------------------------------------------#
# Wrapping in accordion and rendering ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the accordion that the table sits in and renders #
# it to the main report.                                                       #
#------------------------------------------------------------------------------#

  ##########################
  # Creating the accordion #
  ##########################
  cat(paste0(
    '<details class="accordion">',
    '<summary><strong>Additional Information (Definitions and ',
    'Abbreviations)</strong></summary>',
    '<div class="accordion-body">',

    '<p>This section provides supplementary information to support ',
    'interpretation of the results presented in this report. It includes ',
    'key definitions and abbreviations used throughout the report, ',
    'including those related to the disease classification, target data, ',
    'population of interest, and date definitions.</p>',

    '<h3>Important Definitions and Abbreviations</h3>',

    table_html,

    '</div>',
    '</details>'
  ))

  invisible(NULL)

}
