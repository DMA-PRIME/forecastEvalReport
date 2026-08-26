#' Build the evaluation model legend section
#'
#' Constructs a collapsible legend section for the evaluation model where
#' each unique forecast horizon gets its own checkbox legend item, plus an
#' optional training data item when training data was provided. Returns NULL
#' when no evaluation model file is provided, when the file path is NA or
#' empty, or when there are neither horizons nor training data to show, so the
#' section is cleanly omitted.
#'
#' Each horizon item is labelled "{N}-Week Horizon" and its checkbox
#' `trace_name` is the raw horizon key (e.g. "1"), matching the `legendgroup`
#' set on the corresponding evaluation model trace. The optional training item
#' is labelled with the training data source clean name and its `trace_name`
#' is that same clean name, matching the legendgroup on the training trace.
#'
#' @param evaluation.model.file Character. Path to the evaluation model
#'   file, or `NA` when no evaluation model was provided.
#' @param unique_horizons A vector of unique horizon values, or `NULL`.
#' @param evaluation.temp A data frame of evaluation model data, or `NULL`.
#' @param training_source_name Character. The training data source clean name
#'   to show as a checkbox under this section, or `NULL` when there is no
#'   training data. Default `NULL`.
#'
#' @return An `htmltools` tag or `NULL`.
#'
#' @keywords internal
#' @noRd
build_evaluation_model_legend <- function(evaluation.model.file,
                                          unique_horizons,
                                          evaluation.temp     = NULL,
                                          training_source_name = NULL) {

#------------------------------------------------------------------------------#
# Initial checks & Preparation -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section completes some initial checks to ensure the input files  #
# are present and in the correct format. It then normalizes information        #
# related to horizons and the training source name. It also flags when the     #
# evaluation data is present and when the training source name is present.     #
#------------------------------------------------------------------------------#

  #######################################################################
  # Return NULL if evaluation model file is absent, NA, or empty string #
  #######################################################################
  if(is.null(evaluation.model.file) ||
     length(evaluation.model.file) == 0 ||
     all(is.na(evaluation.model.file)) ||
     all(nchar(trimws(as.character(evaluation.model.file))) == 0)){

    # Returning NULL
    return(NULL)

  }

  ##################################################
  # Normalize horizons (may be NULL / contain NAs) #
  ##################################################
  if(!is.null(unique_horizons)){

    # Pulling unique horizons and removing NAs
    unique_horizons <- unique_horizons[!is.na(unique_horizons)]

  }

  # Indicator for horizon if present and not NULL
  has_horizons <- !is.null(unique_horizons) && length(unique_horizons) > 0

  ##########################################
  # Need evaluation data for horizon items #
  ##########################################
  has_eval_data <- !is.null(evaluation.temp) &&
    is.data.frame(evaluation.temp) &&
    nrow(evaluation.temp) > 0

  ##################################
  # Normalize training source name #
  ##################################

  # Flag for if training data is present
  has_training <- !is.null(training_source_name) &&
    length(training_source_name) > 0 &&
    !all(is.na(training_source_name)) &&
    all(nchar(trimws(as.character(training_source_name))) > 0)

  # Pulling training source name if present
  if(has_training){training_source_name <- as.character(training_source_name)[1]}

#------------------------------------------------------------------------------#
# Building the horizon & training legend entries -------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the horizon legend entries if there is evaluation #
# data present and the training (+ solid line swatch) if the user provides     #
# training data. The final step of this section builds the final evaluation    #
# model legend section within each of the individual elements.                 #
#------------------------------------------------------------------------------#

  ########################################################
  # Building the horizon legend item: Horizon is Present #
  ########################################################
  horizon_items <- if(has_horizons && has_eval_data){

    # Applying to each unique horizon
    lapply(unique_horizons, function(horizon){

      # Indexed horizon
      horizon_key <- as.character(horizon)

      # Building the horizon legend rows
      build_legend_item(
        label        = paste0(horizon_key, "-Week Horizon"),
        trace_name   = horizon_key,
        swatch_class = "legend-line-swatch",
        checked      = FALSE,
        swatch_attrs = list(
          `data-horizon`   = horizon_key,
          # Dotted swatch matches the dotted eval lines drawn on the plot
          `data-line-type` = "dot"
        )
      )

      })

  ######################################################
  # No horizon to plot in legend: Returning empty list #
  ######################################################
  }else{list()}

  ############################################################
  # Build the optional training item: Training data Provided #
  ############################################################
  training_item <- if(has_training){

    list(

      # Building the legend item
      build_legend_item(
        label        = training_source_name,
        trace_name   = training_source_name,
        swatch_class = "legend-line-swatch",
        checked      = FALSE,
        swatch_attrs = list(
          `data-horizon`   = training_source_name,
          `data-line-type` = "solid"
        )
      )
    )

  ###################################################
  # No training data provided: Returning empty list #
  ###################################################
  }else{list()}

  ##################################
  # Combining horizon and training #
  ##################################

  # Combining the horizon and training section
  content <- c(horizon_items, training_item)

  # Returning NULL if no things to show
  if(length(content) == 0) return(NULL)

  ##############################################
  # Building the full evaluation model section #
  ##############################################
  build_legend_section(
    title      = "Evaluation Model",
    section_id = "eval-horizons",
    collapsed  = TRUE,
    content    = content
  )

}
