#' Build the implementation model legend section
#'
#' Constructs the legend section for the implementation model. The
#' checkbox trace names match the legendgroup values set on the
#' corresponding plotly traces in the plot loop:
#'   - Current Projections: legendgroup = "Implementation Model"
#'   - Historical Estimates: legendgroup = "Implementation Model"
#'   - PI levels: legendgroup = paste0("PI-", pi_name)
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`,
#'   or `NULL`.
#' @param bounds Named list of PI bounds from `prepare_pi_bounds()`,
#'   or `NULL`.
#' @param pi_styles Named list of PI style settings from
#'   `PLOT_STYLES$pi_styles`.
#'
#' @return An `htmltools` tag or `NULL`.
#'
#' @keywords internal
#' @noRd
build_implementation_model_legend <- function(impl_meta, bounds, pi_styles) {

#------------------------------------------------------------------------------#
# Checking if legend show be built ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section determines if the legend section for the implementation  #
# model should be built. It checks for the files, and historical estimates.    #
# The next sections will build the traces related to the implementation model. #
#------------------------------------------------------------------------------#

  ##########################################
  # Return NULL if no implementation model #
  ##########################################
  if(is.null(impl_meta)) return(NULL)

  #######################################################
  # Empty indicator to save historical estimates legend #
  #######################################################
  historical_item <- NULL

  ##############################################
  # Checking if there are historical estimates #
  ##############################################
  has_historical <- !is.null(impl_meta$historical_data) &&
    is.data.frame(impl_meta$historical_data) &&
    nrow(impl_meta$historical_data) > 0

#------------------------------------------------------------------------------#
# Building the legend items ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the various legend items and then returns them    #
# to the main script. This includes conditional legend items, such as the      #
# historical estimates and other sections for the projections and the PIs.     #
#------------------------------------------------------------------------------#

  ##########################################
  # Checking if historical estimates exist #
  ##########################################
  if(has_historical){

    # Building the historical estimates legend item
    historical_item <- build_legend_item(
      label        = "Historical Estimates",
      trace_name   = "Implementation Model",
      swatch_class = "historical-estimates",
      checked      = TRUE
    )
  }

  ################################
  # Building the PI legend items #
  ################################
  pi_items <- build_pi_legend_items(bounds, pi_styles)

  ########################################################
  # Building the legend section for implementation model #
  ########################################################
  build_legend_section(
    title      = "Implementation Model",
    section_id = "implementation-model",
    collapsed  = FALSE,
    content    = list(

      ##################################
      # Current Projections legend row #
      ##################################
      build_legend_item(
        label        = "Current Projections",
        trace_name   = "Implementation Model",
        swatch_class = "current-projections",
        checked      = TRUE
      ),

      ###################################
      # Historical Estimates legend row #
      ###################################
      historical_item,

      ########################
      # PI levels legend row #
      ########################
      pi_items

    )
  )

}
