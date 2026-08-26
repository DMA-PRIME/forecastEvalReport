#' Build auxiliary variable legend sections grouped by data source
#'
#' Constructs one collapsible legend section per unique data source group
#' in the auxiliary variable data. The legend group title is the source's
#' `clean_name_full` from the crosswalk with " Auxiliary Variables"
#' appended. Each individual legend item shows the variable's `clean_name_full`
#' from the crosswalk rather than the raw variable name.
#'
#' @param parameter_data A data frame of auxiliary variable data for the
#'   current location from `prepare_parameter_data()`, or `NULL`.
#' @param variables_crosswalk A validated crosswalk data frame, or `NULL`.
#'
#' @return A list of `htmltools` legend section tags, or `NULL`.
#'
#' @keywords internal
#' @noRd
build_auxiliary_variables_legend <- function(parameter_data,
                                             variables_crosswalk) {

#------------------------------------------------------------------------------#
# Checking the function inputs -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the function inputs to ensure they are not NULL   #
# prior to running the remainder of the section. If they are NULL, the         #
# function automatically returns NULL to ensure the remainder of the script    #
# can run as necessary.                                                        #
#------------------------------------------------------------------------------#

  ####################################
  # Return NULL if no parameter data #
  ####################################
  if(is.null(parameter_data) || !is.data.frame(parameter_data) ||
     nrow(parameter_data) == 0) return(NULL)

  #####################################
  # Checking for the data source name #
  #####################################
  if(!"data_source" %in% names(parameter_data)) return(NULL)

  ########################
  # Group by data source #
  ########################
  param_groups <- split(parameter_data, parameter_data$data_source)

  # Returning NULL if groupings return nothing
  if(length(param_groups) == 0) return(NULL)

#------------------------------------------------------------------------------#
# Creating legend item for each data source group ------------------------------
#------------------------------------------------------------------------------#
# About: This section creates a legend item for each data source group for the #
# auxiliary variables. Therefore, they are grouped by data source rather than  #
# each having their own drop down menu.                                        #
#------------------------------------------------------------------------------#
  lapply(names(param_groups), function(group_name){

    # Grabbing the indexed group
    group_data <- param_groups[[group_name]]

#------------------------------------------------------------------------------#
# Resolve legend section title from crosswalk ----------------------------------
#------------------------------------------------------------------------------#
# About: This section determines what the section title for the legend should  #
# be. The user provides the data source clean title in the crosswalk file,     #
# which is then used to label each group's variables. Auxiliary Variables is   #
# always appended so the section header clearly identifies what the group      #
# contains.                                                                    #
#------------------------------------------------------------------------------#

    ######################################
    # Getting the data menu source label #
    ######################################
    source_labels   <- get_aux_source_labels(group_name, variables_crosswalk)

    # Adding the Auxiliary Variables to the end
    formatted_title <- paste0(source_labels$legend_group_title,
                              " Auxiliary Variables")

#------------------------------------------------------------------------------#
# Build one legend item per unique variable ------------------------------------
#------------------------------------------------------------------------------#
# About: This section create the legend item per unique variable. The display  #
# label uses clean_name_full from the crosswalk aux_variable row rather than   #
# the raw variable column name. The trace_name must match the legendgroup set  #
# in add_parameter_traces_by_disease().                                        #
#------------------------------------------------------------------------------#

    ############################################
    # Building unique legend item per variable #
    ############################################
    param_items <- lapply(unique(group_data$variable), function(var_name){

      # Disease name for this variable (first match)
      var_disease <- unique(group_data$disease_name_clean[group_data$variable == var_name])[1]

      # Full trace name must match legendgroup in add_parameter_traces_by_disease
      full_trace_name <- paste0(var_name, " (", var_disease, ")")

      # Look up clean display name from crosswalk
      clean_label <- var_name

      ##############################################################
      # Pulling the auxiliary variable rows: Crosswalk is provided #
      ##############################################################
      if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk)){

        # Pulling the auxiliary variable rows
        aux_rows <- variables_crosswalk[
          !is.na(variables_crosswalk$variable_type) &
            variables_crosswalk$variable_type == "aux_variable" &
            !is.na(variables_crosswalk$variable) &
            variables_crosswalk$variable == var_name, ]

        # Preparing the clean label: Aux rows are present
        if(nrow(aux_rows) > 0 &&
           !is.na(aux_rows$clean_name_full[1]) &&
           nchar(trimws(aux_rows$clean_name_full[1])) > 0){

          # Saving the clean label
          clean_label <- aux_rows$clean_name_full[1]
        }
      }

      #############################
      # Building the legend items #
      #############################
      build_legend_item(
        label        = clean_label,
        trace_name   = paste0(source_labels$legend_group_title, "|", var_name),
        swatch_class = "param-line",
        checked      = FALSE,
        extra_attrs  = list(`data-group` = group_name)
      )

    })

    ########################
    # Assemble the section #
    ########################
    build_legend_section(
      title       = formatted_title,
      section_id  = group_name,
      collapsed   = TRUE,
      extra_class = "collapsible-section param-section",
      extra_attrs = list(`data-param-section` = "true"),
      content     = param_items
    )

  })

}
