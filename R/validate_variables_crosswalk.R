#' Validate a variables crosswalk file
#'
#' Performs a full validation of a variables crosswalk against the
#' forecastEvalReport crosswalk specification. The crosswalk maps variables
#' (data sources, auxiliary variables, outcomes, and general glossary terms)
#' to report-facing names, definitions, and metadata. Errors are collected
#' within each phase and reported together; a phase that produces errors
#' halts subsequent phases that depend on it.
#'
#' The crosswalk is typically produced by `build_variables_crosswalk()`,
#' completed by the user, and validated here as part of report generation.
#'
#' @param x Either a path to a crosswalk CSV file (character), or an
#'   already-loaded crosswalk data frame. Passing a data frame is intended
#'   for in-report validation; passing a path is intended for standalone
#'   checking outside report generation.
#' @param verbose Logical. If `TRUE`, prints a summary of the validated
#'   crosswalk on success. Defaults to `FALSE`.
#'
#' @return A data frame containing the validated crosswalk, with the
#'   `on_right_axis` and `convert_percent` columns normalized to logical
#'   values.
#'
#' @export
validate_variables_crosswalk <- function(x, verbose = FALSE) {

#------------------------------------------------------------------------------#
# Creating the `add_error` function --------------------------------------------
#------------------------------------------------------------------------------#
# About: This function allows for the printing of errors to the console. The   #
# goal of this is to make it clear to users when one of their entries is not   #
# appropriately entered. Errors are collected as they occur and reported at    #
# phase boundaries.                                                            #
#------------------------------------------------------------------------------#

  ##############################
  # Empty list to store errors #
  ##############################
  errors <- character()

  #####################################
  # Function to return error messages #
  #####################################
  add_error <- function(msg) errors <<- c(errors, msg)

  ##############################################
  # Function to abort if any errors are queued #
  ##############################################
  abort_if_errors <- function(){

    if(length(errors) > 0){

      stop(
        "Variables crosswalk validation failed with ", length(errors), " error(s):\n  - ",
        paste(errors, collapse = "\n  - "),
        call. = FALSE
      )

    }
  }

#------------------------------------------------------------------------------#
# Creating the `is_absent` helper ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This function checks whether a single value is "absent" -- NULL, NA,  #
# or an empty/whitespace-only string. Used throughout to test optional and     #
# conditionally-required cells.                                                #
#------------------------------------------------------------------------------#

  is_absent <- function(x){

    is.null(x) || length(x) == 0L ||
      (length(x) == 1L && (is.na(x) || nchar(trimws(as.character(x))) == 0L))

  }

#------------------------------------------------------------------------------#
# Creating the `parse_logical` helper ------------------------------------------
#------------------------------------------------------------------------------#
# About: This function parses a single value into a logical TRUE/FALSE, case-  #
# insensitively. Returns NA if the value is absent, and the sentinel string    #
# "INVALID" if the value is present but not a recognizable boolean.            #
#------------------------------------------------------------------------------#

  parse_logical <- function(v){

    # Absent -> NA
    if(is_absent(v)) return(NA)

    # Normalize
    v_norm <- tolower(trimws(as.character(v)))

    # Match recognized boolean forms
    if(v_norm %in% c("true", "t"))  return(TRUE)
    if(v_norm %in% c("false", "f")) return(FALSE)

    # Not a recognizable boolean
    "INVALID"

  }

#------------------------------------------------------------------------------#
# Defining variables crosswalk lookup tables -----------------------------------
#------------------------------------------------------------------------------#
# About: This section centralizes the crosswalk specification constants:       #
# required columns, allowed enum values, and the disease-to-clean-name map.    #
#------------------------------------------------------------------------------#

  ##############################
  # Required crosswalk columns #
  ##############################
  required_columns <- c(
    "spatial_scale", "location", "disease", "disease_name_clean",
    "variable_type", "variable", "clean_name_full", "clean_name_abb",
    "on_right_axis", "data_source", "convert_percent", "definition",
    "binning", "cohort", "file"
  )

  ##############################
  # Strict enum: variable_type #
  ##############################
  allowed_variable_type <- c("data_source", "aux_variable", "outcome", "general_term", "training")

  ########################
  # Strict enum: binning #
  ########################
  allowed_binning <- c("incident", "severity", "burden")

  #######################
  # Strict enum: cohort #
  #######################
  allowed_cohort <- c("dx", "dx_cond_lab")

  ################################################
  # Variable types that require a non-empty file #
  ################################################
  file_required_types <- c("aux_variable", "outcome", "training")

  #######################################################
  # Variable types that require a non-empty data_source #
  #######################################################
  data_source_required_types <- c("aux_variable", "outcome", "training")

  ######################################################
  # Variable types that require a non-empty definition #
  ######################################################
  definition_required_types <- c("outcome", "aux_variable", "general_term", "training")

  ###########################################
  # Disease-to-clean-name canonical mapping #
  ###########################################
  disease_clean_map <- c(
    covid_19  = "COVID-19",
    RSV       = "RSV",
    influenza = "Influenza"
  )

#------------------------------------------------------------------------------#
# Input resolution and file-level checks ---------------------------------------
#------------------------------------------------------------------------------#
# About: This section resolves the input, which may be either a path to a CSV  #
# file or an already-loaded data frame. When a path is supplied, the file is   #
# read from disk. The result must be a non-empty data frame before any         #
# further checks run.                                                          #
#------------------------------------------------------------------------------#

  ############################################
  # Resolving the input (path or data.frame) #
  ############################################
  if(is.data.frame(x)){

    # Data frame supplied directly -- use as-is
    data <- x

  # Checking if file name provided
  }else if(is.character(x) && length(x) == 1L){

    # Running if an error occurs
    if(!file.exists(x)){

      # Error to show to users
      add_error(paste0("File does not exist: ", x))

      # Triggering cumulative error function
      abort_if_errors()

    }

    ##################################
    # Reading in the CSV if provided #
    ##################################
    data <- tryCatch(

      # Trying to read in the CSV file
      utils::read.csv(
        x,
        stringsAsFactors = FALSE,
        na.strings       = c("NA", "")
      ),

      # Error to show to users if file could not be read in
      error = function(e){

        # Error to add to cumulative list
        add_error(paste0("File could not be read as CSV: ", conditionMessage(e)))
        NULL

      }

    )

    # Triggering the cumulative error function
    abort_if_errors()

  ####################################################
  # Running if total error occurs (nothing provided) #
  ####################################################
  }else{

    # Neither a data frame nor a single path
    add_error("`x` must be a data frame or a single path to a CSV file.")

    # Triggering the cumulative error function
    abort_if_errors()

  }

  #############################
  # Confirming non-empty data #
  #############################
  if(nrow(data) == 0){

    # Error to add to the cumulative error list
    add_error("Crosswalk contains no data rows.")

    # Triggering the cumulative error function
    abort_if_errors()

  }

#------------------------------------------------------------------------------#
# Column structure -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section confirms that the crosswalk has exactly the 15 required  #
# columns. Missing required columns or extra unexpected columns are treated    #
# as errors. Column existence must be confirmed before per-column value        #
# checks can run.                                                              #
#------------------------------------------------------------------------------#

  ################################
  # Checking for missing columns #
  ################################
  missing_cols <- setdiff(required_columns, names(data))

  # Running error if columns are missing
  if(length(missing_cols) > 0){

    # Error to show to users
    add_error(paste0(
      "Crosswalk is missing required column(s): ",
      paste(missing_cols, collapse = ", "), "."
    ))

  }

  ##############################
  # Checking for extra columns #
  ##############################
  extra_cols <- setdiff(names(data), required_columns)

  # Running error if columns are missing
  if(length(extra_cols) > 0){

    # Error to show to users
    add_error(paste0(
      "Crosswalk contains unexpected column(s): ",
      paste(extra_cols, collapse = ", "),
      ". Only the 15 required columns are allowed."
    ))

  }

  # Triggering the cumulative error function
  abort_if_errors()

#------------------------------------------------------------------------------#
# Per-column type and value checks ---------------------------------------------
#------------------------------------------------------------------------------#
# About: This section validates each individual column against its expected    #
# type and allowed value set. Conditional rules that depend on variable_type   #
# and cross-row foreign-key checks are handled in the following two sections.  #
#------------------------------------------------------------------------------#

  #########################################
  # `variable_type` must be a strict enum #
  #########################################
  bad_vartype <- which(is.na(data$variable_type) |
                         !data$variable_type %in% allowed_variable_type)

  # Running error if a bad variable type is provided
  if(length(bad_vartype) > 0){

    # Error to show to users
    add_error(paste0(
      "`variable_type` has ", length(bad_vartype), " invalid value(s). Allowed: ",
      paste(allowed_variable_type, collapse = ", "),
      ". First bad row: ", min(bad_vartype), "."
    ))

  }

  ###################################################
  # `spatial_scale` is non-empty: Non-general terms #
  ###################################################

  # List of rows that can't have NA
  ss_required_rows <- which(data$variable_type != "general_term")

  # Flagging rows with issues
  bad_ss_missing <- ss_required_rows[
    vapply(data$spatial_scale[ss_required_rows], is_absent, logical(1))
  ]

  # Triggering error if no spatial scale is provided
  if(length(bad_ss_missing ) > 0){

    # Error to show to users
    add_error(paste0(
      "`spatial_scale` is required for non-general_term rows but is missing in ",
      length(bad_ss_missing), " row(s). First bad row: ", min(bad_ss_missing), "."
    ))

  }

  ###########################################
  # `spatial_scale` is empty: General terms #
  ###########################################

  # List of rows that can have NA
  gen_rows <- which(data$variable_type == "general_term")

  # Flagging rows with issues
  bad_ss_general <- gen_rows[
    !vapply(data$spatial_scale[gen_rows], is_absent, logical(1))
  ]

  # Triggering error if spatial scale is provided
  if(length(bad_ss_general) > 0){

    # Error to show to users
    add_error(paste0(
      "`spatial_scale` must be NA for general_term rows but was set in ",
      length(bad_ss_general), " row(s). First bad row: ", min(bad_ss_general), "."
    ))

  }

  ##############################################
  # `location` is non-empty: Non general terms #
  ##############################################

  # List of rows that can not have NA
  loc_required_rows <- which(data$variable_type != "general_term")

  # Flagging rows with issues
  bad_loc_missing <- loc_required_rows[
    vapply(data$location[loc_required_rows], is_absent, logical(1))
  ]

  # Triggering error if no location is provided
  if(length(bad_loc_missing) > 0){

    # Error to show to users
    add_error(paste0(
      "`location` is required for non-general_term rows but is missing in ",
      length(bad_loc_missing), " row(s). First bad row: ", min(bad_loc_missing), "."
    ))

  }

  ######################################
  # `location` is empty: General terms #
  ######################################

  # Flagging rows with issues
  bad_loc_general <- gen_rows[
    !vapply(data$location[gen_rows], is_absent, logical(1))
  ]

  # Triggering error if no location is provided
  if(length(bad_loc_general) > 0){

    # Error to show to users
    add_error(paste0(
      "`location` must be NA for general_term rows but was set in ",
      length(bad_loc_general), " row(s). First bad row: ", min(bad_loc_general), "."
    ))

  }

  #############################################
  # `disease` is non-empty: Non-General Terms #
  #############################################

  # List of rows that can not have NA
  dis_required_rows <- which(data$variable_type != "general_term")

  # Flagging rows with issues
  bad_dis_missing <- dis_required_rows[
    vapply(data$disease[dis_required_rows], is_absent, logical(1))
  ]

  # Trigger error if no disease is provided
  if(length(bad_dis_missing) > 0){

    # Error to show to users
    add_error(paste0(
      "`disease` is required for non-general_term rows but is missing in ",
      length(bad_dis_missing), " row(s). First bad row: ", min(bad_dis_missing), "."
    ))

  }

  #####################################
  # `disease` is empty: General Terms #
  #####################################

  # Flagging rows with issues
  bad_dis_general <- gen_rows[
    !vapply(data$disease[gen_rows], is_absent, logical(1))
  ]

  # Triggering an error
  if(length(bad_dis_general) > 0){

    # Error message to show to users
    add_error(paste0(
      "`disease` must be NA for general_term rows but was set in ",
      length(bad_dis_general), " row(s). First bad row: ", min(bad_dis_general), "."
    ))

  }

  ########################################################
  # `disease_name_clean` is non-empty: Non-General terms #
  ########################################################

  # List of rows that can not have NA
  dnc_required_rows <- which(data$variable_type != "general_term")

  # Flagging rows with issues
  bad_dnc_missing <- dnc_required_rows[
    vapply(data$disease_name_clean[dnc_required_rows], is_absent, logical(1))
  ]

  # Triggering error if no clean name is provided
  if(length(bad_dnc_missing) > 0){

    # Error to show to users
    add_error(paste0(
      "`disease_name_clean` is required for non-general_term rows but is missing in ",
      length(bad_dnc_missing), " row(s). First bad row: ", min(bad_dnc_missing), "."
    ))

  }

  ################################################
  # `disease_name_clean` is empty: General terms #
  ################################################

  # Flagging rows with issues
  bad_dnc_general <- gen_rows[
    !vapply(data$disease_name_clean[gen_rows], is_absent, logical(1))
  ]

  # Triggering error clean name is provided
  if(length(bad_dnc_general) > 0){

    # Error to show to users
    add_error(paste0(
      "`disease_name_clean` must be NA for general_term rows but was set in ",
      length(bad_dnc_general), " row(s). First bad row: ", min(bad_dnc_general), "."
    ))

  }

  ###############################################################
  # Conditional: known disease must map to canonical clean name #
  ###############################################################

  # Determining if the value provided for disease is covid_19, RSV, or influenza
  known_disease_rows <- which(data$disease %in% names(disease_clean_map))

  # Checking if COVID, RSV, and Influenza provided clean names are correct
  bad_dnc_map <- known_disease_rows[
    data$disease_name_clean[known_disease_rows] !=
      disease_clean_map[data$disease[known_disease_rows]]
  ]

  # Triggering if the mapping does not work
  if(length(bad_dnc_map) > 0){

    # Error to show to users
    add_error(paste0(
      "`disease_name_clean` has ", length(bad_dnc_map),
      " row(s) where a known disease is not mapped to its canonical clean ",
      "name (covid_19 -> COVID-19, RSV -> RSV, influenza -> Influenza). ",
      "First bad row: ", min(bad_dnc_map), "."
    ))

  }

  ###########################
  # `variable` is non-empty #
  ###########################
  bad_var <- which(vapply(data$variable, is_absent, logical(1)))

  # Triggering error if variable is empty
  if(length(bad_var) > 0){

    # Error to show to users
    add_error(paste0(
      "`variable` has ", length(bad_var),
      " missing or empty value(s). First bad row: ", min(bad_var), "."
    ))

  }

  ##################################
  # `clean_name_full` is non-empty #
  ##################################
  bad_cnf <- which(vapply(data$clean_name_full, is_absent, logical(1)))

  # Triggering error if the full clean name is not provided
  if(length(bad_cnf) > 0){

    # Error to show to users
    add_error(paste0(
      "`clean_name_full` has ", length(bad_cnf),
      " missing or empty value(s). First bad row: ", min(bad_cnf), "."
    ))

  }

  #################################
  # `clean_name_abb` is non-empty #
  #################################
  bad_cna <- which(vapply(data$clean_name_abb, is_absent, logical(1)))

  # Triggering error if the abbreviation is none empty
  if(length(bad_cna) > 0){

    # Error to show to users
    add_error(paste0(
      "`clean_name_abb` has ", length(bad_cna),
      " missing or empty value(s). First bad row: ", min(bad_cna), "."
    ))

  }

  #########################################
  # `binning` must be a strict enum or NA #
  #########################################

  # Checking if binning has been provided
  binning_present <- !vapply(data$binning, is_absent, logical(1))

  # Checking the binning across the list
  bad_binning <- which(binning_present & !data$binning %in% allowed_binning)

  # Triggering error if the binning violates the enum
  if(length(bad_binning) > 0){

    # Error to show to users
    add_error(paste0(
      "`binning` has ", length(bad_binning),
      " invalid value(s) (must be one of ",
      paste(allowed_binning, collapse = ", "), ", or NA). ",
      "First bad row: ", min(bad_binning), "."
    ))

  }

  ########################################
  # `cohort` must be a strict enum or NA #
  ########################################

  # Checking if the cohort has been provided
  cohort_present <- !vapply(data$cohort, is_absent, logical(1))

  # Checking the cohort across the list
  bad_cohort <- which(cohort_present & !data$cohort %in% allowed_cohort)

  # Triggering error if the cohort violates the enum
  if(length(bad_cohort) > 0){

    # Error to show to users
    add_error(paste0(
      "`cohort` has ", length(bad_cohort),
      " invalid value(s) (must be one of ",
      paste(allowed_cohort, collapse = ", "), ", or NA). ",
      "First bad row: ", min(bad_cohort), "."
    ))

  }

  ###########################################
  # `on_right_axis` parses as boolean or NA #
  ###########################################

  # Checking if the entry for the on right axis is logical
  ora_parsed <- lapply(data$on_right_axis, parse_logical)

  # Flagging any entries that are not boolean
  bad_ora <- which(vapply(ora_parsed, function(v) identical(v, "INVALID"), logical(1)))

  # Triggering error if there is non-boolean values
  if(length(bad_ora) > 0){

    # Error to show to users
    add_error(paste0(
      "`on_right_axis` has ", length(bad_ora),
      " value(s) that are not recognizable booleans (TRUE/FALSE) or NA. ",
      "First bad row: ", min(bad_ora), "."
    ))

  }

  #############################################
  # `convert_percent` parses as boolean or NA #
  #############################################

  # Checking if the entry for the converting to percent is boolean
  cp_parsed <- lapply(data$convert_percent, parse_logical)

  # Flagging any entries that are not boolean
  bad_cp <- which(vapply(cp_parsed, function(v) identical(v, "INVALID"), logical(1)))

  # Triggering error if there is non-boolean values
  if(length(bad_cp) > 0){

    # Error to show to users
    add_error(paste0(
      "`convert_percent` has ", length(bad_cp),
      " value(s) that are not recognizable booleans (TRUE/FALSE) or NA. ",
      "First bad row: ", min(bad_cp), "."
    ))

  }

  # Triggering the cumulative error function
  abort_if_errors()

  ###############################################
  # Coerce booleans now that they are validated #
  ###############################################

  # Convert on right axis to a boolean
  data$on_right_axis   <- vapply(ora_parsed, function(v) if(is.logical(v)) v else NA, logical(1))

  # Convert percent to a boolean
  data$convert_percent <- vapply(cp_parsed,  function(v) if(is.logical(v)) v else NA, logical(1))

#------------------------------------------------------------------------------#
# Conditional rules by variable_type -------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces the rules that depend on each row's             #
# variable_type: which columns must be present and which must be NA. The       #
# foreign-key cross-reference is handled separately in the next section.       #
#------------------------------------------------------------------------------#

  ######################################################
  # `definition` required for outcome/aux/general rows #
  ######################################################

  # Checking which rows require definitions
  def_required_rows <- which(data$variable_type %in% definition_required_types)

  # Flagging the rows that violate this rule
  bad_def <- def_required_rows[
    vapply(data$definition[def_required_rows], is_absent, logical(1))
  ]

  # Trigger error if there is a violation
  if(length(bad_def) > 0){

    # Error to show to user
    add_error(paste0(
      "`definition` is required for variable_type in (",
      paste(definition_required_types, collapse = ", "),
      ") but is missing in ", length(bad_def), " row(s). First bad row: ",
      min(bad_def), "."
    ))

  }

  #####################################################
  # `on_right_axis` only applies to aux_variable rows #
  #####################################################

  # For any non-aux_variable row, on_right_axis must be NA.
  bad_ora_cond <- which(data$variable_type != "aux_variable" &
                          !is.na(data$on_right_axis))

  # Triggering error if there is a violation
  if(length(bad_ora_cond) > 0){

    # Error to show to users
    add_error(paste0(
      "`on_right_axis` must be NA unless variable_type is 'aux_variable', ",
      "but was set in ", length(bad_ora_cond), " non-aux_variable row(s). ",
      "First bad row: ", min(bad_ora_cond), "."
    ))

  }

  #######################################################
  # `convert_percent` only applies to aux_variable rows #
  #######################################################
  bad_cp_cond <- which(
    !is.na(data$convert_percent) &
      data$variable_type != "aux_variable" &
      data$variable_type != "outcome"
  )

  # Trigger error if there is a violation
  if(length(bad_cp_cond) > 0){

    # Error to show to users
    add_error(paste0(
      "`convert_percent` must be NA unless variable_type is 'aux_variable' or 'outcome', ",
      "but was set in ", length(bad_cp_cond), " non-aux_variable row(s). ",
      "First bad row: ", min(bad_cp_cond), "."
    ))

  }

  ################################################################
  # `data_source` column: required for aux/outcome, NA otherwise #
  ################################################################

  # Checking if data source column is present
  ds_present <- !vapply(data$data_source, is_absent, logical(1))

  # Rows that require data_source but lack it
  ds_required_rows <- which(data$variable_type %in% data_source_required_types)

  # Checking if any rows violate the data source row
  bad_ds_missing <- ds_required_rows[!ds_present[ds_required_rows]]

  # Triggering error if there is a violation
  if(length(bad_ds_missing) > 0){

    # Error to show to users
    add_error(paste0(
      "`data_source` is required for variable_type in (",
      paste(data_source_required_types, collapse = ", "),
      ") but is missing in ", length(bad_ds_missing), " row(s). First bad row: ",
      min(bad_ds_missing), "."
    ))

  }

  #############################################
  # Rows that have data_source but should not #
  #############################################

  # Checking if rows have data sources listed but should not
  bad_ds_extra <- which(!data$variable_type %in% data_source_required_types &
                          ds_present)

  # Triggering an error if a violation occurs
  if(length(bad_ds_extra) > 0){

    # Error to show to users
    add_error(paste0(
      "`data_source` must be NA unless variable_type is in (",
      paste(data_source_required_types, collapse = ", "),
      "), but was set in ", length(bad_ds_extra), " other row(s). ",
      "First bad row: ", min(bad_ds_extra), "."
    ))

  }

  #########################################################
  # `file` column: required for aux/outcome, NA otherwise #
  #########################################################

  # Checking if the file is provided
  file_present <- !vapply(data$file, is_absent, logical(1))

  # Rows that require file but lack it
  file_required_rows <- which(data$variable_type %in% file_required_types)

  # Flagging the rows that have violations
  bad_file_missing <- file_required_rows[!file_present[file_required_rows]]

  # Triggering an error if a violation occurs
  if(length(bad_file_missing) > 0){

    # Error to show to users
    add_error(paste0(
      "`file` is required for variable_type in (",
      paste(file_required_types, collapse = ", "),
      ") but is missing in ", length(bad_file_missing), " row(s). First bad row: ",
      min(bad_file_missing), "."
    ))

  }

  ######################################
  # Rows that have file but should not #
  ######################################

  # Checking if a row has a file and should not
  bad_file_extra <- which(!data$variable_type %in% file_required_types &
                            file_present)

  # Triggering an error if a violation occurs
  if(length(bad_file_extra) > 0){

    # Error to show to users
    add_error(paste0(
      "`file` must be NA unless variable_type is in (",
      paste(file_required_types, collapse = ", "),
      "), but was set in ", length(bad_file_extra), " other row(s). ",
      "First bad row: ", min(bad_file_extra), "."
    ))

  }

  #####################################################
  # `file` paths that are provided must exist on disk #
  #####################################################

  # Checking which rows have files listed
  file_check_rows <- which(file_present)

  # Flagging rows that do not have files that are accurate
  bad_file_exists <- file_check_rows[
    !vapply(data$file[file_check_rows], file.exists, logical(1))
  ]

  # Triggering an error if a violation occurs
  if(length(bad_file_exists) > 0){

    # Error to show to users
    add_error(paste0(
      "`file` references ", length(bad_file_exists),
      " path(s) that do not exist on disk. First bad row: ",
      min(bad_file_exists), "."
    ))

  }

  ###########################################
  # Triggering the script stopping function #
  ###########################################
  abort_if_errors()

#------------------------------------------------------------------------------#
# Foreign-key cross-reference --------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces the data_source foreign-key rule: every         #
# aux_variable and outcome row's `data_source` value must match the            #
# `variable` of a data_source-type row within the same                         #
# (spatial_scale, location, disease) group.                                    #
#------------------------------------------------------------------------------#

  ################################################################
  # Building the set of valid (group, data_source variable) keys #
  ################################################################

  # For each data_source row, the key is its group plus its variable.
  ds_rows <- which(data$variable_type == "data_source")

  # Creating the cross walk for each data source row
  ds_keys <- if(length(ds_rows) > 0){

    # Cross walk
    paste(
      data$spatial_scale[ds_rows],
      data$location[ds_rows],
      data$disease[ds_rows],
      data$variable[ds_rows],
      sep = "|"
    )

  # No cross walk to create
  }else{character(0)}

  #############################################################
  # Checking each aux_variable / outcome row against the keys #
  #############################################################

  # Pulling data types for reference rows
  ref_rows <- which(data$variable_type %in% c("aux_variable", "outcome", "training"))

  # Empty vector to store violations
  bad_fk <- integer()

  # Looping through rows with data sources
  for(i in ref_rows){

    # The group + referenced data_source value for this row
    lookup_key <- paste(
      data$spatial_scale[i],
      data$location[i],
      data$disease[i],
      data$data_source[i],
      sep = "|"
    )

    # Flag if no matching data_source row exists in the same group
    if(!lookup_key %in% ds_keys){
      bad_fk <- c(bad_fk, i)
    }

  }

  # Returning error if any foreign-key violations
  if(length(bad_fk) > 0){

    # Sample of unmatched data_source references
    sample_refs <- unique(paste0(
      data$data_source[bad_fk], " (", data$spatial_scale[bad_fk], "/",
      data$location[bad_fk], "/", data$disease[bad_fk], ")"
    ))

    # Error to show to users
    add_error(paste0(
      length(bad_fk),
      " aux_variable/outcome row(s) reference a `data_source` that has no ",
      "matching data_source-type row within the same ",
      "(spatial_scale, location, disease) group. ",
      "Unmatched: ", paste(utils::head(sample_refs, 5), collapse = "; "),
      if(length(sample_refs) > 5) paste0(" (and ", length(sample_refs) - 5, " more)") else "",
      ". First bad row: ", min(bad_fk), "."
    ))

  }

  ###########################################
  # Triggering the script stopping function #
  ###########################################
  abort_if_errors()

#------------------------------------------------------------------------------#
# Duplicate-row check ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section enforces that the crosswalk key -- the combination of    #
# spatial_scale, location, disease, variable_type, and variable -- is unique   #
# across all rows.                                                             #
#------------------------------------------------------------------------------#

  ##################################
  # No duplicate rows for same key #
  ##################################
  key_cols <- c("spatial_scale", "location", "disease",
                "variable_type", "variable")

  # Pulling the keys from the data sets
  key_str <- do.call(paste, c(data[key_cols], sep = "|"))

  # Checking for duplicates keys
  dup_idx <- which(duplicated(key_str))

  # Triggering error if there are duplicate keys
  if(length(dup_idx) > 0){

    # Error to show to users
    add_error(paste0(
      "Crosswalk contains ", length(dup_idx),
      " duplicate row(s) (same spatial_scale + location + disease + ",
      "variable_type + variable). First duplicate row: ", min(dup_idx), "."
    ))

  }

  ###########################################
  # Triggering the script stopping function #
  ###########################################
  abort_if_errors()

#------------------------------------------------------------------------------#
# Returning validated crosswalk and printing optional summary ------------------
#------------------------------------------------------------------------------#
# About: This section returns the validated crosswalk data frame on success.   #
# When `verbose = TRUE`, a summary of the crosswalk's contents is printed to   #
# the console for diagnostic purposes.                                         #
#------------------------------------------------------------------------------#

  ################################
  # Printing the success summary #
  ################################
  if(isTRUE(verbose)){

    # Creating the table with the validated cross walk
    vt_table   <- table(data$variable_type)

    # Summary of the cross walk
    vt_summary <- paste0(
      names(vt_table), " (", format(vt_table, big.mark = ","), ")",
      collapse = ", "
    )

    # List of diseases
    disease_list <- paste(unique(data$disease), collapse = ", ")

    # List of locations
    n_loc <- length(unique(data$location))

    # Message to show to users
    message(
      "\u2713 Variables crosswalk validated successfully.\n",
      "  Rows:          ", format(nrow(data), big.mark = ","), "\n",
      "  Locations:     ", n_loc, "\n",
      "  Diseases:      ", disease_list, "\n",
      "  Variable types: ", vt_summary
    )

  }

  ##############################
  # Returning the validated df #
  ##############################
  data

}
