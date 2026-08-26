#' Validate and normalize report parameters
#'
#' Checks that a `params` list supplied to a forecast evaluation report
#' template contains all required fields, that file paths exist on disk,
#' and that enumerated fields have allowed values. Returns a normalized
#' configuration list with consistent snake_case names. Collects all
#' validation errors and reports them together rather than failing on the
#' first error encountered.
#'
#' @param params A list, typically the `params` object of an RMarkdown
#'   rendered via [rmarkdown::render()].
#' @param verbose Logical. If `TRUE`, prints a success message on
#'   successful validation. Defaults to `FALSE` so report templates remain
#'   silent. Set `TRUE` for interactive console use.
#'
#' @return A list with validated, snake_case-named entries. File contents
#'   are *not* loaded by this function; only file existence is checked.
#'
#' @keywords internal
#' @noRd
validate_report_params <- function(params, verbose = FALSE){

#------------------------------------------------------------------------------#
# Creating the 'is_absent' function --------------------------------------------
#------------------------------------------------------------------------------#
# About: This function runs various checks to confirm that the user input IS   #
# NOT empty. All user entries either must have a value or a NA. This is the    #
# function applied throughout the script to check this.                        #
#------------------------------------------------------------------------------#
  is_absent <- function(x){

    # Checks for entities to confirm not empty
    is.null(x) || length(x) == 0L || (length(x) == 1L && (is.na(x) || identical(x, "")))

  }

#------------------------------------------------------------------------------#
# Creating the `add_error` function --------------------------------------------
#------------------------------------------------------------------------------#
# About: This function allows for the printing of errors to the console. The   #
# goal of this is to make it clear to users when one of their entries is not   #
# appropriately entered.                                                       #
#------------------------------------------------------------------------------#

  ##############################
  # Empty list to store errors #
  ##############################
  errors <- character()

  #####################################
  # Function to return error messages #
  #####################################
  add_error <- function(msg) errors <<- c(errors, msg)

#------------------------------------------------------------------------------#
# Helper: canonical state lookup -----------------------------------------------
#------------------------------------------------------------------------------#
# About: Helper to resolve a state input (abbreviation or full name) to its    #
# canonical two-letter abbreviation. Returns NA if the input doesn't match     #
# any known state.                                                             #
#------------------------------------------------------------------------------#

  ###########################################
  # Function to standardize the state input #
  ###########################################
  resolve_state <- function(x){

    # Handle missing state
    if(is_absent(x)) return(NA_character_)

    # Lowercase + trim for matching
    x_norm <- tolower(trimws(x))

    # Try abbreviation match
    abbr_match <- match(x_norm,
                        tolower(forecastEvalReport::hubverse_locations$abbreviation))

    # Returning the match if present
    if(!is.na(abbr_match)){
      return(forecastEvalReport::hubverse_locations$abbreviation[abbr_match])
    }

    # Try full name match
    name_match <- match(x_norm,
                        tolower(forecastEvalReport::hubverse_locations$location_name)
    )

    # Returning match if present
    if(!is.na(name_match)){
      return(forecastEvalReport::hubverse_locations$abbreviation[name_match])
    }

    # No match
    NA_character_

  }

#------------------------------------------------------------------------------#
# Checking all entries have a 'value' ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks that all entries have a value or a NA. If there   #
# is any missing entries for any of the user variables, this section will      #
# return an error message to the user prompting them to fix their response.    #
#------------------------------------------------------------------------------#

  ###########################
  # List of required inputs #
  ###########################
  required <- c("implementation.model.file",
                "evaluation.model.file",
                "disease",
                "name",
                "email",
                "base.box",
                "general.model.type",
                "outcome.data.label",
                "outcome.data",
                "outcome.name",
                "model.descriptions")

  ##########################################################
  # Checking to make sure all required entries are present #
  ##########################################################
  for(field in required){

    # Returning an error to the user
    if(!field %in% names(params)){

      # Calling the error function
      add_error(paste0("Required parameter `", field, "` is missing."))

    # Checking the options that allow NA
    }else if(field != "implementation.model.file" &&
             field != "evaluation.model.file" &&
             is_absent(params[[field]])){

      # Calling the error function
      add_error(paste0("Required parameter `", field, "` is missing or NA."))

    }
  }

#------------------------------------------------------------------------------#
# Checking the 'report reason' -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the 'report reason' input. Given that this system #
# only works with Hubverse and a few other types of data sets, we have to      #
# tell the system what to look for. This section checks that entry for the     #
# user to ensure they have selected the correct one.                           #
#------------------------------------------------------------------------------#

  ###########################
  # Allowed list of reasons #
  ###########################
  reason_canonical <- c("FluSight", "COVIDHub", "RSVHub", "MetroCast",
                        "DoD", "Software", "Internal")

  ############################################
  # Checking for entry (default to Internal) #
  ############################################
  if(is.null(params$reason) || is_absent(params$reason)){

    # Default to Internal
    reason_value <- "Internal"

  #############################
  # Checking for other errors #
  #############################
  }else{

    # Standardizing the reason & checking for match
    match_idx <- match(tolower(params$reason), tolower(reason_canonical))

    # Handling if no match is found
    if(is.na(match_idx)){

      # Calling the error function
      add_error(paste0(
        "`reason` must be one of: ",
        paste(reason_canonical, collapse = ", "),
        ". Got: '", params$reason, "'."
      ))

      # Saving the reason
      reason_value <- params$reason
    } else {
      reason_value <- reason_canonical[match_idx]
    }
  }

#------------------------------------------------------------------------------#
# Checking for either the implementation or evaluation files -------------------
#------------------------------------------------------------------------------#
# About: This section checks to make sure that either the implementation file  #
# path or the evaluation file path has been provided. One or the other or      #
# both MUST be indicated by the user. If there is an error, the system will    #
# return an error in the console.                                              #
#------------------------------------------------------------------------------#

  ##########################
  # Checking for the files #
  ##########################

  # Implementation file check
  impl_absent <- is_absent(params$implementation.model.file)

  # Evaluation file check
  eval_absent <- is_absent(params$evaluation.model.file)

  #############################################
  # Returning error if both files are missing #
  #############################################
  if(impl_absent && eval_absent){

    # Calling the error function
    add_error(
      "At least one of `implementation.model.file` or `evaluation.model.file` must be provided. Both are absent."
    )

  }

#------------------------------------------------------------------------------#
# Validating optional state.context field --------------------------------------
#------------------------------------------------------------------------------#
# About: This section validates the optional `state.context` field. If         #
# provided, it must resolve to a known U.S. state (by abbreviation or full     #
# name, case-insensitive). The resolved value is the canonical two-letter      #
# abbreviation. Used downstream by the general-format validator for county     #
# and HSA disambiguation.                                                      #
#------------------------------------------------------------------------------#

  ################################################
  # Resolving state.context to a canonical state #
  ################################################
  if(is_absent(params$state.context)){

    # Absent is allowed
    state_context_value <- NA_character_

  #################################
  # Validating the state variable #
  #################################
  }else{

    # Try to resolve
    state_context_value <- resolve_state(params$state.context)

    # Returning error if unresolvable
    if(is.na(state_context_value)){

      # Error to return to users
      add_error(paste0(
        "`state.context` could not be resolved to a known U.S. state. ",
        "Got: '", params$state.context,
        "'. Expected a two-letter abbreviation (e.g., 'SC') or full name ",
        "(e.g., 'South Carolina')."
      ))

    }

  }

#------------------------------------------------------------------------------#
# Checking the fields related to training data ---------------------------------
#------------------------------------------------------------------------------#
# About: This section checks the fields related to the training data. This     #
# includes making sure that either all fields are present or all are NA. They  #
# can not be mixed (i.e., some present and some not).                          #
#------------------------------------------------------------------------------#

  #####################################################
  # Determining the names of the training data fields #
  #####################################################
  training_fields <- c("training.data.file", "training.variable.name", "training_data_source")

  ###################################################
  # Checking which training data fields are missing #
  ###################################################
  training_absent <- vapply(training_fields, function(f) is_absent(params[[f]]), logical(1))

  ############################################
  # Determining if errors should be returned #
  ############################################
  if(any(training_absent) && !all(training_absent)){

    # Checking what fields are present
    present <- training_fields[!training_absent]

    # Checking what fields are missing
    missing <- training_fields[training_absent]

    # Calling the error function
    add_error(paste0(
      "Training data fields must be all-present or all-NA. ",
      "Present: ", paste(present, collapse = ", "), ". ",
      "Missing/NA: ", paste(missing, collapse = ", "), "."
    ))
  }

#------------------------------------------------------------------------------#
# Check if file pathways exist -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks to see if the provided file pathways exist. This  #
# will only run for provided file pathways and will not run when a file path   #
# is not provided. An error will return if any provided pathways can not be    #
# found.                                                                       #
#------------------------------------------------------------------------------#

  ##########################################
  # Determining what files must be checked #
  ##########################################

  # Empty list to store files to check
  files_to_check <- list()

  # Implementation file
  if (!impl_absent) files_to_check$implementation.model.file <- params$implementation.model.file

  # Evaluation file
  if (!eval_absent) files_to_check$evaluation.model.file <- params$evaluation.model.file

  # Outcome data file
  if (!is_absent(params$outcome.data)) files_to_check$outcome.data <- params$outcome.data

  # Training data
  if (!any(training_absent) && length(training_absent) == 3){
    files_to_check$training.data.file <- params$training.data.file
  }

  # Auxiliary data
  if (!is_absent(params$variables.crosswalk.file)) {
    files_to_check$variables.crosswalk.file <- params$variables.crosswalk.file
  }

  ###########################################
  # Checking if an error should be returned #
  ###########################################
  for(nm in names(files_to_check)){

    # Indexed file path to check
    path <- files_to_check[[nm]]

    # Checking & Returning error if needed
    if(!file.exists(path)){

      # Calling the error function
      add_error(paste0("File for `", nm, "` does not exist: ", path))

    }

  }

#------------------------------------------------------------------------------#
# Checking if the model description is formatted right -------------------------
#------------------------------------------------------------------------------#
# About: This section confirms that the model description section is formatted #
# as a data frame. This is set up in the options file, so this check is more   #
# of a bigger picture, syntax check.                                           #
#------------------------------------------------------------------------------#

  ########################################################
  # Ensuring that data frame is present and a data frame #
  ########################################################
  if(!is.null(params$model.descriptions) &&
     !is_absent(params$model.descriptions) &&
     !is.data.frame(params$model.descriptions)){

    # Calling the error function
    add_error("`model.descriptions` must be a data.frame.")

  }

#------------------------------------------------------------------------------#
# Consolidating all errors -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section consolidates all errors above into one place to ensure   #
# the user gets all errors at once rather than one at a time.                  #
#------------------------------------------------------------------------------#
  if(length(errors) > 0){

    # Stopping the code & Returning and Error
    stop(
      "Parameter validation failed with ", length(errors), " error(s):\n  - ",
      paste(errors, collapse = "\n  - "),
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Returning the list of cleaned parameters -------------------------------------
#------------------------------------------------------------------------------#
# About: This section returns the list of cleaned parameters, if and only if,  #
# the parameters do not return any errors. This is the data frame that will    #
# be called throughout the entire RMarkdown.                                   #
#------------------------------------------------------------------------------#

  ###############################
  # Creating the list to return #
  ###############################
  config <- list(
    implementation_model_file  = if (impl_absent) NA_character_ else params$implementation.model.file,
    evaluation_model_file      = if (eval_absent) NA_character_ else params$evaluation.model.file,
    disease                    = params$disease,
    contact_name               = params$name,
    contact_email              = params$email,
    reason                     = reason_value,
    base_box                   = params$base.box,
    general_model_type         = params$general.model.type,
    outcome_data_label         = params$outcome.data.label,
    outcome_data               = params$outcome.data,
    outcome_name               = params$outcome.name,
    training_data_file         = if (all(training_absent)) NA_character_ else params$training.data.file,
    training_variable_name     = if (all(training_absent)) NA_character_ else params$training.variable.name,
    training_data_source       = if (all(training_absent)) NA_character_ else params$training_data_source,
    model_descriptions         = params$model.descriptions,
    variables_crosswalk_file   = if (is_absent(params$variables.crosswalk.file)) NA_character_ else params$variables.crosswalk.file,
    state_context              = state_context_value,
    population_label           = if(is_absent(params$population.label)) NA_character_ else params$population.label,
    output_dir                 = if(is_absent(params$output.dir)) NA_character_ else params$output.dir,
    plot_styles = if(is.null(params$plot.styles)) NULL else params$plot.styles,

    # Optional user-supplied location crosswalk (raw code -> clean display
    # name). Resolved to a named character vector, or NULL when not provided.
    # Errors from a malformed crosswalk surface here with actionable text.
    location_crosswalk = tryCatch(
      read_location_crosswalk(params$location.crosswalk),
      error = function(e){
        stop("`location_crosswalk` is invalid.\n\n", conditionMessage(e),
             call. = FALSE)
      }
    )
  )

  ################################
  # Printing the success message #
  ################################
  if(isTRUE(verbose)) {
    message("\u2713 Parameters validated successfully.")
  }

  # Returning the list
  config
}
