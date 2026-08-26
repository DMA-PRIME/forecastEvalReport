#' Build a variables crosswalk from a completed options file
#'
#' Reads a completed report options file, validates all parameters, loads
#' and validates the model files, and calls `build_variables_crosswalk()`
#' to seed the variables crosswalk CSV. This is the intended entry point
#' for crosswalk generation and should be run after filling in the options
#' file but before completing the crosswalk and running `generate_report()`.
#'
#' @param options_file Path to a completed report options `.R` file
#'   produced by [create_options_template()]. The
#'   `variables.crosswalk.file` field does not need to be filled in at
#'   this stage.
#' @param force Logical. Passed to [build_variables_crosswalk()]. If
#'   `TRUE`, overwrites an existing crosswalk file. Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the written crosswalk CSV.
#'
#' @export
build_crosswalk_from_options <- function(options_file, force = FALSE) {

#------------------------------------------------------------------------------#
# Validate inputs --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Given that the options file is required to seed the cross walk file,  #
# this section checks to make sure that the options file exists prior to       #
# completing the remainder of its tasks.                                       #
#------------------------------------------------------------------------------#

  ###########################
  # Options file must exist #
  ###########################
  if(missing(options_file) || is.null(options_file) || is.na(options_file)){

    # Stopping the script if no options file is provided
    stop(
      "No options file provided.\n\n",
      "Create a template with:\n",
      "  create_options_template()\n\n",
      "Fill in the template (except variables.crosswalk.file), then call:\n",
      "  build_crosswalk_from_options('path/to/report_options_template.R')",
      call. = FALSE
    )

  }

  ####################################
  # Options file pathway must exist #
  ####################################
  if(!file.exists(options_file)){

    # Stopping the script if the options file can not be found
    stop(
      "Options file not found: ", options_file, "\n\n",
      "Check the path and try again. To create a new template:\n",
      "  create_options_template()",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Reading the options file -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section sources the options file in a clean environment and      #
# confirms that a `report_options` list is defined within it.                  #
#------------------------------------------------------------------------------#

  #########################################################
  # Creating a new environment with no parent environment #
  #########################################################
  opts_env <- opts_env <- new.env(parent = baseenv())

  ######################################
  # Trying to read in the options file #
  ######################################
  tryCatch(

    # Sourcing the file
    source(options_file, local = opts_env),

    ###############################################
    # Runs if the options file can not be read in #
    ###############################################
    error = function(e){

      # Stopping the script if an error occurs
      stop(
        "Failed to read the options file.\n\n",
        "The file could not be sourced as valid R code. Check for syntax\n",
        "errors in: ", options_file, "\n\n",
        "R error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  ######################################
  # Confirm report_options list exists #
  ######################################
  if(!exists("report_options", envir = opts_env)){

    # Stopping the script if an error occurs
    stop(
      "The options file does not define a `report_options` list.\n\n",
      "Your options file should contain:\n",
      "  report_options <- list(\n",
      "    name  = 'Your Name',\n",
      "    email = 'your@email.com',\n",
      "    ...\n",
      "  )\n\n",
      "Re-create the template with create_options_template() if needed.",
      call. = FALSE
    )

  }

  #################################################
  # Extracting the 'report options' (config file) #
  #################################################
  opts <- get("report_options", envir = opts_env)

  ###############################################
  # Triggering if error occured with extraction #
  ###############################################
  if(!is.list(opts)){

    # Stopping the script if an error occurs
    stop(
      "`report_options` in the options file must be a list, but found: ",
      class(opts)[1], ".\n\n",
      "Re-create the template with create_options_template() if needed.",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Validate report parameters ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: Passes the options list to validate_report_params() to confirm all    #
# required fields are present, file paths exist, and enums are valid.          #
# variables.crosswalk.file does not need to be filled in at this stage.        #
#------------------------------------------------------------------------------#

  ##############################
  # Run validate_report_params #
  ##############################
  config <- tryCatch(

    # Running the validate parameters function
    forecastEvalReport:::validate_report_params(opts, verbose = FALSE),

    ################################################
    # Triggered if an error occurs with validation #
    ################################################
    error = function(e){

      # Stopping the script and returning an error
      stop(
        "Options file validation failed.\n\n",
        "Fix the following issue(s) in your options file:\n",
        "  ", options_file, "\n\n",
        conditionMessage(e),
        call. = FALSE
      )

    }
  )

#------------------------------------------------------------------------------#
# Load and validate the implementation model -----------------------------------
#------------------------------------------------------------------------------#
# About: If an implementation model file is provided, loads and validates it   #
# using the appropriate validator for the reason specified in the options file.#
# For Software and Internal reasons, uses validate_general_model(). For        #
# hubverse reasons, dispatches to the appropriate hub validator.               #
#------------------------------------------------------------------------------#

  ####################################
  # Loading the implementation model #
  ####################################
  if(!is.na(config$implementation_model_file)){

    # Validating the model using correct function
    implementation_model <- tryCatch(

      # Validation function options
      switch(config$reason,

             # Flusight files
             "FluSight"  = forecastEvalReport::validate_flusight_model(
               config$implementation_model_file
             ),

             # COVID hub files
             "COVIDHub"  = forecastEvalReport::validate_covidhub_model(
               config$implementation_model_file
             ),

             # RSV hub files
             "RSVHub"    = forecastEvalReport::validate_rsvhub_model(
               config$implementation_model_file
             ),

             # Metrocast hub files
             "MetroCast" = forecastEvalReport::validate_metrocast_model(
               config$implementation_model_file
             ),

             # Software hub files
             "Software"  = forecastEvalReport::validate_general_model(
               config$implementation_model_file,
               state_context = config$state_context
             ),

             # Internal or general files
             "Internal"  = forecastEvalReport::validate_general_model(
               config$implementation_model_file,
               state_context = config$state_context
             ),

             # Reading in the file
             utils::read.csv(
               config$implementation_model_file,
               colClasses     = c(location = "character"),
               stringsAsFactors = FALSE
             )
      ),

      ##########################################################
      # Running if an error occurs whem trying to read in file #
      ##########################################################
      error = function(e){

        # Stopping the script if an error occurs
        stop(
          "Implementation model validation failed.\n\n",
          "The file at implementation.model.file could not be validated:\n",
          "  ", config$implementation_model_file, "\n\n",
          conditionMessage(e),
          call. = FALSE
        )

      }
    )

  ##################################################
  # Running if no implementation file is available #
  ##################################################
  }else{implementation_model <- NULL}

#------------------------------------------------------------------------------#
# Load and validate the evaluation model ---------------------------------------
#------------------------------------------------------------------------------#
# About: If an evaluation model file is provided, loads and validates it using #
# validate_eval_model(). The evaluation file always uses the general format    #
# regardless of the reason specified.                                          #
#------------------------------------------------------------------------------#

  ################################
  # Loading the evaluation model #
  ################################
  if(!is.na(config$evaluation_model_file)){

    ###########################################
    # Trying to validate the evaluation model #
    ###########################################
    evaluation_model <- tryCatch(

      # Validating the evaluation model
      forecastEvalReport::validate_eval_model(
        config$evaluation_model_file,
        state_context = config$state_context
      ),

      #################################################
      # Triggering if an error occurs with validation #
      #################################################
      error = function(e){

        # Stopping the script if an error occurs
        stop(
          "Evaluation model validation failed.\n\n",
          "The file at evaluation.model.file could not be validated:\n",
          "  ", config$evaluation_model_file, "\n\n",
          conditionMessage(e),
          call. = FALSE
        )

      }
    )

  ###################################################
  # Returning NULL if no evaluation model is loaded #
  ###################################################
  }else{evaluation_model <- NULL}

#------------------------------------------------------------------------------#
# Build the variables crosswalk ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Passes the validated config and loaded model files to                 #
# build_variables_crosswalk() to seed the crosswalk CSV. The function writes   #
# the file to the working directory and prints a completion message telling    #
# the user what to fill in and where to provide the file path.                 #
#------------------------------------------------------------------------------#

  ##########################
  # Building the crosswalk #
  ##########################
  crosswalk_path <- tryCatch(

    # Building the variable cross walk file: Initial Seeding
    forecastEvalReport::build_variables_crosswalk(
      config               = config,
      implementation_model = implementation_model,
      evaluation_model     = evaluation_model,
      force                = force
    ),

    ###############################################
    # Triggering if an error occurs with building #
    ###############################################
    error = function(e){

      # Stopping script if an error occurs
      stop(
        "Crosswalk build failed.\n\n",
        conditionMessage(e),
        call. = FALSE
      )

    }
  )

  #################################
  # Making path to file invisible #
  #################################
  invisible(crosswalk_path)

}
