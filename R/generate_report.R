#' Generate a forecast evaluation report
#'
#' Reads a completed report options file, validates all parameters, and
#' renders the forecast evaluation RMarkdown report to an HTML file. This
#' is the primary user-facing entry point for report generation.
#'
#' The options file is an R script that defines a list named
#' `report_options`. Create a template using `create_options_template()`,
#' fill in all fields, build and complete the variables crosswalk using
#' `build_crosswalk_from_options()`, add the crosswalk path to the options
#' file, then call this function.
#'
#' Plot appearance can be fully customized by passing a styles object
#' produced by `create_plot_styles()`. If no styles object is provided,
#' the default appearance is used automatically.
#'
#' By default a single report is produced containing every location in the
#' model file(s), switchable via the in-report geography dropdown. Set
#' `split_by_location = TRUE` to instead produce one self-contained report
#' per location: the implementation and evaluation model files are filtered
#' to each location in turn and rendered individually. This automates the
#' otherwise-manual workflow of pre-filtering the model CSVs by location and
#' looping over a set of per-location options files.
#'
#' @param options_file Path to a completed report options `.R` file
#'   produced by `create_options_template()`.
#' @param output_dir Directory where the rendered HTML report should be
#'   saved. Defaults to the current working directory.
#' @param quiet Logical. If `TRUE` (default), suppresses
#'   `rmarkdown::render()` console output. Set to `FALSE` for verbose
#'   rendering output useful during debugging.
#' @param output_file Name of report. It must end with .html. When
#'   `split_by_location = TRUE`, the location name is appended to this base
#'   name for each per-location file (e.g. `myreport-Upstate.html`).
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`. If `NULL` (default), the report uses the
#'   default styles returned by `create_plot_styles()` with no arguments.
#' @param eval_config Optional named list of evaluation tuning settings
#'   produced by `create_evaluation_config()`. If `NULL` (default), package
#'   defaults are used.
#' @param plot_export Optional named list of forecast-plot export settings,
#'   normally supplied via `save_forecast_plot()` rather than directly. When
#'   non-`NULL`, the report is not rendered in full: the forecast-plot section
#'   is built and written to the requested file(s), the rest of the report is
#'   skipped, and the saved path(s) are returned instead of an HTML report.
#'   When `NULL` (default), a normal report is generated. Ignored (with a
#'   warning) when combined with `split_by_location = TRUE`.
#' @param split_by_location Logical. If `FALSE` (default), a single report is
#'   produced with all locations behind the in-report geography dropdown. If
#'   `TRUE`, one self-contained report is produced per location: the model
#'   file(s) are filtered to each location and rendered separately, and the
#'   vector of all saved report paths is returned. The set of locations is
#'   taken from the `location` column of the implementation model file (or the
#'   evaluation model file when no implementation file is supplied), matching
#'   the location universe of the combined report.
#' @param split_filename Optional character template controlling where each
#'   per-location report is written when `split_by_location = TRUE`. It is
#'   interpreted relative to `output_dir` and must contain at least one
#'   location placeholder so every report gets a distinct path:
#'   `{location}` is a filesystem-safe label (e.g. `"Pee_Dee"`) and
#'   `{location_name}` is the raw location value (e.g. `"Pee Dee"`). The
#'   template may include subdirectories (created as needed) and must end in
#'   `.html`. When `NULL` (default), the location is appended to the base
#'   filename as `-<Location>`. Ignored (with a warning) when
#'   `split_by_location = FALSE`. Examples:
#'   `"region-covid_19-inpatient-{location}.html"` writes flat files with the
#'   location at the end; `"{location}/report.html"` writes one subfolder per
#'   location.
#' @param location_crosswalk Optional user-supplied mapping from raw location
#'   values to cleaned display names. Raw codes are kept for archived forecast
#'   filenames and the forecast data; the cleaned names are shown throughout
#'   the report (and used in the assembled master data set). May be a path to a
#'   two-column CSV with columns `location` (the raw value, exactly as it
#'   appears in your model files' `location` column) and `clean_name` (the
#'   display name), a `data.frame` with those columns, or `NULL` (default) to
#'   use the package's built-in location lookups. Matching is exact after
#'   trimming whitespace; only listed locations are overridden, and a supplied
#'   name takes precedence over the built-in tables.
#'
#' @return Invisibly returns the path to the rendered HTML report; a character
#'   vector of paths (one per location) when `split_by_location = TRUE`; or --
#'   when `plot_export` is supplied -- the path(s) to the saved plot file(s).
#'
#' @export
generate_report <- function(options_file,
                            output_dir  = getwd(),
                            output_file = NULL,
                            quiet       = TRUE,
                            plot_styles = NULL,
                            eval_config = NULL,
                            plot_export = NULL,
                            split_by_location = FALSE,
                            split_filename = NULL,
                            location_crosswalk = NULL) {
#------------------------------------------------------------------------------#
# Validating the function inputs -----------------------------------------------
#------------------------------------------------------------------------------#
# About: Confirms the options file exists and the output directory is usable   #
# before doing any further work.                                               #
#------------------------------------------------------------------------------#

  ###########################
  # Options file must exist #
  ###########################
  if(missing(options_file) || is.null(options_file) || is.na(options_file)){

    # Stopping if no options file is provided
    stop(
      "No options file provided.\n\n",
      "Create a template with:\n",
      "  create_options_template()\n\n",
      "Fill in the template, then call:\n",
      "  generate_report('path/to/report_options_template.R')",
      call. = FALSE
    )

  }

  ############################################
  # Option file path must be found on system #
  ############################################
  if(!file.exists(options_file)){

    # Stopping if options file can not be found
    stop(
      "Options file not found: ", options_file, "\n\n",
      "Check the path and try again. To create a new template:\n",
      "  create_options_template()",
      call. = FALSE
    )

  }

  ##############################
  # split_by_location is a flag #
  ##############################
  if(!is.logical(split_by_location) || length(split_by_location) != 1L ||
     is.na(split_by_location)){

    # Stopping if the flag is malformed
    stop(
      "`split_by_location` must be a single TRUE or FALSE.",
      call. = FALSE
    )

  }

  ###################################################
  # split_filename is an optional naming template   #
  ###################################################
  # About: When provided, it is the filename template used for each per-       #
  # location report (relative to output_dir). It must contain a location       #
  # placeholder -- {location} (filesystem-safe label) and/or {location_name}   #
  # (the raw location value) -- so each report gets a distinct path, it may    #
  # include subdirectories, and it must end in .html. Only meaningful when     #
  # split_by_location = TRUE.                                                   #
  if(!is.null(split_filename)){

    # Must be a single, non-NA string
    if(!is.character(split_filename) || length(split_filename) != 1L ||
       is.na(split_filename)){
      stop(
        "`split_filename` must be a single character string (or NULL).",
        call. = FALSE
      )
    }

    # Must carry a location placeholder or every report overwrites the last
    if(!grepl("\\{location\\}", split_filename, fixed = FALSE) &&
       !grepl("\\{location_name\\}", split_filename, fixed = FALSE)){
      stop(
        "`split_filename` must contain a location placeholder so each report\n",
        "gets its own path. Use `{location}` (a filesystem-safe label) and/or\n",
        "`{location_name}` (the raw location value), for example:\n",
        "  split_filename = 'region-covid_19-inpatient-{location}.html'\n",
        "  split_filename = '{location}/report.html'",
        call. = FALSE
      )
    }

    # Must end in .html
    if(!grepl("\\.html?$", split_filename, ignore.case = TRUE)){
      stop(
        "`split_filename` must end in .html\n",
        "  Got: '", split_filename, "'",
        call. = FALSE
      )
    }

    # A template only makes sense in split mode
    if(!isTRUE(split_by_location)){
      warning(
        "`split_filename` is ignored because `split_by_location = FALSE`.",
        call. = FALSE
      )
    }

  }

  ###################################
  # Output directory must be usable #
  ###################################
  if(!dir.exists(output_dir)){

    # Trying to create user-provided directory
    dir_created <- tryCatch({

      # Creating the directory
      dir.create(output_dir, recursive = TRUE)

      # Changing to true
      TRUE

    #######################################
    # Returning false if there are errors #
    #######################################
    }, error = function(e) FALSE)

    #######################################
    # Triggering if an error occurs above #
    #######################################
    if(!dir_created){

      # Stopping script if error occurs
      stop(
        "Output directory does not exist and could not be created:\n  ",
        output_dir, "\n\n",
        "Check the path or create the directory manually.",
        call. = FALSE
      )
    }

  }

    #######################
    # Resolve plot_styles #
    #######################

    # If no plot_styles provided, use the defaults from create_plot_styles().
    # If provided, it must be a list (i.e., a valid create_plot_styles() output).
    if(is.null(plot_styles)){

      # Use package defaults
      plot_styles <- forecastEvalReport::create_plot_styles()

    # Triggers if user-provided plotly style input has issues
    }else if(!is.list(plot_styles)){

      # Stopping script and returning error to users
      stop(
        "`plot_styles` must be a named list produced by `create_plot_styles()`, ",
        "but received: ", class(plot_styles)[1], ".\n\n",
        "To use default styles, omit the argument or pass NULL.\n",
        "To customize, use:\n",
        "  generate_report(..., plot_styles = create_plot_styles(...))",
        call. = FALSE
      )

    }

    #######################
    # Resolve eval_config #
    #######################

    # If no eval_config provided, use the defaults from create_evaluation_config().
    # If provided, it must be a list (i.e., a valid create_evaluation_config() output).
    if(is.null(eval_config)){

      # Use package defaults
      eval_config <- forecastEvalReport::create_evaluation_config()

      # Triggers if user-provided evaluation config input has issues
    }else if(!is.list(eval_config)){

      # Stopping script and returning error to users
      stop(
        "`eval_config` must be a named list produced by `create_evaluation_config()`, ",
        "but received: ", class(eval_config)[1], ".\n\n",
        "To use default settings, omit the argument or pass NULL.\n",
        "To customize, use:\n",
        "  generate_report(..., eval_config = create_evaluation_config(...))",
        call. = FALSE
      )

    }

#------------------------------------------------------------------------------#
# Read the options file --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Sources the options file in a clean environment so that               #
# `report_options` is available without polluting the user's global            #
# environment. Only `report_options` needs to exist in that environment.       #
#------------------------------------------------------------------------------#

  ###################################
  # Source into a clean environment #
  ###################################

  # Creating an empty environment
  opts_env <- new.env(parent = baseenv())

  ###################################
  # Trying to load the options file #
  ###################################
  tryCatch(

    # Sourcing the options file
    source(options_file, local = opts_env),

    #############################################################
    # Triggers if an error occurs with loading the options file #
    #############################################################
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

    # Stopping script if options file is not correctly formatted
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

  ##########################################################
  # Extracting the options parameters from the environment #
  ##########################################################
  opts <- get("report_options", envir = opts_env)

  ####################################################
  # Triggering error if issue with extraction occurs #
  ####################################################
  if(!is.list(opts)){

    # Stopping script if an error occurs
    stop(
      "`report_options` in the options file must be a list, but found: ",
      class(opts)[1], ".\n\n",
      "Re-create the template with create_options_template() if needed.",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Inject internal fields into opts ---------------------------------------------
#------------------------------------------------------------------------------#
# About: Some fields are not user-supplied but must travel through the params  #
# channel into the RMarkdown. These are injected here BEFORE                   #
# validate_report_params() runs so they flow into the config list correctly.   #
#                                                                              #
# Fields injected:                                                             #
#                                                                              #
#   output.dir   - directory containing the options file; used by extraction   #
#                  functions to write Forecasts/ and Data/ folders relative    #
#                  to the user's working location rather than inst/reports/    #
#   plot.styles  - resolved styles list; passed through params so the          #
#                  RMarkdown does not need to call create_plot_styles() again  #
#------------------------------------------------------------------------------#

  ##################################
  # Injecting the output directory #
  ##################################
  opts$output.dir <- normalizePath(dirname(options_file))

  #############################
  # Injecting the plot styles #
  #############################
  opts$plot.styles <- plot_styles

  ###################################
  # Injecting the evaluation config #
  ###################################
  opts$eval.config <- eval_config

  #######################################
  # Resolve + inject location crosswalk #
  #######################################
  # About: Resolve the optional location crosswalk (path, data.frame, or named
  # vector) into a named "raw -> clean" vector now, so it travels through the
  # params channel as a self-contained object with no file dependency at render
  # time. A malformed crosswalk stops here with an actionable message. A value
  # in the options file (opts$location.crosswalk) is used when the argument is
  # not supplied.
  loc_xwalk_input <- if(!is.null(location_crosswalk)){
    location_crosswalk
  }else{
    opts$location.crosswalk
  }

  resolved_loc_xwalk <- tryCatch(
    read_location_crosswalk(loc_xwalk_input),
    error = function(e){
      stop(
        "`location_crosswalk` could not be read.\n\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Store as a named vector, or NA when none was supplied
  opts$location.crosswalk <- if(is.null(resolved_loc_xwalk)){
    NA
  }else{
    resolved_loc_xwalk
  }

#------------------------------------------------------------------------------#
# Check variables.crosswalk.file is present and not NA -------------------------
#------------------------------------------------------------------------------#
# About: The variables crosswalk file is required for report generation.       #
# We enforce its presence explicitly here before passing to                    #
# validate_report_params so the error message is immediately actionable.       #
#------------------------------------------------------------------------------#

  ########################################
  # variables.crosswalk.file is required #
  ########################################

  # Checking if the cross walk is present
  cwf <- opts$variables.crosswalk.file

  # Triggering if the cross walk is not present
  if(is.null(cwf) || (length(cwf) == 1L && is.na(cwf)) ||
     nchar(trimws(as.character(cwf))) == 0L){

    # Stopping script and returning error if no cross walk is present
    stop(
      "`variables.crosswalk.file` is required but was not provided.\n\n",
      "Steps to resolve:\n",
      "  1. Run build_crosswalk_from_options() to create the seeded crosswalk:\n",
      "       build_crosswalk_from_options('", options_file, "')\n",
      "  2. Open the crosswalk file and complete all required fields:\n",
      "       clean_name_full, clean_name_abb, on_right_axis,\n",
      "       convert_percent, definition, binning, cohort, file\n",
      "  3. Add the completed crosswalk file path to your options file:\n",
      "       variables.crosswalk.file = 'path/to/your_crosswalk.csv'\n",
      "  4. Re-run generate_report().",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Validate report parameters ---------------------------------------------------
#------------------------------------------------------------------------------#
# About: Passes the options list to validate_report_params() to confirm all    #
# required fields are present, file paths exist, and enums are valid. Any      #
# validation failure is caught and re-presented with actionable context.       #
#------------------------------------------------------------------------------#

  ########################################
  # Trying to run validate_report_params #
  ########################################
  config <- tryCatch(

    # Validating the report parameters
    forecastEvalReport:::validate_report_params(opts, verbose = FALSE),

    ########################################
    # Triggering error if validation fails #
    ########################################
    error = function(e){

      # Stopping script and returning an error
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
# Build the output filename ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Constructs the HTML output filename from the contact name, disease,   #
# reason, and model type stored in the validated config, using the same        #
# sanitation pattern as build_variables_crosswalk().                           #
#------------------------------------------------------------------------------#

  ###################
  # Sanitize helper #
  ###################
  sanitize <- function(x) gsub(" ", "_", trimws(as.character(x)))

  ######################
  # Construct filename #
  ######################
  if(is.null(output_file)){

    # File name
    output_filename <- paste0(

      # Contact name
      sanitize(config$contact_name), "-",

      # Disease name
      sanitize(config$disease), "-",

      # Reason name
      sanitize(config$reason), "-",

      # General model type
      sanitize(config$general_model_type),

      # Extension
      ".html"
    )

  }else{output_filename <- output_file}

  ##########################
  # Creating the file path #
  ##########################
  output_path <- file.path(output_dir, output_filename)

#------------------------------------------------------------------------------#
# Locate the RMarkdown template ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Finds the bundled RMarkdown template using system.file() so the       #
# path works regardless of where the package is installed on the user's        #
# machine.                                                                     #
#------------------------------------------------------------------------------#

  #####################
  # Find the template #
  #####################
  template_path <- system.file(
    "reports", "forecast_evaluation.Rmd",
    package = "forecastEvalReport"
  )

  ########################################################
  # Triggering if the report template could not be found #
  ########################################################
  if(nchar(template_path) == 0L){

    # Stopping the script if an error occurs
    stop(
      "The report template could not be found within the forecastEvalReport\n",
      "package installation.\n\n",
      "This is an internal package error. Try re-installing the package:\n",
      "  devtools::install()\n\n",
      "If the problem persists, contact the package maintainer.",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Internal single-report renderer ----------------------------------------------
#------------------------------------------------------------------------------#
# About: Renders one report to `dest_path` from the supplied params list.      #
# Rendering goes into a clean, space-free temp directory and the finished HTML #
# is copied to the (possibly spaced / cloud-synced) destination. Render errors #
# are translated into actionable messages. Used by both the normal single-     #
# report path and the per-location split path so the render logic lives once.  #
#------------------------------------------------------------------------------#

  render_one <- function(params_local, dest_path){

    #####################
    # Render the report #
    #####################
    tryCatch(

      {

        #######################################################################
        # Render into a clean temp directory, then copy the finished file to  #
        # output_dir. Rendering straight into a cloud-synced path with spaces #
        # (Box/OneDrive) makes pandoc misplace the output -- creating a folder #
        # named after output_file with a pandoc-default file inside. Pandoc    #
        # only ever touches the space-free temp path; file.copy() handles the  #
        # final (spaced) Box path reliably.                                    #
        #######################################################################
        tmp_dir <- tempfile("report_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

        # Render to the temp location
        tmp_out <- rmarkdown::render(
          input             = template_path,
          output_file       = basename(dest_path),
          output_dir        = tmp_dir,
          intermediates_dir = tmp_dir,
          params            = params_local,
          quiet             = quiet,
          envir             = new.env(parent = globalenv())
        )

        # Copy the finished HTML to the real destination
        copied <- file.copy(tmp_out, dest_path, overwrite = TRUE)
        if(!isTRUE(copied)){
          stop(
            "The report rendered successfully but could not be copied to:\n  ",
            dest_path, "\n\n",
            "Check that the destination folder is writable.",
            call. = FALSE
          )
        }

      },

      ##############################################
      # Error trigger if report could not generate #
      ##############################################
      error = function(e){

        ############################
        # Message to show to users #
        ############################
        msg <- conditionMessage(e)

        ####################################################
        # Known pattern: validation failure inside a chunk #
        ####################################################
        if(grepl("validation failed", msg, ignore.case = TRUE)){

          # Stopping the script if an error occurs
          stop(
            "Report generation failed during data validation.\n\n",
            "A validator inside the report rejected one of your data files.\n",
            "Review the error below and check the relevant file.\n",
            "Re-run with quiet = FALSE for the full rendering log:\n",
            "  generate_report('", options_file, "', quiet = FALSE)\n\n",
            msg,
            call. = FALSE
          )

        }

        ##################################################
        # Known pattern: file not found during rendering #
        ##################################################
        if(grepl("cannot open", msg, ignore.case = TRUE) ||
           grepl("No such file", msg, ignore.case = TRUE)){

          # Stopping the script if an error occurs
          stop(
            "Report generation failed because a file could not be found.\n\n",
            "Check that all file paths in your options file are correct and\n",
            "that the files exist on disk:\n",
            "  ", options_file, "\n\n",
            "R error: ", msg,
            call. = FALSE
          )

        }

        ###########################################################
        # Known pattern: object not found (chunk reference error) #
        ###########################################################
        if(grepl("object .* not found", msg, ignore.case = TRUE)){

          # Stopping the script if an error occurs
          stop(
            "Report generation failed due to a missing object in the report template.\n\n",
            "A chunk in the report references a variable that was not created by a\n",
            "prior chunk. This may indicate a validator failed silently upstream.\n",
            "Re-run with quiet = FALSE for the full rendering log:\n",
            "  generate_report('", options_file, "', quiet = FALSE)\n\n",
            "R error: ", msg,
            call. = FALSE
          )

        }

        ########################################
        # Known pattern: package not available #
        ########################################
        if(grepl("there is no package", msg, ignore.case = TRUE)){

          # Stopping the script if an error occurs
          stop(
            "Report generation failed because a required package is not installed.\n\n",
            "Install any missing packages and try again.\n\n",
            "R error: ", msg,
            call. = FALSE
          )

        }

        ###########################
        # Fallback: unknown error #
        ###########################
        stop(
          "Report generation failed with an unexpected error.\n\n",
          "Re-run with quiet = FALSE to see the full rendering log:\n",
          "  generate_report('", options_file, "', quiet = FALSE)\n\n",
          "R error: ", msg,
          call. = FALSE
        )

      }
    )

    # Returning the finished path
    invisible(dest_path)

  }

#------------------------------------------------------------------------------#
# Plot-only export path --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: When plot_export is supplied, the full report is not rendered. The    #
# export options are injected into the params channel; the forecast-plot       #
# chunk in the template detects them, writes the requested HTML/image, records #
# the written path(s), and calls knit_exit() so no later sections run. The     #
# partial render goes to a temp directory and is discarded; the saved path(s)  #
# are read back from the render environment and returned.                      #
#------------------------------------------------------------------------------#

  #############################################
  # Branching only when an export is requested #
  #############################################
  if(!is.null(plot_export)){

    ###################################################################
    # plot_export already handles per-geography output on its own, so #
    # split_by_location does not apply here. Warn and continue.       #
    ###################################################################
    if(isTRUE(split_by_location)){
      warning(
        "`split_by_location` is ignored when `plot_export` is supplied; ",
        "the plot export already writes one file per geography.",
        call. = FALSE
      )
    }

    # Injecting the export request into the params channel
    opts$plot.export <- plot_export

    # Render environment retained so written paths can be read back
    plot_env <- new.env(parent = globalenv())

    # Temp render target; the partial HTML output is discarded
    tmp_dir <- tempfile("forecast_plot_")
    dir.create(tmp_dir)
    on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

    ##############################################
    # Rendering only far enough to save the plot #
    ##############################################
    tryCatch(

      # Rendering the template in plot-only mode
      rmarkdown::render(
        input             = template_path,
        output_file       = "forecast_plot_export.html",
        output_dir        = tmp_dir,
        intermediates_dir = tmp_dir,
        params            = opts,
        quiet             = quiet,
        envir             = plot_env
      ),

      ###############################################
      # Translating render errors into a clear note #
      ###############################################
      error = function(e){

        # Stopping with an actionable message
        stop(
          "Forecast plot export failed while running the report pipeline.\n\n",
          "Re-run with quiet = FALSE to see the full log:\n",
          "  save_forecast_plot('", options_file, "', quiet = FALSE)\n\n",
          "R error: ", conditionMessage(e),
          call. = FALSE
        )

      }
    )

    # Reading the written path(s) back out of the render environment
    saved_plot_paths <- get0("saved_plot_paths", envir = plot_env,
                             ifnotfound = character())

    ##################################################
    # Warning if the template produced no plot files #
    ##################################################
    if(length(saved_plot_paths) == 0){

      # Most likely no data was available to plot
      warning(
        "No forecast plot was produced. The supplied inputs may not contain ",
        "data to plot.",
        call. = FALSE
      )

    }

    ############################
    # Message to show to users #
    ############################
    message(
      "\n",
      "================================================================================\n",
      "  \u2713  Forecast plot(s) saved\n",
      "================================================================================\n",
      paste0("    ", saved_plot_paths, "\n", collapse = ""),
      "================================================================================\n"
    )

    # Returning the saved plot path(s) instead of a report
    return(invisible(saved_plot_paths))

  }

#------------------------------------------------------------------------------#
# Per-location split path ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: When split_by_location is TRUE, one report is produced per location.  #
# The implementation and evaluation model files are read once, the set of      #
# locations is taken from the implementation file (falling back to the         #
# evaluation file), and for each location the model file(s) are filtered to    #
# that location, written to a temporary directory, and rendered to their own   #
# HTML file. Only the two model files are filtered -- the outcome/truth data   #
# is filtered downstream by assemble_report_data() to whatever locations the   #
# (now single-location) model files contain, so the whole report becomes       #
# single-location automatically. The vector of saved paths is returned.        #
#------------------------------------------------------------------------------#

  if(isTRUE(split_by_location)){

    ###############################################################
    # Reader: read every column as character so the per-location  #
    # round-trip is lossless. This preserves zero-padded FIPS      #
    # codes (e.g. "01" is not turned into the integer 1) AND the   #
    # full text of forecast values (a numeric read + write.csv     #
    # would truncate to getOption("digits") significant digits).   #
    # Downstream validators re-read and type-convert as usual.     #
    ###############################################################
    read_model_file <- function(path){

      # Peek at the header to learn the column names
      header <- names(utils::read.csv(
        path, nrows = 1, check.names = FALSE, stringsAsFactors = FALSE
      ))

      # The location column is required to split
      if(!"location" %in% header){
        stop(
          "`split_by_location = TRUE` requires a `location` column, but the\n",
          "model file has none:\n  ", path, "\n\n",
          "Use split_by_location = FALSE for this file, or add a `location`\n",
          "column identifying each geography.",
          call. = FALSE
        )
      }

      # Read the full file as character for an exact, lossless round-trip
      utils::read.csv(
        path, check.names = FALSE, stringsAsFactors = FALSE,
        colClasses = "character"
      )

    }

    #############################################
    # Resolve the model file paths from config  #
    #############################################
    impl_file <- config$implementation_model_file
    eval_file <- config$evaluation_model_file

    # Presence flags (validate_report_params sets absent files to NA)
    impl_present <- !is.null(impl_file) && !is.na(impl_file)
    eval_present <- !is.null(eval_file) && !is.na(eval_file)

    #################################
    # Read whichever files exist     #
    #################################
    impl_df <- if(impl_present) read_model_file(impl_file) else NULL
    eval_df <- if(eval_present) read_model_file(eval_file) else NULL

    ###################################################################
    # Determine the location universe: implementation first, else      #
    # evaluation. This matches the combined report, which draws its     #
    # location set from the implementation model when it is present.    #
    ###################################################################
    primary_df <- if(!is.null(impl_df)) impl_df else eval_df

    if(is.null(primary_df)){
      stop(
        "`split_by_location = TRUE` needs at least one model file, but neither\n",
        "an implementation nor an evaluation model file was provided.",
        call. = FALSE
      )
    }

    # Unique, non-blank locations, alphabetically ordered
    locations <- sort(unique(primary_df$location[
      !is.na(primary_df$location) & nzchar(trimws(primary_df$location))
    ]))

    if(length(locations) == 0L){
      stop(
        "No locations were found in the `location` column of the model file.\n",
        "Cannot split by location.",
        call. = FALSE
      )
    }

    #########################################################
    # Temp workspace for the per-location filtered CSV files #
    #########################################################
    split_dir <- tempfile("report_split_")
    dir.create(split_dir)
    on.exit(unlink(split_dir, recursive = TRUE), add = TRUE)

    ################################################################
    # Filesystem-safe location label for filenames (matches the    #
    # sanitizer used by the plot-export per-geography writer).      #
    ################################################################
    safe_label <- function(x){
      gsub("^_+|_+$", "", gsub("[^A-Za-z0-9._-]+", "_", x))
    }

    # Base filename (without the .html extension) for per-location naming
    base_no_ext <- sub("\\.html?$", "", output_filename, ignore.case = TRUE)

    ###########################
    # Pre-render notification #
    ###########################
    message(
      "\n",
      "================================================================================\n",
      "  Generating ", length(locations), " per-location report(s)...\n",
      "================================================================================\n",
      "  Template:   ", template_path, "\n",
      "  Output dir: ", output_dir, "\n",
      "  Reason:     ", config$reason, "\n",
      "  Disease:    ", config$disease, "\n",
      "  Model:      ", config$general_model_type, "\n",
      "  Locations:  ", paste(locations, collapse = ", "), "\n",
      "================================================================================\n"
    )

    ##############################
    # Loop over every location    #
    ##############################
    saved_report_paths <- character(0)

    for(loc in locations){

      # Clone the base params for this location
      opts_loc <- opts

      # Filesystem-safe label for this location
      safe <- safe_label(loc)

      ############################################
      # Filter + write the implementation model   #
      ############################################
      if(!is.null(impl_df)){

        sub_impl <- impl_df[impl_df$location == loc, , drop = FALSE]

        if(nrow(sub_impl) > 0L){
          impl_path <- file.path(split_dir, paste0("impl_", safe, ".csv"))
          utils::write.csv(sub_impl, impl_path, row.names = FALSE)
          opts_loc$implementation.model.file <- impl_path
        }else{
          # No implementation rows for this location -> treat as absent
          opts_loc$implementation.model.file <- NA
        }

      }

      #########################################
      # Filter + write the evaluation model    #
      #########################################
      if(!is.null(eval_df)){

        sub_eval <- eval_df[eval_df$location == loc, , drop = FALSE]

        if(nrow(sub_eval) > 0L){
          eval_path <- file.path(split_dir, paste0("eval_", safe, ".csv"))
          utils::write.csv(sub_eval, eval_path, row.names = FALSE)
          opts_loc$evaluation.model.file <- eval_path
        }else{
          # No evaluation rows for this location -> treat as absent
          opts_loc$evaluation.model.file <- NA
        }

      }

      #####################################################
      # Skip the location if neither file has rows for it  #
      #####################################################
      impl_ok <- !is.null(opts_loc$implementation.model.file) &&
        !is.na(opts_loc$implementation.model.file)
      eval_ok <- !is.null(opts_loc$evaluation.model.file) &&
        !is.na(opts_loc$evaluation.model.file)

      if(!impl_ok && !eval_ok){
        warning(
          "Skipping location '", loc, "': no rows in any model file.",
          call. = FALSE
        )
        next
      }

      ################################
      # Per-location output filename  #
      ################################
      # When a split_filename template is supplied, substitute the location    #
      # placeholders into it; otherwise fall back to the default of appending   #
      # "-<Location>" to the base name. {location} is the filesystem-safe       #
      # label; {location_name} is the raw location value.                       #
      if(!is.null(split_filename)){

        loc_filename <- gsub("{location_name}", loc,  split_filename, fixed = TRUE)
        loc_filename <- gsub("{location}",      safe, loc_filename,   fixed = TRUE)

      }else{

        loc_filename <- paste0(base_no_ext, "-", safe, ".html")

      }

      # Resolve to a full path under output_dir; the template may include
      # subdirectories, so create the parent folder when needed.
      loc_path   <- file.path(output_dir, loc_filename)
      loc_parent <- dirname(loc_path)
      if(!dir.exists(loc_parent)){
        dir.create(loc_parent, recursive = TRUE, showWarnings = FALSE)
      }

      # Per-location progress line
      message("  ", loc, "  ->  ", loc_filename)

      # Render this location's report
      render_one(opts_loc, loc_path)

      # Record the saved path
      saved_report_paths <- c(saved_report_paths, loc_path)

    }

    ############################
    # Message to show to users #
    ############################
    message(
      "\n",
      "================================================================================\n",
      "  \u2713  ", length(saved_report_paths), " per-location report(s) generated\n",
      "================================================================================\n",
      "  Saved to:\n",
      paste0("    ", saved_report_paths, "\n", collapse = ""),
      "================================================================================\n"
    )

    # Returning the vector of saved report paths
    return(invisible(saved_report_paths))

  }

#------------------------------------------------------------------------------#
# Render the report (combined, all locations) ----------------------------------
#------------------------------------------------------------------------------#
# About: The default path. Renders a single report containing every location   #
# behind the in-report geography dropdown.                                     #
#------------------------------------------------------------------------------#

  ###########################
  # Pre-render notification #
  ###########################

  # Message shows to user prior to rendering
  message(
    "\n",
    "================================================================================\n",
    "  Generating report...\n",
    "================================================================================\n",
    "  Template:   ", template_path, "\n",
    "  Output:     ", output_path, "\n",
    "  Reason:     ", config$reason, "\n",
    "  Disease:    ", config$disease, "\n",
    "  Model:      ", config$general_model_type, "\n",
    "================================================================================\n"
  )

  #####################
  # Render the report #
  #####################
  render_one(opts, output_path)

#------------------------------------------------------------------------------#
# Confirm completion and return ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Prints a success message with the output file path and returns the    #
# path invisibly so it can be captured and used in pipelines if needed.        #
#------------------------------------------------------------------------------#

  ############################
  # Message to show to users #
  ############################
  message(
    "\n",
    "================================================================================\n",
    "  \u2713  Report generated successfully\n",
    "================================================================================\n",
    "  Saved to:\n",
    "    ", output_path, "\n",
    "================================================================================\n"
  )

  ######################
  # Hiding output path #
  ######################
  invisible(output_path)

}
