#' Save the forecast plot without rendering the full report
#'
#' Builds and saves the interactive forecast figure for a set of report inputs
#' without producing the full evaluation report. This is for users who want
#' only the forecast plot -- as an interactive HTML file, or as a static PNG,
#' JPEG, or PDF image -- rather than the complete document.
#'
#' It takes the same completed options file as `generate_report()` and runs the
#' identical data-assembly pipeline, but stops as soon as the forecast plot has
#' been written: no master-data CSV is saved and none of the downstream report
#' sections are produced. A static export captures the same plot the report
#' shows -- floating side legend, full data range, and no range slider -- as an
#' image.
#'
#' Static formats (PNG, JPEG, PDF) require \pkg{webshot2} (headless Chrome): the
#' image is a screenshot of the rendered plot, so it matches the report exactly.
#' HTML export needs no backend.
#'
#' @param options_file Path to a completed report options `.R` file produced by
#'   `create_options_template()` -- the same file `generate_report()` uses.
#' @param file Where to save the plot: a full file path (e.g.
#'   `"exports/upstate.pdf"`) or a directory. When a directory is given, or when
#'   every geography is saved, files are auto-named `forecast_plot_<location>`.
#'   Defaults to the current working directory.
#' @param format Output format: one of `"html"`, `"png"`, `"jpeg"` (alias
#'   `"jpg"`), `"pdf"`, or `"tiff"` (alias `"tif"`). If omitted, inferred from
#'   the `file` extension when possible, otherwise `"html"`. TIFF is written
#'   lossless at 300 dpi (needs the `magick` package).
#' @param plot Which figure to export: `"forecast"` (default) for the main
#'   forecast plot, or `"consistency"` for the forecast-consistency plot.
#' @param location Which geography to export: a display name (e.g. `"Upstate"`)
#'   or a positive integer index into the alphabetically-ordered geographies.
#'   `NULL` (default) exports every geography. For static formats, multiple
#'   geographies are written one file each into the directory given by `file`;
#'   for HTML, a single geography is rendered as its own page, and `NULL`
#'   produces the usual all-geographies page with the dropdown.
#' @param width,height Viewport size in pixels for a static capture. `NULL`
#'   (default) uses 1200 x 750.
#' @param scale Resolution multiplier for a static capture (passed to
#'   \pkg{webshot2} as `zoom`). Higher values give sharper images. Defaults
#'   to `1`.
#' @param delay Seconds \pkg{webshot2} waits before capturing a static image,
#'   so the plot can finish drawing and the page can expand to the side-legend
#'   view. `NULL` (default) picks the value automatically: a longer wait for
#'   `plot = "consistency"` (its many overlaid forecasts take longer to render)
#'   and the standard wait for the main forecast plot. Raise it if a static
#'   export comes out blank.
#' @param n_forecasts Integer. For `plot = "consistency"` only: how many of the
#'   most-recent forecasts to load and show in the export. A static image
#'   cannot toggle forecasts on, so this is exactly what appears. Defaults to
#'   `5`.
#' @param forecast_every Optional positive integer. For `plot = "consistency"`
#'   only: instead of the `n_forecasts` most-recent forecasts shown
#'   consecutively, overlay every Xth forecast across the view window, starting
#'   at the beginning of that window (`view_start` when supplied, otherwise the
#'   earliest forecast). Every qualifying forecast on the stride is shown -- the
#'   view window bounds how many appear, and `n_forecasts` does not cap stride
#'   mode. `NULL` (default) keeps the consecutive most-recent behavior. For
#'   example, with a Jan--Apr window, `forecast_every = 4` shows forecasts 1, 5,
#'   9, ... from the window start through its end.
#' @param end_cushion Optional positive integer used only with `forecast_every`
#'   (stride mode). Drops the most-recent N forecasts from the window before
#'   striding, so the stride stops short of the very last forecast(s) -- useful
#'   when you don't want the latest forecast included. `NULL` (default) keeps
#'   them.
#' @param font_size Optional single positive number giving the base font size
#'   (points) for the whole figure -- axis titles, tick labels, the legend, and
#'   hover. `NULL` (default) keeps the configured plot styling.
#' @param tick_size Optional single positive number setting the axis tick-label
#'   (the axis numbers/dates) size in points, independent of `font_size` -- use
#'   it to enlarge the tick labels without changing the axis titles. For
#'   `plot = "consistency"` only. `NULL` (default) follows `font_size`.
#' @param forecast_size Optional single positive number giving the forecast
#'   median line width. For `plot = "consistency"` only. `NULL` (default) uses
#'   the built-in width.
#' @param forecast_color Optional single color (e.g. `"#1f77b4"` or
#'   `"firebrick"`) applied to every forecast median line and its markers. For
#'   `plot = "consistency"` only. Overrides the recency-based coloring (the
#'   solid-newest / dashed-older distinction is kept); the prediction-interval
#'   bands keep their styling. `NULL` (default) keeps the recency colors.
#' @param observed_size Optional single positive number giving the observed
#'   (truth) line width. For `plot = "consistency"` only. `NULL` (default) uses
#'   the built-in width.
#' @param observed_color Optional single color applied to the observed (truth)
#'   line. For `plot = "consistency"` only. `NULL` (default) keeps the built-in
#'   color.
#' @param aux_size Optional single positive number giving the auxiliary-variable
#'   line width (secondary axis). For `plot = "consistency"` only. `NULL`
#'   (default) uses the built-in width.
#' @param aux_color Optional color or vector of colors for the auxiliary-variable
#'   lines (secondary axis). A single value colors every aux line; a vector acts
#'   as a palette, cycled across the aux lines in order. For
#'   `plot = "consistency"` only. `NULL` (default) keeps the built-in colors.
#' @param title_gap Optional single positive number giving the standoff in
#'   pixels between the axis tick labels (the numbers) and the axis title --
#'   useful when large fonts make them crowd. For `plot = "consistency"` only.
#'   `NULL` (default) uses plotly's automatic spacing.
#' @param view_start,view_end Optional `Date`s (or `"YYYY-MM-DD"` strings)
#'   giving the initial visible x-window. The data is unchanged -- only the
#'   opening range differs -- so all of it remains pannable. Supply both or
#'   neither; `NULL` (default) shows the full default window.
#' @param legend Logical. For a static image export, draw the side legend
#'   panel? `TRUE` (default) reserves a right-hand column for it, matching the
#'   report; `FALSE` hides it and lets the plot fill the full capture width.
#'   HTML exports keep their interactive (collapsible) legend regardless.
#' @param overwrite Logical. Overwrite existing files? Defaults to `TRUE`.
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`. If `NULL` (default), package defaults are used.
#' @param eval_config A named list produced by `create_evaluation_config()`.
#'   If `NULL` (default), package defaults are used.
#' @param quiet Logical. If `TRUE` (default), suppresses rendering console
#'   output. Set to `FALSE` for verbose output useful when debugging.
#'
#' @return Invisibly returns the path(s) to the saved plot file(s).
#'
#' @examples
#' \dontrun{
#'   # Interactive HTML of every geography, into a folder
#'   save_forecast_plot("report_options.R", file = "exports/")
#'
#'   # One geography as a high-resolution PDF
#'   save_forecast_plot(
#'     "report_options.R",
#'     file     = "exports/upstate.pdf",
#'     location = "Upstate",
#'     width    = 1400,
#'     height   = 800
#'   )
#'
#'   # All geographies as sharp JPEGs
#'   save_forecast_plot("report_options.R", file = "exports/",
#'                      format = "jpeg", scale = 2)
#' }
#'
#' @export
save_forecast_plot <- function(options_file,
                               file        = NULL,
                               format      = c("html", "png", "jpeg",
                                               "jpg", "pdf", "tiff", "tif"),
                               plot        = c("forecast", "consistency"),
                               location    = NULL,
                               width       = NULL,
                               height      = NULL,
                               scale       = 1,
                               delay       = NULL,
                               n_forecasts = 5L,
                               forecast_every = NULL,
                               end_cushion = NULL,
                               font_size   = NULL,
                               tick_size      = NULL,
                               forecast_size  = NULL,
                               forecast_color = NULL,
                               observed_size  = NULL,
                               observed_color = NULL,
                               aux_size       = NULL,
                               aux_color      = NULL,
                               title_gap      = NULL,
                               view_start  = NULL,
                               view_end    = NULL,
                               legend      = TRUE,
                               overwrite   = TRUE,
                               plot_styles = NULL,
                               eval_config = NULL,
                               quiet       = TRUE) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Confirms an options file was supplied before any work begins. The     #
# deeper validation (file exists, required fields, valid paths) is handled by  #
# generate_report(), which this function calls, so it is not duplicated here.  #
#------------------------------------------------------------------------------#

  ###########################
  # Options file must exist #
  ###########################
  if(missing(options_file) || is.null(options_file) || is.na(options_file)){

    # Stopping if no options file is provided
    stop(
      "No options file provided.\n\n",
      "save_forecast_plot() takes the same completed options file as\n",
      "generate_report(). Create one with create_options_template().",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Resolving the output format --------------------------------------------------
#------------------------------------------------------------------------------#
# About: Picks the export format. When the caller did not name a format but a  #
# file path carries a recognized extension, that extension wins; otherwise the #
# default applies. The jpg alias is folded into jpeg.                          #
#------------------------------------------------------------------------------#

  ###############################
  # Inferring from a file path  #
  ###############################
  if(missing(format) && !is.null(file)){
    fe <- tolower(tools::file_ext(file))
    if(fe %in% c("html", "png", "jpeg", "jpg", "pdf", "tiff", "tif")){
      format <- fe
    }
  }

  ###############################
  # Validating and normalizing  #
  ###############################

  # Validating the format
  format <- match.arg(format)

  # Validating the plot target (forecast vs consistency)
  plot <- match.arg(plot)

  # Folding the jpg alias into jpeg
  if(identical(format, "jpg")) format <- "jpeg"

  # Folding the tif alias into tiff
  if(identical(format, "tif")) format <- "tiff"

#------------------------------------------------------------------------------#
# Resolving the output location ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Resolves `file` to an absolute path up front. The plot is written     #
# from inside the rendering environment, whose working directory is not the    #
# caller's, so a relative path (or the default) must be anchored to the user's #
# current working directory before being handed off.                           #
#------------------------------------------------------------------------------#

  ###################################
  # Anchoring the output to the cwd #
  ###################################
  if(is.null(file)){

    # Defaulting to the caller's working directory
    file <- getwd()

  }else{

    # Making any user path absolute (handles Box/OneDrive spaced paths)
    file <- normalizePath(file, winslash = "/", mustWork = FALSE)

  }

#------------------------------------------------------------------------------#
# Resolving the figure options -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Validates the optional whole-figure font size and the initial view    #
# window. font_size is a single positive number (points). A view window needs  #
# both ends, which are coerced to Date; the data is unchanged, only the        #
# opening x-range differs.                                                     #
#------------------------------------------------------------------------------#

  ###############################
  # Validating the font size    #
  ###############################
  if(!is.null(font_size)){

    # font_size must be a single positive number
    if(!is.numeric(font_size) || length(font_size) != 1 || font_size <= 0){
      stop("font_size must be a single positive number (points).",
           call. = FALSE)
    }

  }

  ###############################
  # Validating line size args   #
  ###############################
  # tick_size, forecast_size, observed_size, and aux_size are each an optional
  # single positive number. Loop so the message names the offending argument.
  for(.nm in c("tick_size", "forecast_size", "observed_size", "aux_size",
               "title_gap")){
    .v <- get(.nm)
    if(!is.null(.v) && (!is.numeric(.v) || length(.v) != 1 || .v <= 0)){
      stop(.nm, " must be a single positive number.", call. = FALSE)
    }
  }

  ###############################
  # Validating line color args  #
  ###############################
  # forecast_color and observed_color are each an optional single color string.
  for(.nm in c("forecast_color", "observed_color")){
    .v <- get(.nm)
    if(!is.null(.v) && (!is.character(.v) || length(.v) != 1 ||
                        is.na(.v) || !nzchar(.v))){
      stop(.nm, " must be a single non-empty color string ",
           "(e.g. \"#1f77b4\" or \"firebrick\").", call. = FALSE)
    }
  }

  # aux_color may be one color or a vector of colors (a palette) cycled across
  # the auxiliary lines.
  if(!is.null(aux_color) && (!is.character(aux_color) || length(aux_color) < 1 ||
                             any(is.na(aux_color)) || any(!nzchar(aux_color)))){
    stop("aux_color must be one or more non-empty color strings ",
         "(e.g. \"firebrick\" or c(\"#1f77b4\", \"#ff7f0e\")).", call. = FALSE)
  }

  ###############################
  # Validating n_forecasts      #
  ###############################
  # Only the consistency plot uses it: the number of most-recent forecasts to
  # load and show in the export (a static image cannot toggle them on).
  if(identical(plot, "consistency")){

    # n_forecasts must be a single positive whole number
    if(!is.numeric(n_forecasts) || length(n_forecasts) != 1 ||
       n_forecasts < 1 || n_forecasts != round(n_forecasts)){
      stop("n_forecasts must be a single positive whole number.",
           call. = FALSE)
    }

    # Coercing to integer for build_forecast_archive()
    n_forecasts <- as.integer(n_forecasts)

    # forecast_every and end_cushion (stride mode) must be positive whole numbers
    for(.nm in c("forecast_every", "end_cushion")){
      .v <- get(.nm)
      if(!is.null(.v) && (!is.numeric(.v) || length(.v) != 1 || .v < 1 ||
                          .v != round(.v))){
        stop(.nm, " must be a single positive whole number.", call. = FALSE)
      }
    }

  }

  ###############################
  # Resolving the capture delay #
  ###############################
  # A static capture screenshots a rendered page, so it must wait for the plot
  # to draw and the page to expand to the side-legend view. The consistency
  # plot overlays many forecasts and takes longer, so when no delay is given it
  # waits longer than the main forecast plot. A user-supplied delay always wins.
  if(is.null(delay)){

    # Heavier consistency figure gets a longer default wait
    delay <- if(identical(plot, "consistency")) 8 else 4

  }else{

    # An explicit delay must be a single positive number (seconds)
    if(!is.numeric(delay) || length(delay) != 1 || delay <= 0){
      stop("delay must be a single positive number (seconds).", call. = FALSE)
    }

  }

  ###############################
  # Resolving the view window   #
  ###############################

  # A window needs both ends, or neither
  if(is.null(view_start) != is.null(view_end)){
    stop("Provide both view_start and view_end, or neither.", call. = FALSE)
  }

  # Coercing the bounds to Date and sanity-checking the order
  if(!is.null(view_start)){

    # Accept Date or "YYYY-MM-DD" strings
    view_start <- as.Date(view_start)
    view_end   <- as.Date(view_end)

    # Both bounds must parse as dates
    if(is.na(view_start) || is.na(view_end)){
      stop("view_start / view_end must be dates (e.g. \"2025-01-01\").",
           call. = FALSE)
    }

    # The window must run forward
    if(view_end <= view_start){
      stop("view_end must be after view_start.", call. = FALSE)
    }

  }

#------------------------------------------------------------------------------#
# Building the export request --------------------------------------------------
#------------------------------------------------------------------------------#
# About: Packs the export options into a single list. This travels through     #
# generate_report() into the report params as `plot.export`, where the         #
# forecast-plot chunk detects it, writes the plot, and exits early.            #
#------------------------------------------------------------------------------#

  ###############################
  # Assembling the export list  #
  ###############################
  plot_export <- list(
    format      = format,
    plot        = plot,
    file        = file,
    location    = location,
    width       = width,
    height      = height,
    scale       = scale,
    delay       = delay,
    n_forecasts = n_forecasts,
    forecast_every = forecast_every,
    end_cushion = end_cushion,
    font_size   = font_size,
    tick_size      = tick_size,
    forecast_size  = forecast_size,
    forecast_color = forecast_color,
    observed_size  = observed_size,
    observed_color = observed_color,
    aux_size       = aux_size,
    aux_color      = aux_color,
    title_gap      = title_gap,
    view_start  = view_start,
    view_end    = view_end,
    legend      = legend,
    overwrite   = overwrite
  )

#------------------------------------------------------------------------------#
# Running the pipeline in plot-only mode ---------------------------------------
#------------------------------------------------------------------------------#
# About: Hands off to generate_report() with the export request set. In this   #
# mode it runs the same assembly pipeline, writes the plot, skips the report,  #
# and returns the saved path(s). output_dir points at a temp location since    #
# no HTML report is produced.                                                  #
#------------------------------------------------------------------------------#

  #################################
  # Delegating to generate_report #
  #################################
  saved_paths <- generate_report(
    options_file = options_file,
    output_dir   = tempdir(),
    quiet        = quiet,
    plot_styles  = plot_styles,
    eval_config  = eval_config,
    plot_export  = plot_export
  )

  ##############################
  # Returning the saved paths  #
  ##############################
  invisible(saved_paths)

}

#' Save the forecast-consistency plot to a file
#'
#' Convenience wrapper around [save_forecast_plot()] with
#' `plot = "consistency"`. Accepts the same arguments; see that function for
#' full details. Use `n_forecasts` to set how many of the most-recent forecasts
#' the static image shows.
#'
#' @param options_file Path to the completed options file (as for
#'   [save_forecast_plot()] and [generate_report()]).
#' @param ... Further arguments passed to [save_forecast_plot()] (for example
#'   `file`, `format`, `n_forecasts`, `font_size`, `view_start`, `view_end`).
#'
#' @return The path(s) to the saved file(s), invisibly.
#'
#' @seealso [save_forecast_plot()]
#' @export
save_consistency_plot <- function(options_file, ...){

  # Delegating with the consistency target preselected
  save_forecast_plot(options_file, plot = "consistency", ...)

}
