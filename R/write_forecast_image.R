#' Write the forecast plot section to a static image (internal)
#'
#' Internal writer for the static export path (PNG / JPEG / PDF). It builds the
#' very same self-contained HTML page the HTML export produces -- via
#' `write_forecast_html()`, which opens already expanded so the legend sits to
#' the side rather than floating over the plot -- and captures that page as an
#' image with `webshot2`. Screenshotting the rendered, expanded page (rather
#' than snapshotting a bare plotly object) is what lets the static image match
#' the report: the side legend, the correct axis scale, and all data are
#' exactly what the page shows.
#'
#' The section should be built with `for_export = TRUE` so the range slider is
#' gone and the full series is in view. `webshot2` (headless Chrome) is
#' required; there is no figure-only fallback because the side legend lives in
#' the page DOM, not in the plotly object.
#'
#' @param section The `htmltools` tag list returned by
#'   `section_forecast_plots()` (built with `for_export = TRUE`).
#' @param file Output destination: a full image path or a directory. When a
#'   directory is given, the file is named `forecast_plot.<format>`.
#' @param format One of `"png"`, `"jpeg"`, `"jpg"`, `"pdf"`, `"tiff"`, `"tif"`.
#'   TIFF is captured as PNG then converted with the `magick` package (lossless
#'   LZW, tagged 300 dpi). The file extension is coerced to match.
#' @param width,height Viewport size in pixels for the capture. Default to
#'   1400 x 850 when `NULL`.
#' @param scale Resolution multiplier (passed to `webshot2` as `zoom`).
#' @param delay Seconds to wait before capturing, allowing the plot to render
#'   and the page to finish expanding to the side-legend view.
#' @param overwrite Logical. Overwrite an existing file?
#'
#' @return The path written, invisibly.
#'
#' @keywords internal
#' @noRd
write_forecast_image <- function(section,
                                 file,
                                 format    = "png",
                                 width     = NULL,
                                 height    = NULL,
                                 scale     = 1,
                                 delay     = 4,
                                 overwrite = TRUE,
                                 font_size = NULL,
                                 legend    = TRUE) {

#------------------------------------------------------------------------------#
# Section guard ----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: A NULL or empty section means the upstream builder found nothing to   #
# plot (e.g. no archived forecasts, no implementation model, or no truth for   #
# this geography). Capturing it would silently write a blank white image, so   #
# stop with a clear message instead of producing a junk file.                  #
#------------------------------------------------------------------------------#

  if(is.null(section) || length(section) == 0){
    stop("write_forecast_image(): nothing to export -- the plot section is ",
         "empty (no data for this plot/location), so no image was written. ",
         "Re-run with quiet = FALSE to see which section was skipped.",
         call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Backend guard ----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Static capture renders the live page in headless Chrome, so webshot2  #
# is required. There is no figure-only fallback because the side legend is     #
# part of the page, not the plotly object.                                     #
#------------------------------------------------------------------------------#

  if(!requireNamespace("webshot2", quietly = TRUE)){
    stop("write_forecast_image(): static export needs the 'webshot2' package ",
         "(and Chrome/Chromium). Install it with ",
         "install.packages(\"webshot2\"), or export to HTML instead.",
         call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Resolving the output path ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Resolves where the image lands. A directory target yields a default   #
# forecast_plot.<format> name; a file path is used as-is with its extension    #
# coerced to the requested format. Parent directories are created as needed.   #
#------------------------------------------------------------------------------#

  ###################################
  # Normalizing the format / ext    #
  ###################################
  fmt <- tolower(format)
  ext <- if(identical(fmt, "jpg")){
    "jpeg"
  }else if(identical(fmt, "tif")){
    "tiff"
  }else{
    fmt
  }

  ###################################
  # Detecting a directory target    #
  ###################################
  dir_target <- is.null(file) ||
    dir.exists(file) ||
    grepl("[/\\\\]$", file) ||
    identical(tools::file_ext(file), "")

  ###########################
  # Building the file path  #
  ###########################
  if(dir_target){

    # Resolving (and creating) the output directory
    out_dir <- if(is.null(file)) "." else file
    if(!dir.exists(out_dir)){
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Default name inside the directory
    out <- file.path(out_dir, paste0("forecast_plot.", ext))

  }else{

    # Coercing the extension to the requested format
    out <- paste0(tools::file_path_sans_ext(file), ".", ext)

    # Creating the parent directory if needed
    parent <- dirname(out)
    if(nzchar(parent) && !dir.exists(parent)){
      dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    }

  }

  ##############################
  # Overwrite protection guard #
  ##############################
  if(!isTRUE(overwrite) && file.exists(out)){
    stop("write_forecast_image(): file already exists and overwrite = FALSE: ",
         out, call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Building the page to capture -------------------------------------------------
#------------------------------------------------------------------------------#
# About: Reuses write_forecast_html() to produce the exact page the HTML       #
# export makes -- the report's chrome plus the open-expanded script -- so the  #
# captured image shows the side legend, not the floating overlay.              #
#------------------------------------------------------------------------------#

  ###################################
  # Rendering the page to a temp    #
  ###################################
  # Debug aid: when getOption("fer.keep_export_html") or the FER_KEEP_EXPORT_HTML
  # env var is set, write the intermediate page next to the image (same name,
  # .html) and keep it, so it can be opened in a browser to inspect rendering.
  .keep_html <- isTRUE(getOption("fer.keep_export_html")) ||
    nzchar(Sys.getenv("FER_KEEP_EXPORT_HTML"))

  if(isTRUE(.keep_html)){
    tmp_html <- paste0(tools::file_path_sans_ext(out), ".export.html")
  }else{
    tmp_html <- tempfile(fileext = ".html")
    on.exit(unlink(tmp_html), add = TRUE)
  }

  write_forecast_html(section = section, file = tmp_html, overwrite = TRUE,
                      for_export = TRUE, font_size = font_size,
                      legend = legend)

  if(isTRUE(.keep_html)){
    message("write_forecast_image(): kept export HTML at ", tmp_html)
  }

#------------------------------------------------------------------------------#
# Capturing the image ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Captures the rendered page with webshot2 at the requested viewport    #
# and zoom. The delay gives the plot time to draw and the page time to finish  #
# expanding to the side-legend view before the screenshot is taken.            #
#------------------------------------------------------------------------------#

  ###############################
  # Viewport and zoom defaults  #
  ###############################
  vw   <- if(is.null(width))  1400 else width
  vh   <- if(is.null(height))  850 else height
  zoom <- if(is.null(scale))     1 else scale
  delay <- if(is.null(delay))    4 else delay

  # Source URL of the rendered page
  src_url <- paste0("file://", normalizePath(tmp_html, winslash = "/"))

  #############################
  # Capturing the page image  #
  #############################
  if(identical(ext, "tiff")){

    # webshot2 (Chrome) cannot emit TIFF, so capture a PNG and convert it with
    # magick. The TIFF is written lossless (LZW) and tagged 300 dpi, the usual
    # journal requirement. Pixel dimensions follow vwidth/vheight x scale, so
    # raise `scale` for a higher-resolution figure.
    if(!requireNamespace("magick", quietly = TRUE)){
      stop("write_forecast_image(): TIFF export needs the 'magick' package. ",
           "Install it with install.packages(\"magick\"), or use png/pdf.",
           call. = FALSE)
    }

    tmp_png <- tempfile(fileext = ".png")
    on.exit(unlink(tmp_png), add = TRUE)

    webshot2::webshot(url = src_url, file = tmp_png, vwidth = vw,
                      vheight = vh, zoom = zoom, delay = delay)

    img <- magick::image_read(tmp_png)
    img <- magick::image_flatten(magick::image_background(img, "white"))
    magick::image_write(img, path = out, format = "tiff",
                        density = "300x300", compression = "LZW")

  }else{

    webshot2::webshot(url = src_url, file = out, vwidth = vw,
                      vheight = vh, zoom = zoom, delay = delay)

  }

  # Telling the user where it landed
  message("\u2713 Forecast plot saved to: ", out)

  ###########################
  # Returning the file path #
  ###########################
  invisible(out)

}
