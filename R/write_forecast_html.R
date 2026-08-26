#' Write the forecast plot section to a self-contained HTML page (internal)
#'
#' Internal writer for the HTML export path. Takes the full `htmltools` tag
#' output of `section_forecast_plots()` (which already contains the floating
#' legend, the per-plot expand button, and the geography dropdown) and wraps it
#' with the same CSS/JS the report loads, then writes it as a single
#' self-contained HTML file. Unlike `write_forecast_plot()` -- which snapshots
#' a bare plotly object for static export -- this preserves the report's
#' interactive chrome (legend, expand, dropdown) without any of the surrounding
#' report sections (no header, table of contents, or model text).
#'
#' Assembling the page here (rather than rendering the whole report) keeps the
#' output to just the plot. The required assets mirror the report's
#' `loading-scripts` chunk; if those change, update both.
#'
#' @param section The `htmltools` tag list returned by
#'   `section_forecast_plots()` with `return_plots = FALSE` (the full section,
#'   not the raw plotly objects).
#' @param file Output destination: a full `.html` file path or a directory.
#'   When a directory is given, the file is named `forecast_plot.html`.
#' @param overwrite Logical. Overwrite an existing file?
#' @param for_export Logical. When `TRUE`, the page is prepared for a static
#'   screenshot: the legend-drag/sync and y-axis rescaling scripts are left out,
#'   and a CSS block opens the report's floating legend in the plot's reserved
#'   right margin with the drag header gone, all sections expanded, and the
#'   toggle checkboxes hidden. Defaults to `FALSE`.
#'
#' @return The path written, invisibly.
#'
#' @keywords internal
#' @noRd
write_forecast_html <- function(section, file, overwrite = TRUE,
                                for_export = FALSE, font_size = NULL,
                                legend = TRUE) {

#------------------------------------------------------------------------------#
# Resolving the output path ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Resolves where the HTML lands. A directory target yields a default    #
# forecast_plot.html name; a file path is used as-is with its extension        #
# coerced to .html. Parent directories are created as needed.                  #
#------------------------------------------------------------------------------#

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
    out <- file.path(out_dir, "forecast_plot.html")

  }else{

    # Coercing the extension to .html
    out <- paste0(tools::file_path_sans_ext(file), ".html")

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
    stop("write_forecast_html(): file already exists and overwrite = FALSE: ",
         out, call. = FALSE)
  }

#------------------------------------------------------------------------------#
# Gathering the report assets --------------------------------------------------
#------------------------------------------------------------------------------#
# About: Pulls the same CSS and JS the report's loading-scripts chunk loads so #
# the standalone page styles and behaves identically: floating legend, legend  #
# checkbox sync, geography dropdown sync, and max-phase tracking. The          #
# per-plot expand button travels inside the widgets via onRender already.      #
#------------------------------------------------------------------------------#

  ###############################
  # Locating a packaged asset   #
  ###############################
  asset_path <- function(f){
    system.file("reports", f, package = "forecastEvalReport")
  }

  ###############################
  # CSS the plot section needs  #
  ###############################
  css_files <- c("styles.css", "accordion.css")

  # One <style> tag per readable CSS file
  css_tags <- lapply(css_files, function(f){
    p <- asset_path(f)
    if(nzchar(p)){
      htmltools::tags$style(paste(readLines(p), collapse = "\n"))
    }else{
      NULL
    }
  })

  ###############################
  # JS the plot section needs   #
  ###############################
  js_files <- c("plot-max-phase.js",
                "float-legend.js",
                "syncLegendCheckboxesOnRender.js",
                "sync-geo-dropdowns.js")

  # Static export keeps float-legend.js so the line swatches are painted from
  # the trace colors, but drops the y-rescaling and the checkbox-sync (the sync
  # would hide the default-off horizons we want shown). Positioning is handled
  # by the export CSS, not the script's drag.
  if(isTRUE(for_export)){
    js_files <- setdiff(js_files, c("plot-max-phase.js",
                                    "syncLegendCheckboxesOnRender.js"))
  }

  # One inline <script> per readable JS file
  js_tags <- lapply(js_files, function(f){
    p <- asset_path(f)
    if(nzchar(p)){
      htmltools::includeScript(p)
    }else{
      NULL
    }
  })

#------------------------------------------------------------------------------#
# Export legend layout (export only) ------------------------------------------#
#------------------------------------------------------------------------------#
# About: For a static export the report's floating legend is moved into        #
# the plot's reserved right margin and opened: the drag header is hidden,      #
# every section is forced expanded, the toggle checkboxes are hidden           #
# (their swatches and labels stay), and unchecked items are un-dimmed.         #
# Injected for export only, so the interactive HTML is untouched.              #
#------------------------------------------------------------------------------#

  export_css <- if(isTRUE(for_export) && isTRUE(legend)){
    htmltools::tags$style(htmltools::HTML(paste(
      # Reserve a 360px column on the right for the legend so it never overlaps
      # the plot or the right axis: the plot's fullscreen overlay is narrowed,
      # and the legend becomes a clean borderless side panel in that column
      # (a thin divider only), vertically centered -- not a floating box.
      ".fs-overlay.active { width: calc(100% - 360px) !important; }",
      ".fs-wrap .float-legend, .fs-wrap .floating-legend {",
      "  position: fixed !important; top: 0 !important;",
      "  right: 0 !important; left: auto !important; bottom: auto !important;",
      "  transform: none !important; width: 340px !important;",
      "  height: 100% !important; max-height: none !important;",
      "  display: flex !important; flex-direction: column !important;",
      "  justify-content: center !important;",
      "  box-sizing: border-box !important; padding: 0 22px !important;",
      "  border: none !important; border-left: 1px solid #e5e5e5 !important;",
      "  border-radius: 0 !important; box-shadow: none !important;",
      "  background: #ffffff !important; color: #1a1a1a !important;",
      "  z-index: 300001 !important;",
      "}",
      ".fs-wrap .float-legend .legend-header,",
      ".fs-wrap .float-legend .legend-drag { display: none !important; }",
      ".fs-wrap .float-legend .legend-body,",
      ".fs-wrap .float-legend.collapsed .legend-body {",
      "  display: block !important; max-height: none !important;",
      "  overflow: visible !important; padding: 0 !important;",
      "}",
      ".fs-wrap .float-legend .legend-section-content,",
      ".fs-wrap .float-legend .legend-section-content.collapsed {",
      "  display: block !important; max-height: none !important;",
      "  overflow: visible !important;",
      "}",
      ".fs-wrap .float-legend .section-toggle { display: none !important; }",
      # Section headers: compact, dark, with a hairline rule beneath.
      ".fs-wrap .float-legend .legend-section-title,",
      ".fs-wrap .float-legend .legend-section-title span {",
      "  font-size: 11px !important; font-weight: 600 !important;",
      "  color: #222222 !important; letter-spacing: 0.5px !important;",
      "}",
      ".fs-wrap .float-legend .legend-section-title {",
      "  border-bottom: 1px solid #dddddd !important;",
      "  padding-bottom: 3px !important; margin: 10px 0 6px !important;",
      "}",
      ".fs-wrap .float-legend .legend-section:first-child",
      "  .legend-section-title { margin-top: 0 !important; }",
      # Tight rows; swatch moved to the LEFT of the label (symbol-then-label).
      ".fs-wrap .float-legend .legend-item {",
      "  padding: 2px 0 !important; margin: 0 0 1px !important;",
      "  color: #1a1a1a !important;",
      "}",
      ".fs-wrap .float-legend .legend-swatch,",
      ".fs-wrap .float-legend .legend-swatch.training,",
      ".fs-wrap .float-legend .legend-swatch.validation,",
      ".fs-wrap .float-legend .legend-swatch.testing,",
      ".fs-wrap .float-legend .legend-swatch.pi-50,",
      ".fs-wrap .float-legend .legend-swatch.pi-95 {",
      "  order: -1 !important; margin-left: 0 !important;",
      "}",
      ".fs-wrap .float-legend .legend-checkbox { display: none !important; }",
      ".fs-wrap .float-legend .legend-checkbox:not(:checked) ~ span {",
      "  opacity: 1 !important; color: inherit !important;",
      "}",
      sep = "\n"
    )))
  }else if(isTRUE(for_export)){
    # legend = FALSE: no reserved column -- the plot fills the full capture
    # width and the floating legend is hidden entirely.
    htmltools::tags$style(htmltools::HTML(paste(
      ".fs-overlay.active { width: 100% !important; }",
      ".fs-wrap .float-legend, .fs-wrap .floating-legend {",
      "  display: none !important;",
      "}",
      sep = "\n"
    )))
  }else{
    NULL
  }

  ###################################
  # Scaling the legend to font_size #
  ###################################
  # The floating legend is HTML/CSS, so the plot's font size does not reach it.
  # When a static export sets font_size, scale the legend entries and section
  # titles to match, so the whole figure reads at one size.
  font_css <- if(isTRUE(for_export) && !is.null(font_size)){
    htmltools::tags$style(htmltools::HTML(paste(
      ".fs-wrap .float-legend .legend-item,",
      ".fs-wrap .float-legend .legend-item span {",
      paste0("  font-size: ", font_size, "px !important;"),
      "}",
      ".fs-wrap .float-legend .legend-section-title,",
      ".fs-wrap .float-legend .legend-section-title span {",
      paste0("  font-size: ", font_size, "px !important;"),
      "}",
      sep = "\n"
    )))
  }else{
    NULL
  }

#------------------------------------------------------------------------------#
# Open-expanded behavior (export only) -----------------------------------------
#------------------------------------------------------------------------------#
# About: Opens the standalone HTML already expanded, matching the report's     #
# fullscreen button exactly. It waits for the global toggleFullscreenFromGd()  #
# (installed by plotly_fullscreen.js) and a visible, rendered plot, then calls #
# it once -- so the result is identical to clicking expand. Added here only,   #
# never in the rendered report.                                                #
#------------------------------------------------------------------------------#

  ###################################
  # Script: expand the plot on open #
  ###################################
  fill_js <- htmltools::tags$script(htmltools::HTML(paste(
    "(function() {",
    "  var done = false, tries = 0;",
    "  function visiblePlot() {",
    "    var plots = document.querySelectorAll('.js-plotly-plot');",
    "    for (var i = 0; i < plots.length; i++) {",
    "      var gd = plots[i];",
    "      if (gd.offsetParent !== null && gd._fullLayout) { return gd; }",
    "    }",
    "    return null;",
    "  }",
    "  function tryExpand() {",
    "    if (done) { return true; }",
    "    if (typeof window.toggleFullscreenFromGd !== 'function') {",
    "      return false;",
    "    }",
    "    var gd = visiblePlot();",
    "    if (!gd) { return false; }",
    "    done = true;",
    "    window.toggleFullscreenFromGd(gd);",
    "    var sel = '[data-title=\"Toggle Fullscreen\"]';",
    "    var btns = document.querySelectorAll(sel);",
    "    for (var b = 0; b < btns.length; b++) {",
    "      btns[b].style.display = 'none';",
    "    }",
    "    return true;",
    "  }",
    "  var timer = setInterval(function() {",
    "    tries++;",
    "    if (tryExpand() || tries > 60) { clearInterval(timer); }",
    "  }, 100);",
    "})();",
    sep = "\n"
  )))

#------------------------------------------------------------------------------#
# Writing the self-contained page ----------------------------------------------
#------------------------------------------------------------------------------#
# About: Combines assets and the plot section into one page, writes it with    #
# its widget dependencies, then inlines everything into a single self-contained#
# HTML file via pandoc so the result is a single shareable file.               #
#------------------------------------------------------------------------------#

  #############################
  # Assembling the full page  #
  #############################
  page <- htmltools::tagList(css_tags, export_css, font_css, js_tags,
                             section, fill_js)

  #####################################
  # Writing then self-containing it   #
  #####################################

  # Intermediate page (plus its lib/ dependencies) in a temp location
  tmp_html <- tempfile(fileext = ".html")
  on.exit(unlink(tmp_html), add = TRUE)

  # Writing the page and its widget dependencies
  htmltools::save_html(page, file = tmp_html)

  ################################################
  # Stripping the DOCTYPE before self-containing #
  ################################################
  # pandoc reads the page through its markdown reader when self-containing,
  # which escapes a leading <!DOCTYPE html> into visible "!DOCTYPE" text in
  # the top-left of the page. Removing that token leaves the rest of the
  # document intact; pandoc re-adds a proper declaration in its template.
  raw <- paste(readLines(tmp_html, warn = FALSE), collapse = "\n")
  raw <- sub("<!doctype\\s+html\\s*>", "", raw, ignore.case = TRUE)
  writeLines(raw, tmp_html)

  # Inlining all dependencies into a single self-contained file
  rmarkdown::pandoc_self_contained_html(tmp_html, out)

  # Telling the user where it landed
  message("\u2713 Forecast plot saved to: ", out)

  ###########################
  # Returning the file path #
  ###########################
  invisible(out)

}
