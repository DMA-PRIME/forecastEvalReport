#' Save a printable PDF copy of the forecast evaluation report
#'
#' Produces a paginated PDF of the forecast evaluation report that matches the
#' interactive HTML version. The PDF is printed from the rendered report itself
#' using headless Chrome, so the fonts, colors, table styling, and Plotly
#' figures are the ones the report already shows -- nothing is re-drawn or
#' re-styled for print.
#'
#' Supply either an `options_file` (the same completed options file
#' `generate_report()` takes, which is rendered to HTML first) or an
#' `html_file` that has already been rendered. Supplying both is an error.
#'
#' Three things are adjusted before printing, because they exist only to
#' support interaction and would otherwise leave the PDF with content missing:
#' collapsed `<details>` accordions are opened so their contents appear, the
#' floating plot legends are expanded and pinned beside the figure rather than
#' floating over it, and controls that cannot be used on paper -- the Plotly
#' modebar, the fullscreen button, and the geography dropdown -- are hidden.
#' Every one of these can be turned off individually. Whichever geography the
#' report opens on is the one that prints, matching the on-screen view.
#'
#' The report body is 1500px wide, so the default page is tabloid landscape
#' (17 x 11in), which fits that width at 96dpi without shrinking the layout.
#' Narrower paper still works -- pass `scale` below `1` to fit more across --
#' but the layout will be reduced accordingly.
#'
#' Printing requires \pkg{pagedown} (which drives headless Chrome through
#' \pkg{chromote}). Chrome or Chromium must be installed and discoverable; see
#' `pagedown::find_chrome()` if it is not found automatically.
#'
#' @param options_file Path to a completed report options `.R` file produced by
#'   `create_options_template()` -- the same file `generate_report()` uses. The
#'   report is rendered to HTML in a temporary directory, then printed. Leave
#'   `NULL` when supplying `html_file`.
#' @param html_file Path to an already-rendered report `.html` file (or a vector
#'   of them, as produced by `generate_report(split_by_location = TRUE)`). One
#'   PDF is written per file. Leave `NULL` when supplying `options_file`.
#' @param output_dir Directory where the PDF should be saved. Defaults to the
#'   current working directory.
#' @param output_file Name of the PDF, ending in `.pdf`. When `NULL` (default)
#'   the name of the source HTML report is reused with a `.pdf` extension.
#'   Ignored when several `html_file` paths are supplied, since each keeps its
#'   own name.
#' @param paper Page size. One of `"tabloid-landscape"` (default), `"tabloid"`,
#'   `"letter-landscape"`, `"letter"`, `"legal-landscape"`, `"legal"`,
#'   `"a3-landscape"`, `"a3"`, `"a4-landscape"`, `"a4"`, or `"custom"`. Use
#'   `"custom"` with `width` and `height` for any other size.
#' @param width,height Page size in inches. Used only when `paper = "custom"`.
#' @param margin Page margin in inches, applied to all four sides. A single
#'   non-negative number; defaults to `0.4`.
#' @param scale Zoom applied to the page content, between `0.1` and `2`.
#'   Defaults to `1` (the report at its true size). Lower values fit more
#'   content across a narrower page.
#' @param wait Seconds to wait after the page loads before printing, so the
#'   Plotly figures can finish drawing. Defaults to `8`. Raise it if figures
#'   come out blank or half-drawn.
#' @param background Logical. Print background colors and images? `TRUE`
#'   (default) keeps striped tables, phase ribbons, and accordion shading, so
#'   the PDF matches the screen. `FALSE` gives a lighter, ink-saving page.
#' @param expand_accordions Logical. Open every collapsed `<details>` accordion
#'   before printing so its contents appear in the PDF? Defaults to `TRUE`.
#'   `FALSE` prints only the headers of any section left collapsed.
#' @param expand_legends Logical. Expand each plot's floating legend and place
#'   it beside the figure instead of over it? Defaults to `TRUE`, matching the
#'   static exports from `save_forecast_plot()`.
#' @param hide_controls Logical. Hide interactive-only controls -- the Plotly
#'   modebar, the fullscreen button, and the geography dropdown -- that have no
#'   meaning on paper? Defaults to `TRUE`.
#' @param keep_html Logical. Keep the intermediate print-ready HTML next to the
#'   PDF? Defaults to `FALSE`. Useful for inspecting what was sent to Chrome
#'   when a PDF does not come out as expected.
#' @param overwrite Logical. Overwrite an existing PDF? Defaults to `TRUE`.
#' @param quiet Logical. If `TRUE` (default), suppresses rendering and printing
#'   console output. Set to `FALSE` for verbose output useful when debugging.
#' @param ... Further arguments passed to `generate_report()` when rendering
#'   from `options_file` (for example `plot_styles`, `eval_config`,
#'   `location_crosswalk`, or `population_crosswalk`).
#'
#' @return Invisibly returns the path(s) to the saved PDF file(s).
#'
#' @examples
#' \dontrun{
#'   # From a completed options file, straight to a PDF
#'   save_report_pdf("report_options.R", output_dir = "exports")
#'
#'   # From a report that has already been rendered
#'   save_report_pdf(html_file = "exports/Smith-Influenza-Testing-ARIMA.html")
#'
#'   # Letter landscape, scaled down to fit the 1500px body
#'   save_report_pdf(
#'     "report_options.R",
#'     paper = "letter-landscape",
#'     scale = 0.62
#'   )
#'
#'   # Every per-location report from a split run
#'   paths <- generate_report("report_options.R", split_by_location = TRUE)
#'   save_report_pdf(html_file = paths, output_dir = "exports/pdf")
#' }
#'
#' @export
save_report_pdf <- function(options_file      = NULL,
                            html_file         = NULL,
                            output_dir        = getwd(),
                            output_file       = NULL,
                            paper             = c("tabloid-landscape",
                                                  "tabloid",
                                                  "letter-landscape",
                                                  "letter",
                                                  "legal-landscape",
                                                  "legal",
                                                  "a3-landscape",
                                                  "a3",
                                                  "a4-landscape",
                                                  "a4",
                                                  "custom"),
                            width             = NULL,
                            height            = NULL,
                            margin            = 0.4,
                            scale             = 1,
                            wait              = 8,
                            background        = TRUE,
                            expand_accordions = TRUE,
                            expand_legends    = TRUE,
                            hide_controls     = TRUE,
                            keep_html         = FALSE,
                            overwrite         = TRUE,
                            quiet             = TRUE,
                            ...) {

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Confirms exactly one source was named and that every numeric setting  #
# is usable before any rendering or printing begins. Deeper validation of the  #
# options file itself is handled by generate_report(), which this function     #
# calls, so it is not duplicated here.                                         #
#------------------------------------------------------------------------------#

  ##########################################
  # Exactly one input source must be given #
  ##########################################
  has_options <- !is.null(options_file) && !all(is.na(options_file))
  has_html    <- !is.null(html_file)    && !all(is.na(html_file))

  if(has_options && has_html){

    # Stopping if both sources are provided
    stop(
      "Both `options_file` and `html_file` were supplied.\n\n",
      "save_report_pdf() takes one source or the other:\n",
      "  options_file -- renders the report, then prints it\n",
      "  html_file    -- prints a report that is already rendered",
      call. = FALSE
    )

  }

  if(!has_options && !has_html){

    # Stopping if neither source is provided
    stop(
      "No report source provided.\n\n",
      "Supply either:\n",
      "  options_file -- the same completed options file generate_report()\n",
      "                  takes; create one with create_options_template()\n",
      "  html_file    -- a report .html file that has already been rendered",
      call. = FALSE
    )

  }

  ############################################
  # Chrome-backed printing must be available #
  ############################################
  if(!requireNamespace("pagedown", quietly = TRUE)){

    # Stopping if the printing backend is missing
    stop(
      "save_report_pdf() needs the 'pagedown' package to print the report\n",
      "through headless Chrome.\n\n",
      "Install it with:\n",
      "  install.packages(\"pagedown\")\n\n",
      "Chrome or Chromium must also be installed. If it is present but not\n",
      "found automatically, see pagedown::find_chrome().",
      call. = FALSE
    )

  }

  ##################################
  # Validating the page dimensions #
  ##################################

  # Validating the paper preset
  paper <- match.arg(paper)

  # Checking the margin
  if(!is.numeric(margin) || length(margin) != 1L ||
     !is.finite(margin) || margin < 0){

    # Stopping if the margin is unusable
    stop("`margin` must be a single non-negative number of inches.",
         call. = FALSE)

  }

  # Checking the scale against Chrome's accepted range
  if(!is.numeric(scale) || length(scale) != 1L ||
     !is.finite(scale) || scale < 0.1 || scale > 2){

    # Stopping if the scale is outside Chrome's range
    stop("`scale` must be a single number between 0.1 and 2.", call. = FALSE)

  }

  # Checking the render wait
  if(!is.numeric(wait) || length(wait) != 1L || !is.finite(wait) || wait < 0){

    # Stopping if the wait is unusable
    stop("`wait` must be a single non-negative number of seconds.",
         call. = FALSE)

  }

#------------------------------------------------------------------------------#
# Resolving the page size ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Turns the paper preset into the width and height in inches that       #
# Chrome's print interface expects. The default is tabloid landscape because   #
# the report body is 1500px wide, which is 15.6in at 96dpi and so needs a page #
# wider than letter to print without being shrunk.                             #
#------------------------------------------------------------------------------#

  ####################################
  # Page dimensions for each preset  #
  ####################################
  paper_sizes <- list(
    "letter"            = c(8.50, 11.00),
    "letter-landscape"  = c(11.00, 8.50),
    "legal"             = c(8.50, 14.00),
    "legal-landscape"   = c(14.00, 8.50),
    "tabloid"           = c(11.00, 17.00),
    "tabloid-landscape" = c(17.00, 11.00),
    "a4"                = c(8.27, 11.69),
    "a4-landscape"      = c(11.69, 8.27),
    "a3"                = c(11.69, 16.54),
    "a3-landscape"      = c(16.54, 11.69)
  )

  ####################################
  # Resolving custom vs preset sizes #
  ####################################
  if(identical(paper, "custom")){

    # Both dimensions are required for a custom page
    if(is.null(width) || is.null(height) ||
       !is.numeric(width)  || length(width)  != 1L || !is.finite(width)  ||
       !is.numeric(height) || length(height) != 1L || !is.finite(height) ||
       width <= 0 || height <= 0){

      # Stopping if a custom page is incompletely specified
      stop(
        "`paper = \"custom\"` needs both `width` and `height` as single\n",
        "positive numbers of inches.",
        call. = FALSE
      )

    }

    # Custom page dimensions
    page_width  <- width
    page_height <- height

  }else{

    # Preset page dimensions
    page_width  <- paper_sizes[[paper]][1]
    page_height <- paper_sizes[[paper]][2]

  }

  #############################################
  # Margins must leave a printable page area  #
  #############################################
  if((2 * margin) >= page_width || (2 * margin) >= page_height){

    # Stopping if the margins consume the whole page
    stop(
      "`margin` of ", margin, "in leaves no printable area on a ",
      page_width, " x ", page_height, "in page.\n\n",
      "Reduce `margin`, or choose a larger `paper` size.",
      call. = FALSE
    )

  }

#------------------------------------------------------------------------------#
# Resolving the output directory -----------------------------------------------
#------------------------------------------------------------------------------#
# About: Anchors the destination to an absolute path up front. Chrome is       #
# handed absolute paths for both its input and its output, so a relative       #
# destination is resolved here rather than inside the printing step.           #
#------------------------------------------------------------------------------#

  ###################################
  # Creating the directory if needed #
  ###################################
  if(!dir.exists(output_dir)){

    # Attempting to create the destination
    created <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    if(!isTRUE(created)){

      # Stopping if the destination cannot be made
      stop(
        "The output directory could not be created:\n  ", output_dir, "\n\n",
        "Check that the parent folder exists and is writable.",
        call. = FALSE
      )

    }

  }

  # Absolute destination
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

#------------------------------------------------------------------------------#
# Obtaining the rendered report ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Collects the HTML file(s) to print. When an options file is given the #
# report is rendered into a temp directory first, so the interactive HTML is   #
# an intermediate and only the PDF is left behind; when HTML files are given   #
# directly they are used as-is and never modified in place.                    #
#------------------------------------------------------------------------------#

  ##########################################
  # Rendering the report from the options  #
  ##########################################
  if(has_options){

    # Rendering into a clean temp directory
    render_dir <- tempfile("report_pdf_")
    dir.create(render_dir)
    on.exit(unlink(render_dir, recursive = TRUE), add = TRUE)

    # Producing the interactive HTML the PDF is printed from
    source_html <- generate_report(
      options_file = options_file,
      output_dir   = render_dir,
      quiet        = quiet,
      ...
    )

  ###################################
  # Using reports already rendered  #
  ###################################
  }else{

    # Normalizing to a character vector of paths
    source_html <- as.character(html_file)

    # Every named file must exist
    missing_html <- source_html[!file.exists(source_html)]

    if(length(missing_html) > 0){

      # Stopping if any report is missing
      stop(
        "These report file(s) could not be found:\n",
        paste0("  ", missing_html, "\n", collapse = ""),
        call. = FALSE
      )

    }

    # Every named file must be HTML
    bad_ext <- source_html[
      !grepl("\\.html?$", source_html, ignore.case = TRUE)
    ]

    if(length(bad_ext) > 0){

      # Stopping if a non-HTML file was supplied
      stop(
        "`html_file` must point to rendered report .html file(s).\n",
        "These are not HTML:\n",
        paste0("  ", bad_ext, "\n", collapse = ""),
        call. = FALSE
      )

    }

    # Absolute source paths
    source_html <- normalizePath(source_html, winslash = "/", mustWork = TRUE)

  }

#------------------------------------------------------------------------------#
# Building the print stylesheet ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Assembles the CSS injected into the copy of the report that Chrome    #
# prints. It changes nothing about how the report looks -- it only settles     #
# what interaction would otherwise have settled: which accordions are open,    #
# where the legend sits, and which on-screen controls are dropped. The rules   #
# for keeping figures and tables off page boundaries are added always, since   #
# a figure split across two pages is the one way a print can misrepresent it.  #
#------------------------------------------------------------------------------#

  ##################################
  # Rules that always apply        #
  ##################################
  print_css <- c(
    # The floating TOC is a fixed sidebar on screen; on paper it must sit in
    # the flow or it repeats on top of every page.
    "#TOC, .tocify {",
    "  position: static !important; width: auto !important;",
    "  max-width: none !important; max-height: none !important;",
    "  overflow: visible !important; border: none !important;",
    "}",
    ".tocify-subheader { display: block !important; }",
    # Keeping a figure, table, or accordion whole on one page.
    ".plot-panel, .fs-wrap, .js-plotly-plot, .html-widget,",
    "details.accordion, table, .section-intro {",
    "  break-inside: avoid !important;",
    "  page-break-inside: avoid !important;",
    "}",
    # Headings should not be stranded at the foot of a page.
    "h1, h2, h3 {",
    "  break-after: avoid !important;",
    "  page-break-after: avoid !important;",
    "}",
    # Hidden geography panels stay hidden: the printed report shows the same
    # geography the report opens on.
    ".plot-panel:not(.active-plot-panel) { display: none !important; }"
  )

  ####################################
  # Opening the collapsed accordions #
  ####################################
  if(isTRUE(expand_accordions)){

    print_css <- c(
      print_css,
      "details.accordion > *:not(summary) { display: block !important; }",
      "details.accordion summary::after { display: none !important; }"
    )

  }

  ####################################
  # Expanding the floating legends   #
  ####################################
  if(isTRUE(expand_legends)){

    print_css <- c(
      print_css,
      # The legend becomes a plain panel in the top-right of the figure rather
      # than a draggable box, and every section within it is opened.
      ".fs-wrap .float-legend, .fs-wrap .floating-legend {",
      "  position: absolute !important; top: 10px !important;",
      "  right: 10px !important; left: auto !important; bottom: auto !important;",
      "  transform: none !important; max-height: none !important;",
      "  overflow: visible !important; box-shadow: none !important;",
      "  background: #ffffff !important;",
      "  border: 1px solid #e5e5e5 !important;",
      "}",
      ".fs-wrap .float-legend .legend-body,",
      ".fs-wrap .float-legend.collapsed .legend-body {",
      "  display: block !important; max-height: none !important;",
      "  overflow: visible !important;",
      "}",
      ".fs-wrap .float-legend .legend-section-content,",
      ".fs-wrap .float-legend .legend-section-content.collapsed {",
      "  display: block !important; max-height: none !important;",
      "  overflow: visible !important;",
      "}",
      # The drag handle, the collapse chevrons, and the trace checkboxes are
      # all interaction-only; the swatches and labels stay.
      ".fs-wrap .float-legend .legend-toggle,",
      ".fs-wrap .float-legend .section-toggle,",
      ".fs-wrap .float-legend .legend-checkbox { display: none !important; }",
      ".fs-wrap .legend-drag { cursor: default !important; }",
      # An unchecked trace is dimmed on screen; on paper that reads as an
      # error, so labels print at full strength.
      ".fs-wrap .float-legend .legend-checkbox:not(:checked) ~ span {",
      "  opacity: 1 !important; color: inherit !important;",
      "}"
    )

  }

  ####################################
  # Dropping interactive-only chrome #
  ####################################
  if(isTRUE(hide_controls)){

    print_css <- c(
      print_css,
      ".modebar, .modebar-container { display: none !important; }",
      "[data-title=\"Toggle Fullscreen\"] { display: none !important; }",
      ".geo-filter-row { display: none !important; }"
    )

  }

  ####################################
  # Assembling the style tag         #
  ####################################
  style_tag <- paste0(
    "<style id=\"fe-print-css\">\n",
    paste(print_css, collapse = "\n"),
    "\n</style>"
  )

#------------------------------------------------------------------------------#
# Building the print script ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: A handful of things cannot be reached from CSS. The open state of a   #
# <details> element is an attribute, not a style, and the collapsed classes on #
# the legends are set by the report's own JavaScript after the page loads, so  #
# they are cleared here once rather than fought with selectors. The script     #
# runs on load and again shortly after, since the legend JS may not have run   #
# by the time the first pass fires.                                            #
#------------------------------------------------------------------------------#

  ##################################
  # Statements the script performs #
  ##################################
  script_lines <- character(0)

  # Opening every accordion
  if(isTRUE(expand_accordions)){

    script_lines <- c(
      script_lines,
      "    var d = document.querySelectorAll('details');",
      "    for (var i = 0; i < d.length; i++) { d[i].open = true; }"
    )

  }

  # Clearing the collapsed classes the legend JS applies
  if(isTRUE(expand_legends)){

    script_lines <- c(
      script_lines,
      "    var c = document.querySelectorAll(",
      "      '.float-legend.collapsed, .floating-legend.collapsed,' +",
      "      ' .legend-section-content.collapsed');",
      "    for (var j = 0; j < c.length; j++) {",
      "      c[j].classList.remove('collapsed');",
      "    }"
    )

  }

  ##################################
  # Assembling the script tag      #
  ##################################
  script_tag <- if(length(script_lines) > 0){

    paste0(
      "<script id=\"fe-print-js\">\n",
      paste(
        c(
          "(function() {",
          "  function fePrepare() {",
          script_lines,
          "  }",
          "  if (document.readyState === 'loading') {",
          "    document.addEventListener('DOMContentLoaded', fePrepare);",
          "  } else {",
          "    fePrepare();",
          "  }",
          "  setTimeout(fePrepare, 1500);",
          "  setTimeout(fePrepare, 4000);",
          "})();"
        ),
        collapse = "\n"
      ),
      "\n</script>"
    )

  }else{""}

#------------------------------------------------------------------------------#
# Printing each report ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: For each source report, a print-ready copy is written with the style  #
# and script blocks appended just before </body>, then Chrome prints that copy #
# to PDF. The copy is written beside the original so the report's self-        #
# contained assets resolve identically, and the original is never altered.     #
#------------------------------------------------------------------------------#

  #####################################
  # Collecting the saved PDF paths    #
  #####################################
  saved_pdf_paths <- character(0)

  #####################################
  # Printing one report at a time     #
  #####################################
  for(i in seq_along(source_html)){

    # The report being printed
    this_html <- source_html[i]

    ####################################
    # Resolving this report's PDF name #
    ####################################
    if(!is.null(output_file) && length(source_html) == 1L){

      # Honoring the caller's filename
      pdf_name <- output_file

      # The extension must be .pdf
      if(!grepl("\\.pdf$", pdf_name, ignore.case = TRUE)){

        # Stopping if the extension is wrong
        stop("`output_file` must end with .pdf", call. = FALSE)

      }

    }else{

      # Reusing the report's own name
      pdf_name <- paste0(
        sub("\\.html?$", "", basename(this_html), ignore.case = TRUE),
        ".pdf"
      )

    }

    # Full destination for this PDF
    pdf_path <- file.path(output_dir, pdf_name)

    ####################################
    # Respecting the overwrite setting #
    ####################################
    if(file.exists(pdf_path) && !isTRUE(overwrite)){

      # Stopping rather than replacing an existing file
      stop(
        "A PDF already exists at:\n  ", pdf_path, "\n\n",
        "Set `overwrite = TRUE` to replace it, or choose another\n",
        "`output_file` or `output_dir`.",
        call. = FALSE
      )

    }

    ##########################################
    # Writing the print-ready copy of the HTML #
    ##########################################

    # Reading the rendered report
    report_lines <- readLines(this_html, warn = FALSE, encoding = "UTF-8")
    report_html  <- paste(report_lines, collapse = "\n")

    # Injecting before the closing body tag, or appending when none is found
    injection <- paste0(style_tag, "\n", script_tag, "\n")

    if(grepl("</body>", report_html, fixed = TRUE)){

      # Placing the blocks last so they override the report's own styles
      report_html <- sub(
        "</body>",
        paste0(injection, "</body>"),
        report_html,
        fixed = TRUE
      )

    }else{

      # Appending when the document has no closing body tag
      report_html <- paste0(report_html, "\n", injection)

    }

    # Writing the copy beside the original so relative assets still resolve
    print_html <- file.path(
      dirname(this_html),
      paste0(
        sub("\\.html?$", "", basename(this_html), ignore.case = TRUE),
        "_print.html"
      )
    )

    writeLines(report_html, print_html, useBytes = TRUE)

    # Removing the intermediate unless it was asked for
    if(!isTRUE(keep_html)){
      on.exit(unlink(print_html), add = TRUE)
    }

    ####################################
    # Printing the copy through Chrome #
    ####################################
    tryCatch(

      {

        pagedown::chrome_print(
          input   = print_html,
          output  = pdf_path,
          wait    = wait,
          verbose = if(isTRUE(quiet)) 0 else 1,
          options = list(
            printBackground   = isTRUE(background),
            scale             = scale,
            paperWidth        = page_width,
            paperHeight       = page_height,
            marginTop         = margin,
            marginBottom      = margin,
            marginLeft        = margin,
            marginRight       = margin,
            preferCSSPageSize = FALSE
          )
        )

      },

      error = function(e){

        # Translating printing failures into something actionable
        stop(
          "The report could not be printed to PDF.\n\n",
          "Chrome reported:\n  ", conditionMessage(e), "\n\n",
          "Common causes:\n",
          "  - Chrome or Chromium is not installed or cannot be found\n",
          "    (see pagedown::find_chrome())\n",
          "  - The figures had not finished drawing; raise `wait`\n",
          "  - The destination is not writable:\n    ", pdf_path,
          call. = FALSE
        )

      }

    )

    # Recording the finished PDF
    saved_pdf_paths <- c(saved_pdf_paths, pdf_path)

    # Keeping the print-ready HTML where it can be found
    if(isTRUE(keep_html)){

      # Moving the intermediate next to its PDF
      kept_html <- file.path(output_dir, basename(print_html))

      if(!identical(normalizePath(print_html, winslash = "/"),
                    normalizePath(kept_html,  winslash = "/",
                                  mustWork = FALSE))){

        file.copy(print_html, kept_html, overwrite = TRUE)
        unlink(print_html)

      }

    }

  }

#------------------------------------------------------------------------------#
# Confirm completion and return ------------------------------------------------
#------------------------------------------------------------------------------#
# About: Prints a success message listing every PDF written and returns the    #
# path(s) invisibly so they can be captured and used in pipelines if needed.   #
#------------------------------------------------------------------------------#

  ############################
  # Message to show to users #
  ############################
  message(
    "\n",
    "================================================================================\n",
    "  \u2713  ", length(saved_pdf_paths),
    " printable report(s) saved\n",
    "================================================================================\n",
    "  Saved to:\n",
    paste0("    ", saved_pdf_paths, "\n", collapse = ""),
    "================================================================================\n"
  )

  ######################
  # Hiding output path #
  ######################
  invisible(saved_pdf_paths)

}
