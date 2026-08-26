#' Build a collapsible legend section for the floating legend
#'
#' Constructs a collapsible section div for organizing groups of legend
#' items within the floating interactive legend. Each section includes a
#' clickable title row with a toggle arrow and a content area for the
#' legend items. Sections can be initialized expanded or collapsed.
#'
#' @param title Character. The display title for the section header.
#' @param section_id Character. A unique identifier used as a data
#'   attribute for the JavaScript expand/collapse handler.
#' @param content An `htmltools` tag or list of tags to render as the
#'   section content (the legend items).
#' @param collapsed Logical. If `TRUE` the section is collapsed on initial
#'   load. Default `FALSE`.
#' @param extra_class Character. Additional CSS class(es) to append to the
#'   outer section div. Default `NULL`.
#' @param extra_attrs Named list of additional HTML attributes to add to
#'   the outer section div. Default `list()`.
#'
#' @return An `htmltools` div tag representing the collapsible legend
#'   section.
#'
#' @keywords internal
#' @noRd

build_legend_section <- function(title, section_id, content,
                                 collapsed   = FALSE,
                                 extra_class = NULL,
                                 extra_attrs = list()) {

#------------------------------------------------------------------------------#
# Build legend section ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds one collapsible legend section as an htmltools    #
# div: a clickable title row (with a toggle arrow that flips between the       #
# collapsed and open states) sitting above a content area that holds the       #
# legend items. `collapsed` sets the initial state; `extra_class` and          #
# `extra_attrs` let callers extend the wrapper div. Returns the assembled div  #
# for insertion into the legend.                                               #
#------------------------------------------------------------------------------#

  ##################################
  # Section classes + toggle state #
  ##################################

  # Toggle arrow: right-pointing when collapsed, down-pointing when open.
  toggle_arrow <- if(collapsed) "\u25b8" else "\u25be"

  # Section div class: base class, the collapsed flag, plus any extra classes.
  section_class <- paste(
    "legend-section",
    if(collapsed) "collapsed" else NULL,
    extra_class
  )

  # Content div class: base class plus the collapsed flag.
  content_class <- paste(
    "legend-section-content",
    if(collapsed) "collapsed" else NULL
  )

  #################################
  # Assemble + return the section #
  #################################

  # do.call lets extra_attrs be spliced in dynamically
  do.call(
    htmltools::div,
    c(
      list(class = section_class),
      extra_attrs,
      list(

        # Title row
        htmltools::div(
          class          = "legend-section-title clickable-title",
          `data-section` = section_id,
          htmltools::span(title),
          htmltools::tags$span(class = "section-toggle", toggle_arrow)
        ),

        # Content area
        htmltools::div(
          class                  = content_class,
          `data-section-content` = section_id,
          content
        )
      )
    )
  )
}
