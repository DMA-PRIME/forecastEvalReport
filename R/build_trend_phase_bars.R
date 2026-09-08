#' Build the intuitive trend/phase bar figure
#'
#' Renders one trend/phase panel as three columns of horizontal bars -- one
#' column per epidemic phase, one bar per observed trend label -- instead of a
#' grid of numbers. The value is printed at the end of its own bar and the
#' sample size sits beneath it, so nothing has to be cross-referenced against a
#' header to be understood: a longer, greener bar is better, and a phase where
#' the model struggles shows up as a column of short red bars.
#'
#' Bars are plain HTML and CSS. Nothing here needs Plotly or any JavaScript, so
#' the figure inherits whatever panel-switching script already governs the
#' `trend-phase-panel` divs it is placed inside, and it prints correctly in the
#' PDF export where a canvas-based chart would not.
#'
#' Color runs on a three-stop traffic light keyed to `good_direction`. When
#' higher values are better the scale runs red through amber to green as the
#' value rises; when lower values are better it runs the other way. Set
#' `good_direction = "none"` for a metric with no natural direction, which draws
#' every bar in the report's purple instead of implying a judgment.
#'
#' @param values Numeric vector of cell values, one per trend label, in the same
#'   order as `trend_labels`. `NA` renders as an absent bar with a dash.
#' @param counts Integer vector of sample sizes parallel to `values`. Pass `NULL`
#'   to omit the sample-size line.
#' @param trend_labels Character vector of trend label names, top to bottom.
#' @param scale_max Numeric upper bound for bar length. Bars are drawn as a
#'   share of this, so every panel in a figure stays comparable. Defaults to the
#'   largest finite value present, or `100` when all values are missing.
#' @param suffix Character string appended to each printed value, such as `"%"`.
#'   Defaults to `""`.
#' @param digits Number of decimal places for the printed value. Defaults to `1`.
#' @param good_direction One of `"higher"` (default), `"lower"`, or `"none"`,
#'   setting which end of the range is colored green.
#' @param bar_height Bar thickness in pixels. Defaults to `18`.
#' @param diverging Logical. Draw bars outward from a center line instead of
#'   from the left edge? Defaults to `FALSE`. Use `TRUE` for signed metrics
#'   such as bias, where `scale_max` is read as the half-range.
#'
#' @return A character string of HTML, safe to paste into a panel div.
#'
#' @keywords internal
#' @noRd
build_trend_phase_bars <- function(values,
                                   counts         = NULL,
                                   trend_labels,
                                   scale_max      = NULL,
                                   suffix         = "",
                                   digits         = 1,
                                   good_direction = c("higher", "lower", "none"),
                                   bar_height     = 18,
                                   diverging      = FALSE){

#------------------------------------------------------------------------------#
# Resolving the scale ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: A single shared maximum across all three phase columns is what makes    #
# the columns comparable. Falling back to the largest observed value keeps a      #
# metric with no fixed ceiling, such as WIS, from drawing every bar at full       #
# width.                                                                         #
#------------------------------------------------------------------------------#

  ###########################
  # Direction of "good"     #
  ###########################
  good_direction <- match.arg(good_direction)

  ###########################
  # Upper bound for bars    #
  ###########################
  finite_values <- values[is.finite(values)]

  # A diverging scale is symmetric, so it is sized by the largest magnitude
  if(isTRUE(diverging) && length(finite_values) > 0 &&
     (is.null(scale_max) || !is.finite(scale_max) || scale_max <= 0)){
    scale_max <- max(abs(finite_values))
  }

  if(is.null(scale_max) || !is.finite(scale_max) || scale_max <= 0){

    scale_max <- if(length(finite_values) > 0){

      max(finite_values)

    }else{100}

  }

  # A zero-width scale would divide by zero below
  if(!is.finite(scale_max) || scale_max <= 0) scale_max <- 1

#------------------------------------------------------------------------------#
# Color for a single bar -------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Three stops rather than a continuous gradient, because a five-stop      #
# ramp reads as decoration while three reads as good, middling, and poor. The     #
# thresholds sit at a third and two thirds of the scale.                          #
#------------------------------------------------------------------------------#

  ###########################
  # Traffic-light selection #
  ###########################
  bar_color <- function(value){

    # No value means no bar to color
    if(!is.finite(value)) return("#e0e0e0")

    # A metric with no natural direction keeps the report's purple
    if(identical(good_direction, "none")) return("#8a6cc4")

    # A signed metric is colored by which way it leans, since the sign is the
    # finding and a magnitude near zero is the good outcome
    if(isTRUE(diverging)){
      return(if(value > 0) "#c1502e" else if(value < 0) "#2c6fa8" else "#9e9e9e")
    }

    # Position on the scale, clamped into range
    position <- min(max(value / scale_max, 0), 1)

    # Flipping the scale when lower values are better
    if(identical(good_direction, "lower")) position <- 1 - position

    if(position >= 0.667){

      "#2e8b57"        # green: doing well

    }else if(position >= 0.334){

      "#d9a13b"        # amber: middling

    }else{

      "#c1502e"        # red: struggling

    }

  }

#------------------------------------------------------------------------------#
# Building one bar row per trend label -----------------------------------------
#------------------------------------------------------------------------------#
# About: Each row is a label, a track, and a filled bar with the value printed    #
# just past its end. Absent values draw an empty track with a dash so the row      #
# still occupies its place and the columns stay aligned across phases.            #
#------------------------------------------------------------------------------#

  ###########################
  # Assembling the bar rows #
  ###########################
  rows <- vapply(seq_along(trend_labels), function(i){

    this_value <- if(i <= length(values)) values[i] else NA_real_
    this_count <- if(!is.null(counts) && i <= length(counts)) counts[i] else NA

    ###############################
    # Rows with no value to show  #
    ###############################
    if(!is.finite(this_value)){

      return(paste0(
        '<div style="display:flex;align-items:center;gap:8px;margin:0 0 9px;">',
        '<div style="flex:1 1 auto;height:', bar_height,
        'px;background:#f4f4f4;border-radius:3px;"></div>',
        '<div style="flex:0 0 62px;font-size:12px;color:#999;',
        'text-align:right;white-space:nowrap;">&mdash;</div>',
        '</div>'
      ))

    }

    ###############################
    # Rows with a value to show   #
    ###############################

    ###############################
    # Bars that grow from center  #
    ###############################
    if(isTRUE(diverging)){

      # Half-width share of the track, floored so a small value stays visible
      half_percent <- max(min(50 * abs(this_value) / scale_max, 50), 1)

      # Left offset places the bar on the correct side of the center line
      left_percent <- if(this_value >= 0) 50 else 50 - half_percent

      count_html <- if(is.finite(this_count) && this_count > 0){
        paste0(
          '<div style="font-size:10px;color:#999;line-height:1.2;">n = ',
          as.integer(this_count), '</div>'
        )
      }else{""}

      return(paste0(
        '<div style="display:flex;align-items:center;gap:8px;margin:0 0 9px;">',
        '<div style="flex:1 1 auto;height:', bar_height,
        'px;background:#f4f4f4;border-radius:3px;position:relative;">',
        '<div style="position:absolute;left:50%;top:0;bottom:0;width:1px;',
        'background:#bbb;"></div>',
        '<div style="position:absolute;top:0;bottom:0;left:',
        sprintf("%.1f", left_percent), '%;width:',
        sprintf("%.1f", half_percent), '%;background:', bar_color(this_value),
        ';border-radius:2px;"></div>',
        '</div>',
        '<div style="flex:0 0 62px;text-align:right;white-space:nowrap;">',
        '<div style="font-size:13px;font-weight:700;color:#333;line-height:1.2;">',
        if(this_value > 0) "+" else "",
        formatC(round(this_value, digits), format = "f", digits = digits),
        suffix, '</div>',
        count_html,
        '</div>',
        '</div>'
      ))

    }

    # Bar width as a share of the shared scale, floored so a tiny value is
    # still visible as a bar rather than vanishing into the track
    fill_percent <- max(
      min(100 * this_value / scale_max, 100),
      1.5
    )

    # Sample size line, omitted when counts were not supplied
    count_html <- if(is.finite(this_count) && this_count > 0){

      paste0(
        '<div style="font-size:10px;color:#999;line-height:1.2;">n = ',
        as.integer(this_count), '</div>'
      )

    }else{""}

    paste0(
      '<div style="display:flex;align-items:center;gap:8px;margin:0 0 9px;">',
      '<div style="flex:1 1 auto;height:', bar_height,
      'px;background:#f4f4f4;border-radius:3px;overflow:hidden;">',
      '<div style="width:', sprintf("%.1f", fill_percent),
      '%;height:100%;background:', bar_color(this_value),
      ';border-radius:3px;"></div>',
      '</div>',
      '<div style="flex:0 0 62px;text-align:right;white-space:nowrap;">',
      '<div style="font-size:13px;font-weight:700;color:#333;line-height:1.2;">',
      formatC(round(this_value, digits), format = "f", digits = digits),
      suffix, '</div>',
      count_html,
      '</div>',
      '</div>'
    )

  }, character(1))

#------------------------------------------------------------------------------#
# Returning the bar block ------------------------------------------------------
#------------------------------------------------------------------------------#

  paste0(rows, collapse = "")

}
