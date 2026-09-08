#------------------------------------------------------------------------------#
#                                                                              #
#            Creating the week-by-week trend capture figure                    #
#                                                                              #
#------------------------------------------------------------------------------#
# About: One figure per location and season. The observed epidemic curve is    #
# drawn over the phase bands, and every forecast submission is plotted as its  #
# own trajectory: a line leaving the observed curve at the reference date and  #
# passing through that submission's forecasts in horizon order. Marks on the   #
# trajectory show how each week's asserted trend call landed: a green circle   #
# when the forecast called the same five-level trend as observed, a red        #
# triangle pointing up when it overshot, and a blue triangle pointing down     #
# when it undershot.                                                           #
#                                                                              #
# Hovering a mark focuses its whole submission: every other trajectory fades,  #
# the hovered submission's line darkens, its own step into the hovered week    #
# is re-drawn in the verdict color, the observed step for the same week is     #
# thickened, and a card states both calls, both values, and the verdict. The   #
# asserted change is always measured along the submission's own trajectory     #
# (or from the last observed value for the shortest horizon), never against    #
# observations the forecaster had not yet seen.                                #
#------------------------------------------------------------------------------#

#' Build the week-by-week trend capture figure
#'
#' @param phase_data Row-level trend/phase results carrying target_end_date,
#'   Observed, observed_trend, forecast_trend, phase, and ideally the
#'   submission columns forecast_anchor_date, prev_anchor_value, and
#'   anchor_source produced by `trendCallCalculation()`.
#' @param location Location code to display.
#' @param season Season to display; defaults to the season with the most rows.
#' @param outcome Display label for the observed series.
#' @param trend_levels The ordered five-level trend ladder.
#' @param width,height Fixed drawing size in pixels.
#' @param id_suffix Suffix appended to classes and ids so several locations can
#'   render their own copy of the figure on one page.
#'
#' @return An HTML string, or an empty string when nothing can be drawn.
#'
#' @keywords internal
#' @noRd
make_trend_phase_curve_plot <- function(phase_data,
                                        location,
                                        season       = NULL,
                                        outcome      = "Observed",
                                        trend_levels = c("large decrease",
                                                         "decrease",
                                                         "stable",
                                                         "increase",
                                                         "large increase"),
                                        width        = 680,
                                        height       = 470,
                                        id_suffix    = ""){

#------------------------------------------------------------------------------#
# Guard: usable rows -----------------------------------------------------------
#------------------------------------------------------------------------------#

  if(is.null(phase_data) || !is.data.frame(phase_data) ||
     nrow(phase_data) == 0L){
    return("")
  }

  needed <- c("target_end_date", "Observed", "observed_trend",
              "forecast_trend", "phase")
  if(!all(needed %in% names(phase_data))) return("")

  # First matching forecast-value column wins; without one the marks fall back
  # onto the observed curve and the trajectories are skipped
  forecast_column <- intersect(
    c("forecastValue", "value", "forecast_value"), names(phase_data)
  )[1]

#------------------------------------------------------------------------------#
# Selecting the rows to draw ---------------------------------------------------
#------------------------------------------------------------------------------#

  rows <- phase_data

  if(!"season" %in% names(rows))          rows$season <- "All seasons"
  if(!"horizon" %in% names(rows))         rows$horizon <- "Overall"
  if(!"is_transmission" %in% names(rows)) rows$is_transmission <- TRUE

  if("location" %in% names(rows)){
    rows <- rows[as.character(rows$location) == as.character(location), ,
                 drop = FALSE]
  }
  if(nrow(rows) == 0L) return("")

  # The season carrying the most rows is the one worth drawing
  rows$season <- as.character(rows$season)
  if(is.null(season)){
    season_counts <- table(rows$season[rows$is_transmission %in% TRUE])
    season <- if(length(season_counts) > 0){
      names(season_counts)[which.max(season_counts)]
    }else{
      rows$season[1]
    }
  }
  rows <- rows[rows$season == season, , drop = FALSE]
  if(nrow(rows) == 0L) return("")

  rows$target_end_date <- as.Date(rows$target_end_date)
  rows <- rows[!is.na(rows$target_end_date), , drop = FALSE]
  if(nrow(rows) == 0L) return("")

#------------------------------------------------------------------------------#
# Observed curve points and submission identity --------------------------------
#------------------------------------------------------------------------------#

  observed <- rows[, c("target_end_date", "Observed")]
  observed <- observed[!duplicated(observed$target_end_date), , drop = FALSE]
  observed <- observed[order(observed$target_end_date), , drop = FALSE]
  observed$Observed <- suppressWarnings(as.numeric(observed$Observed))
  if(nrow(observed) < 2L) return("")

  # Spacing between target periods, used for the previous-week anchor
  step_days <- stats::median(as.numeric(diff(observed$target_end_date)))
  if(!is.finite(step_days) || step_days <= 0) step_days <- 7

  # Submission identity: the reference date column when the backend supplied
  # it, otherwise derived from a numeric horizon; otherwise trajectories are
  # skipped and the marks stand alone.
  if(!"forecast_anchor_date" %in% names(rows)){
    horizon_steps <- suppressWarnings(as.numeric(rows$horizon))
    rows$forecast_anchor_date <- if(all(is.finite(horizon_steps))){
      rows$target_end_date - horizon_steps * step_days
    }else{
      as.Date(NA)
    }
  }
  rows$forecast_anchor_date <- as.Date(rows$forecast_anchor_date)

#------------------------------------------------------------------------------#
# Plot geometry ----------------------------------------------------------------
#------------------------------------------------------------------------------#

  pad_left   <- 58
  pad_right  <- 22
  pad_top    <- 62
  pad_bottom <- 62

  plot_left   <- pad_left
  plot_right  <- width - pad_right
  plot_top    <- pad_top
  plot_bottom <- height - pad_bottom
  plot_width  <- plot_right - plot_left
  plot_height <- plot_bottom - plot_top

  date_min  <- min(observed$target_end_date)
  date_max  <- max(observed$target_end_date)
  date_span <- as.numeric(date_max - date_min)
  if(!is.finite(date_span) || date_span <= 0) return("")

  # The ceiling covers observed and forecast values so no trajectory clips
  fc_all <- if(!is.na(forecast_column)){
    suppressWarnings(as.numeric(rows[[forecast_column]]))
  }else{
    numeric(0)
  }
  value_max <- suppressWarnings(
    max(c(observed$Observed, fc_all), na.rm = TRUE))
  if(!is.finite(value_max) || value_max <= 0) value_max <- 1
  value_ceiling <- value_max * 1.12

  # The in-plot legend goes above whichever shoulder of the season is lower
  third <- floor(nrow(observed) / 3)
  left_peak <- if(third >= 1){
    suppressWarnings(max(observed$Observed[seq_len(third)], na.rm = TRUE))
  }else{0}
  right_peak <- if(third >= 1){
    suppressWarnings(max(
      observed$Observed[seq.int(nrow(observed) - third + 1, nrow(observed))],
      na.rm = TRUE))
  }else{0}
  if(!is.finite(left_peak))  left_peak  <- 0
  if(!is.finite(right_peak)) right_peak <- 0
  legend_side <- if(left_peak <= right_peak) "left" else "right"

  scale_x <- function(d){
    plot_left + plot_width * as.numeric(d - date_min) / date_span
  }
  scale_y <- function(v){
    v[!is.finite(v)] <- 0
    plot_bottom - plot_height * pmin(v / value_ceiling, 1)
  }

#------------------------------------------------------------------------------#
# Formatting helpers -----------------------------------------------------------
#------------------------------------------------------------------------------#

  title_case <- function(x){
    vapply(as.character(x), function(one){
      if(is.na(one) || !nzchar(one)) return("")
      parts <- strsplit(one, " ", fixed = TRUE)[[1]]
      paste(toupper(substring(parts, 1, 1)), substring(parts, 2),
            sep = "", collapse = " ")
    }, character(1), USE.NAMES = FALSE)
  }

  fmt_value <- function(v){
    if(!is.finite(v)) return("n/a")
    if(abs(v) >= 100){
      format(round(v), big.mark = ",", scientific = FALSE)
    }else{
      formatC(v, format = "f", digits = 1)
    }
  }

  hz_text <- function(h){
    hn <- suppressWarnings(as.numeric(h))
    if(!is.finite(hn)) return(paste0("Horizon ", h))
    if(step_days == 7){
      if(hn == 0) "Same wk" else paste0(hn, " wk ahead")
    }else{
      d <- hn * step_days
      if(d == 0) "Same day" else
        paste0(d, if(d == 1) " day ahead" else " days ahead")
    }
  }

#------------------------------------------------------------------------------#
# Phase bands with date ranges under the headers -------------------------------
#------------------------------------------------------------------------------#
# About: Each contiguous run of a phase is one band. The phase name sits above  #
# the plot with the run's observed date range beneath it, formatted to fit the #
# band's width.                                                                 #
#------------------------------------------------------------------------------#

  phase_fill <- c(
    "Ascension" = "#E6F1FB",
    "Peak"      = "#FAEEDA",
    "Decline"   = "#E1F5EE"
  )

  per_date <- rows[!is.na(rows$phase), c("target_end_date", "phase")]
  per_date <- per_date[!duplicated(per_date$target_end_date), , drop = FALSE]
  per_date <- per_date[order(per_date$target_end_date), , drop = FALSE]

  band_svg <- ""

  if(nrow(per_date) > 0){

    run_id <- cumsum(c(TRUE, per_date$phase[-1] != per_date$phase[-nrow(per_date)]))

    for(r in unique(run_id)){

      run_dates  <- per_date$target_end_date[run_id == r]
      this_phase <- per_date$phase[run_id == r][1]
      if(!this_phase %in% names(phase_fill)) next

      band_start <- scale_x(min(run_dates) - step_days / 2)
      band_end   <- scale_x(max(run_dates) + step_days / 2)
      band_start <- max(band_start, plot_left)
      band_end   <- min(band_end, plot_right)
      if(band_end <= band_start) next

      band_w <- band_end - band_start

      range_full <- if(length(run_dates) == 1L){
        format(min(run_dates), "%b %d, %Y")
      }else{
        paste0(format(min(run_dates), "%b %d, %Y"), " \u2013 ",
               format(max(run_dates), "%b %d, %Y"))
      }
      range_short <- if(length(run_dates) == 1L){
        format(min(run_dates), "%b %d")
      }else{
        paste0(format(min(run_dates), "%b %d"), " \u2013 ",
               format(max(run_dates), "%b %d"))
      }
      range_tiny <- if(length(run_dates) == 1L){
        format(min(run_dates), "%m/%d")
      }else{
        paste0(format(min(run_dates), "%m/%d"), "\u2013",
               format(max(run_dates), "%m/%d"))
      }
      band_dates <- if(band_w >= 170){
        range_full
      }else if(band_w >= 80){
        range_short
      }else{
        range_tiny
      }

      band_label <- if(band_w >= 58){
        paste0(
          '<text x="', sprintf("%.1f", (band_start + band_end) / 2),
          '" y="', plot_top - 24,
          '" text-anchor="middle" font-size="12" fill="#52514e">',
          this_phase, '</text>',
          if(nzchar(band_dates)) paste0(
            '<text x="', sprintf("%.1f", (band_start + band_end) / 2),
            '" y="', plot_top - 10,
            '" text-anchor="middle" font-size="10" fill="#898781">',
            band_dates, '</text>'
          ) else ""
        )
      }else{""}

      band_svg <- paste0(
        band_svg,
        '<rect x="', sprintf("%.1f", band_start),
        '" y="', plot_top,
        '" width="', sprintf("%.1f", band_end - band_start),
        '" height="', plot_height,
        '" fill="', phase_fill[[this_phase]], '" opacity="0.6"/>',
        band_label
      )
    }
  }

#------------------------------------------------------------------------------#
# Observed curve and axis furniture --------------------------------------------
#------------------------------------------------------------------------------#

  curve_x <- scale_x(observed$target_end_date)
  curve_y <- scale_y(observed$Observed)
  curve_path <- paste0(
    "M ", paste(sprintf("%.1f %.1f", curve_x, curve_y), collapse = " L ")
  )

  tick_dates <- as.Date(c(date_min, date_min + date_span / 2, date_max),
                        origin = "1970-01-01")
  date_ticks <- paste0(vapply(seq_along(tick_dates), function(k){
    anchor <- c("start", "middle", "end")[k]
    paste0(
      '<text x="', sprintf("%.1f", scale_x(tick_dates[k])),
      '" y="', plot_bottom + 18,
      '" text-anchor="', anchor,
      '" font-size="12" fill="#898781">',
      format(tick_dates[k], "%b %d, %Y"), '</text>'
    )
  }, character(1)), collapse = "")

  tick_values <- c(0, value_max / 2, value_max)
  value_ticks <- paste0(vapply(tick_values, function(v){
    y <- scale_y(v)
    paste0(
      '<line x1="', plot_left, '" y1="', sprintf("%.1f", y),
      '" x2="', plot_right, '" y2="', sprintf("%.1f", y),
      '" stroke="#e1e0d9" stroke-width="1"/>',
      '<text x="', plot_left - 8, '" y="', sprintf("%.1f", y + 4),
      '" text-anchor="end" font-size="12" fill="#898781">',
      format(round(v), big.mark = ","), '</text>'
    )
  }, character(1)), collapse = "")

#------------------------------------------------------------------------------#
# Submissions: one trajectory and one set of marks each ------------------------
#------------------------------------------------------------------------------#
# About: A submission is every forecast issued on one reference date. Its      #
# trajectory leaves the observed curve at that date and steps through its      #
# forecasts in target order. Marks sit at the forecast values and carry the    #
# verdict for each week's asserted trend.                                      #
#------------------------------------------------------------------------------#

  anchor_dates <- sort(unique(
    rows$forecast_anchor_date[!is.na(rows$forecast_anchor_date)]))

  # Fallback grouping when submissions cannot be identified: every row stands
  # alone so the marks still draw, without trajectories
  standalone <- length(anchor_dates) == 0L || is.na(forecast_column)

  group_svg    <- character(0)
  emphasis_all <- character(0)

  submission_list <- if(standalone){
    list(rows)
  }else{
    lapply(seq_along(anchor_dates), function(a){
      rows[rows$forecast_anchor_date %in% anchor_dates[a], , drop = FALSE]
    })
  }

  # Date of the first horizon in each submission, in submission order. These
  # label the forecast-date selector and drive the time-lapse ordering. Early
  # in a season several submissions can share the same first eligible target
  # week, so duplicated labels are disambiguated with the submission date.
  sub_first_dates <- as.Date(vapply(submission_list, function(sub){
    vals <- if(!is.na(forecast_column)){
      suppressWarnings(as.numeric(sub[[forecast_column]]))
    }else{
      suppressWarnings(as.numeric(sub$Observed))
    }
    fk <- is.finite(vals)
    as.numeric(if(any(fk)){
      min(sub$target_end_date[fk])
    }else{
      min(sub$target_end_date)
    })
  }, numeric(1)), origin = "1970-01-01")

  sub_labels <- format(sub_first_dates, "%b %d, %Y")
  dup <- ave(seq_along(sub_labels), sub_labels, FUN = length) > 1
  if(!standalone && any(dup) && length(anchor_dates) == length(sub_labels)){
    sub_labels[dup] <- paste0(
      sub_labels[dup], " (submitted ",
      format(anchor_dates[dup], "%b %d"), ")"
    )
  }


  for(s in seq_along(submission_list)){

    sub <- submission_list[[s]]
    sub <- sub[order(sub$target_end_date), , drop = FALSE]

    sub_vals <- if(!is.na(forecast_column)){
      suppressWarnings(as.numeric(sub[[forecast_column]]))
    }else{
      suppressWarnings(as.numeric(sub$Observed))
    }

    ###########################
    # Scorable marks           #
    ###########################
    scorable <- sub$is_transmission %in% TRUE &
      !is.na(sub$observed_trend) &
      !is.na(sub$forecast_trend) &
      as.character(sub$observed_trend) %in% trend_levels &
      as.character(sub$forecast_trend) %in% trend_levels

    ###########################
    # Trajectory for this one #
    ###########################
    traj <- ""

    if(!standalone){

      keep <- is.finite(sub_vals)
      xs <- scale_x(sub$target_end_date[keep])
      ys <- scale_y(sub_vals[keep])

      # Each submission is its own chain of forecast points; it is not welded
      # onto the observed curve. The link to the last observed value appears
      # on hover for the shortest horizon, where it is the trend's true anchor.
      if(length(xs) >= 2L){

        # Every vertex on the chain gets a visible point: judged weeks carry
        # their verdict mark, and the remaining vertices get a small neutral
        # dot so weekly values can be compared along the line
        vertex_scored <- scorable[keep]
        traj_dots <- if(any(!vertex_scored)){
          paste0(
            '<g class="tp-straj', id_suffix, '" pointer-events="none">',
            paste0(
              '<circle cx="', sprintf("%.1f", xs[!vertex_scored]),
              '" cy="', sprintf("%.1f", ys[!vertex_scored]),
              '" r="3" fill="#3a3936" fill-opacity="0.8"/>',
              collapse = ""
            ),
            '</g>'
          )
        }else{""}

        traj <- paste0(
          '<path class="tp-straj', id_suffix,
          '" d="M ', paste(sprintf("%.1f %.1f", xs, ys), collapse = " L "),
          '" fill="none" stroke="#3a3936" stroke-width="1.3"',
          ' stroke-opacity="0.7" pointer-events="none"',
          ' stroke-linejoin="round" stroke-linecap="round"/>',
          traj_dots
        )
      }
    }

    marks    <- sub[scorable, , drop = FALSE]
    mark_val <- sub_vals[scorable]

    mark_svg     <- ""
    emphasis_svg <- ""

    if(nrow(marks) > 0L){

      offsets <- match(as.character(marks$forecast_trend), trend_levels) -
        match(as.character(marks$observed_trend), trend_levels)

      mark_x <- scale_x(marks$target_end_date)
      mark_y <- scale_y(mark_val)

      for(m in seq_len(nrow(marks))){

        this_offset <- offsets[m]
        captured    <- isTRUE(this_offset == 0)
        arrow_color <- if(this_offset > 0) "#e34948" else "#2a78d6"
        mark_id     <- paste0("s", s, "_", m)

        ###########################
        # Glyph: circle / triangle #
        ###########################
        glyph <- if(captured){
          paste0(
            '<circle class="tp-mark', id_suffix,
            '" data-s="', s, '" data-m="', mark_id, '" data-v="c',
            '" cx="', sprintf("%.1f", mark_x[m]),
            '" cy="', sprintf("%.1f", mark_y[m]),
            '" r="5.6" fill="#1baf7a" stroke="#fcfcfb" stroke-width="1"',
            ' style="cursor:pointer;"/>'
          )
        }else{
          tri_points <- if(this_offset > 0){
            paste0(
              sprintf("%.1f,%.1f", mark_x[m], mark_y[m] - 7.3), " ",
              sprintf("%.1f,%.1f", mark_x[m] - 6.7, mark_y[m] + 5), " ",
              sprintf("%.1f,%.1f", mark_x[m] + 6.7, mark_y[m] + 5)
            )
          }else{
            paste0(
              sprintf("%.1f,%.1f", mark_x[m], mark_y[m] + 7.3), " ",
              sprintf("%.1f,%.1f", mark_x[m] - 6.7, mark_y[m] - 5), " ",
              sprintf("%.1f,%.1f", mark_x[m] + 6.7, mark_y[m] - 5)
            )
          }
          paste0(
            '<polygon class="tp-mark', id_suffix,
            '" data-s="', s, '" data-m="', mark_id,
            '" data-v="', if(this_offset > 0) 'o' else 'u',
            '" points="', tri_points,
            '" fill="', arrow_color,
            '" stroke="#fcfcfb" stroke-width="1" style="cursor:pointer;"/>'
          )
        }

        mark_svg <- paste0(mark_svg, glyph)

        ############################################
        # Hover emphasis: the two compared steps    #
        ############################################
        # The observed step and the submission's own step cover the same
        # calendar week. The asserted change is highlighted along the
        # trajectory itself -- from the previous vertex of this submission's
        # line -- so the comparison never borrows an observation the
        # forecaster had not seen.
        prev_anchor_val <- if("prev_anchor_value" %in% names(marks)){
          suppressWarnings(as.numeric(marks$prev_anchor_value[m]))
        }else{NA_real_}
        anchor_src <- if("anchor_source" %in% names(marks)){
          as.character(marks$anchor_source[m])
        }else{NA_character_}

        obs_index <- match(marks$target_end_date[m], observed$target_end_date)

        if(!is.na(obs_index) && obs_index > 1L){

          prev_x <- scale_x(marks$target_end_date[m] - step_days)
          obs_prev_y <- scale_y(observed$Observed[obs_index - 1L])
          obs_end_x  <- mark_x[m]
          obs_end_y  <- scale_y(observed$Observed[obs_index])

          fc_prev_y <- if(is.finite(prev_anchor_val)){
            scale_y(prev_anchor_val)
          }else{
            obs_prev_y
          }
          fc_end_y <- if(is.finite(mark_val[m])) mark_y[m] else obs_end_y

          verdict_color <- if(captured) "#0f6e56" else arrow_color

          #############################
          # Card content and geometry #
          #############################
          # A single compact column: where and when, the observed pair's
          # trend, the forecast pair's trend, and the verdict. Percent
          # changes are measured within each pair.
          obs_value   <- observed$Observed[obs_index]
          prior_value <- observed$Observed[obs_index - 1L]
          fc_base     <- if(is.finite(prev_anchor_val)) prev_anchor_val else
            prior_value

          fmt_ct <- function(v){
            if(!is.finite(v)) return("?")
            format(round(v, 1), big.mark = ",", trim = TRUE,
                   scientific = FALSE)
          }
          pct_txt <- function(v, base){
            if(!is.finite(v) || !is.finite(base) || abs(base) == 0){
              return("")
            }
            paste0(" (", sprintf("%+.0f%%", 100 * (v - base) / abs(base)),
                   "  \u00b7  ", fmt_ct(base), " \u2192 ", fmt_ct(v), ")")
          }

          verdict <- if(captured){
            "Correct trend call"
          }else if(this_offset > 0){
            "Overshot \u2014 called more upward than observed"
          }else{
            "Undershot \u2014 called more downward than observed"
          }

          fc_vs <- if(identical(anchor_src, "forecast")){
            "vs its prior wk"
          }else{
            "vs last observed"
          }

          header_txt <- paste0(
            "Target ", format(marks$target_end_date[m], "%b %d, %Y"),
            "  \u00b7  Submitted ",
            if(!standalone) format(anchor_dates[s], "%b %d") else "\u2014",
            "  \u00b7  ", hz_text(marks$horizon[m])
          )
          obs_line <- paste0(
            "Observed: ",
            title_case(as.character(marks$observed_trend[m])),
            pct_txt(obs_value, prior_value)
          )
          fc_line <- paste0(
            "Forecast: ",
            title_case(as.character(marks$forecast_trend[m])),
            pct_txt(mark_val[m], fc_base), " ", fc_vs
          )

          card_w <- max(vapply(
            c(header_txt, obs_line, fc_line, verdict),
            function(t) ceiling(nchar(t) * 5.9), numeric(1))) + 34
          card_h <- 86

          card_x <- if(obs_end_x + 14 + card_w <= plot_right){
            obs_end_x + 14
          }else{
            obs_end_x - 14 - card_w
          }
          # When neither side fits inside the plot, the card takes front and
          # center instead of clipping off the edge
          if(card_x < plot_left || card_x + card_w > plot_right){
            card_x <- (plot_left + plot_right - card_w) / 2
          }
          card_y <- min(
            max(min(obs_end_y, fc_end_y) - card_h - 12, plot_top + 4),
            plot_bottom - card_h - 4
          )

          card_line <- function(y, txt, dot_color, txt_color = "#2f2e2b",
                                bold = FALSE){
            paste0(
              if(!is.na(dot_color)) paste0(
                '<circle cx="', sprintf("%.1f", card_x + 12),
                '" cy="', sprintf("%.1f", y - 4),
                '" r="3.5" fill="', dot_color, '"/>'
              ) else "",
              '<text x="', sprintf("%.1f", card_x + 22),
              '" y="', sprintf("%.1f", y),
              '" font-size="11"', if(bold) ' font-weight="600"' else '',
              ' fill="', txt_color, '">',
              htmltools::htmlEscape(txt), '</text>'
            )
          }

          hover_card <- paste0(
            '<rect x="', sprintf("%.1f", card_x),
            '" y="', sprintf("%.1f", card_y),
            '" width="', card_w, '" height="', card_h,
            '" rx="5" fill="#ffffff" fill-opacity="0.98"',
            ' stroke="#c3c2b7" stroke-width="0.5"/>',
            '<text x="', sprintf("%.1f", card_x + 10),
            '" y="', sprintf("%.1f", card_y + 18),
            '" font-size="10.5" fill="#898781">',
            htmltools::htmlEscape(header_txt), '</text>',
            card_line(card_y + 37, obs_line, "#E8912D"),
            card_line(card_y + 54, fc_line, "#2f2e2b"),
            card_line(card_y + 73, verdict, NA_character_,
                      verdict_color, bold = TRUE)
          )

          emphasis_svg <- paste0(
            emphasis_svg,
            '<g class="tp-em', id_suffix, '" data-m="', mark_id,
            '" pointer-events="none" style="display:none;">',

            # The observed pair for this week, in the observed identity color
            '<line x1="', sprintf("%.1f", prev_x),
            '" y1="', sprintf("%.1f", obs_prev_y),
            '" x2="', sprintf("%.1f", obs_end_x),
            '" y2="', sprintf("%.1f", obs_end_y),
            '" stroke="#E8912D" stroke-width="2.5" stroke-linecap="round"/>',
            '<circle cx="', sprintf("%.1f", prev_x),
            '" cy="', sprintf("%.1f", obs_prev_y),
            '" r="4.5" fill="#ffffff" stroke="#E8912D" stroke-width="2"/>',
            '<circle cx="', sprintf("%.1f", obs_end_x),
            '" cy="', sprintf("%.1f", obs_end_y),
            '" r="4.5" fill="#E8912D"/>',

            # The forecast pair: horizon and horizon - 1, in the forecast
            # identity color
            '<line x1="', sprintf("%.1f", prev_x),
            '" y1="', sprintf("%.1f", fc_prev_y),
            '" x2="', sprintf("%.1f", obs_end_x),
            '" y2="', sprintf("%.1f", fc_end_y),
            '" stroke="#2f2e2b" stroke-width="2.5" stroke-linecap="round"/>',
            '<circle cx="', sprintf("%.1f", prev_x),
            '" cy="', sprintf("%.1f", fc_prev_y),
            '" r="4.5" fill="#ffffff" stroke="#2f2e2b" stroke-width="2"/>',
            '<circle cx="', sprintf("%.1f", obs_end_x),
            '" cy="', sprintf("%.1f", fc_end_y),
            '" r="4.5" fill="#2f2e2b"/>',

            hover_card,
            '</g>'
          )
        }
      }
    }

    group_svg <- c(group_svg, paste0(
      '<g class="tp-s', id_suffix, '" data-s="', s,
      '" data-fiso="', format(sub_first_dates[s], "%Y-%m-%d"),
      '" data-flab="', htmltools::htmlEscape(sub_labels[s]), '"',
      if(s > 1L) ' style="display:none"' else '', '>',
      traj, mark_svg,
      '</g>'
    ))

    # Overlays are collected separately and painted last, above the in-plot
    # legend, so a hover card can never be covered by it
    emphasis_all <- c(emphasis_all, emphasis_svg)
  }

#------------------------------------------------------------------------------#
# In-plot legend ----------------------------------------------------------------
#------------------------------------------------------------------------------#

  legend_box_w <- 196
  legend_box_h <- 120

  legend_x <- if(identical(legend_side, "left")){
    plot_left + 10
  }else{
    plot_right - legend_box_w - 10
  }
  legend_y <- plot_top + 12

  # Each legend row is its own hover/click target. An invisible hit rectangle
  # behind the swatch and label makes the whole row responsive.
  leg_item <- function(cat, top, content){
    paste0(
      '<g class="tp-leg', id_suffix, '" data-cat="', cat,
      '" style="cursor:pointer;">',
      '<rect x="', legend_x + 3, '" y="', sprintf("%.1f", legend_y + top),
      '" width="', legend_box_w - 6,
      '" height="19" fill="#ffffff" fill-opacity="0" pointer-events="all"/>',
      content, '</g>'
    )
  }

  marker_legend <- paste0(
    '<rect x="', legend_x, '" y="', legend_y,
    '" width="', legend_box_w, '" height="', legend_box_h,
    '" rx="6" fill="#fcfcfb" fill-opacity="0.92" pointer-events="none"',
    ' stroke="#c3c2b7" stroke-width="0.5"/>',

    '<text x="', legend_x + 8, '" y="', legend_y + 16,
    '" font-size="11" font-weight="700" fill="#52514e" pointer-events="none">',
    'Week-over-Week Trend Call</text>',

    leg_item("obs", 21, paste0(
      '<line x1="', legend_x + 9, '" y1="', legend_y + 30,
      '" x2="', legend_x + 24, '" y2="', legend_y + 30,
      '" stroke="#E8912D" stroke-width="2" stroke-dasharray="5 4"/>',
      '<text x="', legend_x + 30, '" y="', legend_y + 34,
      '" font-size="12" fill="#52514e">Observed</text>'
    )),

    leg_item("traj", 40, paste0(
      '<line x1="', legend_x + 9, '" y1="', legend_y + 49,
      '" x2="', legend_x + 24, '" y2="', legend_y + 49,
      '" stroke="#3a3936" stroke-width="1.5"/>',
      '<text x="', legend_x + 30, '" y="', legend_y + 53,
      '" font-size="12" fill="#52514e">Forecast submission</text>'
    )),

    leg_item("c", 59, paste0(
      '<circle cx="', legend_x + 16, '" cy="', legend_y + 68,
      '" r="5" fill="#1baf7a"/>',
      '<text x="', legend_x + 30, '" y="', legend_y + 72,
      '" font-size="12" fill="#52514e">Correct trend call</text>'
    )),

    leg_item("o", 77, paste0(
      '<polygon points="',
      legend_x + 16, ',', legend_y + 80.5, ' ',
      legend_x + 10, ',', legend_y + 91.5, ' ',
      legend_x + 22, ',', legend_y + 91.5,
      '" fill="#e34948"/>',
      '<text x="', legend_x + 30, '" y="', legend_y + 91,
      '" font-size="12" fill="#52514e">Overshot (too upward)</text>'
    )),

    leg_item("u", 96, paste0(
      '<polygon points="',
      legend_x + 16, ',', legend_y + 110.5, ' ',
      legend_x + 10, ',', legend_y + 99.5, ' ',
      legend_x + 22, ',', legend_y + 99.5,
      '" fill="#2a78d6"/>',
      '<text x="', legend_x + 30, '" y="', legend_y + 110,
      '" font-size="12" fill="#52514e">Undershot (too downward)</text>'
    ))
  )

  outcome_label <- paste0(
    '<text transform="rotate(-90 ', 16, ' ',
    sprintf("%.1f", (plot_top + plot_bottom) / 2), ')" x="', 16,
    '" y="', sprintf("%.1f", (plot_top + plot_bottom) / 2),
    '" text-anchor="middle" font-size="12" fill="#52514e">',
    htmltools::htmlEscape(outcome), '</text>'
  )

#------------------------------------------------------------------------------#
# Forecast-date selector and time-lapse controls --------------------------------
#------------------------------------------------------------------------------#
# About: Each forecast submission is listed under the target date of its first  #
# horizon. Only the first forecast is checked (and drawn) initially. The        #
# time-lapse button walks the selected submissions in chronological order, one  #
# at a time, so the season can be replayed forecast by forecast.                #
#------------------------------------------------------------------------------#

  date_options <- if(!standalone){
    paste0(vapply(seq_along(sub_first_dates), function(k){
      paste0(
        '<label style="display:flex;align-items:center;gap:6px;font-size:14px;',
        'color:#52514e;padding:3px 4px;white-space:nowrap;cursor:pointer;">',
        '<input type="checkbox" class="tp-datebox', id_suffix,
        '" value="', k, '"', if(k == 1L) ' checked' else '', '/>',
        htmltools::htmlEscape(sub_labels[k]), '</label>'
      )
    }, character(1)), collapse = "")
  }else{""}

  sub_controls <- if(!standalone){
    paste0(
      '<div style="display:flex;flex-wrap:wrap;gap:10px;justify-content:center;',
      'align-items:center;margin:0.25rem 0 0.6rem;">',

      # Checkbox dropdown listing every submission by first-horizon date
      '<details class="tp-datepick', id_suffix,
      '" style="position:relative;">',
      '<summary style="list-style:none;display:inline-flex;align-items:center;',
      'gap:10px;border:0.5px solid #c3c2b7;background:#f1efe8;border-radius:8px;',
      'padding:10px 20px;font-size:17px;color:#52514e;cursor:pointer;',
      'line-height:1;white-space:nowrap;">',
      '<strong>Forecasts</strong>',
      '<span style="font-size:25px;line-height:1;margin-left:8px;">&#9662;</span></summary>',
      '<div style="position:absolute;left:50%;transform:translateX(-50%);',
      'z-index:30;margin-top:4px;background:#ffffff;',
      'border:0.5px solid #c3c2b7;border-radius:6px;padding:8px 10px;',
      'box-shadow:0 4px 12px rgba(0,0,0,0.12);max-height:220px;',
      'overflow-y:auto;min-width:180px;">',
      '<label style="display:flex;align-items:center;gap:6px;font-size:13px;',
      'font-weight:700;color:#52514e;padding:2px 4px;',
      'border-bottom:1px solid #e1e0d9;margin-bottom:4px;cursor:pointer;">',
      '<input type="checkbox" class="tp-dateall', id_suffix,
      '"/>All forecasts</label>',
      date_options,
      '</div></details>',

      # Time-lapse playback across the selected submissions
      '<button type="button" class="tp-playbtn', id_suffix, '"',
      ' style="display:inline-flex;align-items:center;gap:10px;',
      'border:0.5px solid #c3c2b7;background:#f1efe8;border-radius:8px;',
      'padding:10px 20px;font-size:17px;color:#52514e;cursor:pointer;',
      'line-height:1;">',
      '&#9654; <strong>Time-lapse</strong></button>',

      '<span class="tp-playlab', id_suffix,
      '" style="font-size:14px;color:#52514e;"></span>',
      '</div>'
    )
  }else{""}

#------------------------------------------------------------------------------#
# Hover and toggle behaviour ----------------------------------------------------
#------------------------------------------------------------------------------#
# About: Hovering a mark adds a focus class to the SVG (dimming everything      #
# through CSS), a highlight class to the mark's submission group (its           #
# trajectory darkens and thickens), and reveals that mark's emphasis overlay.   #
# Buttons and marks are re-wired defensively because the figure lives inside a  #
# collapsed accordion whose panels are swapped by the section's own script.     #
#------------------------------------------------------------------------------#

  toggle_script <- paste0(
    '<script>(function(){',
    'var scope=document;',
    'function svgRoot(){return scope.getElementById("tpSvg', id_suffix, '");}',
    'function ems(){return scope.querySelectorAll(".tp-em', id_suffix, '");}',
    'function subs(){return scope.querySelectorAll(".tp-s', id_suffix, '");}',

    # Legend categories: obs curve, trajectories, and marks by verdict
    'var cats=["obs","traj","c","o","u"];',
    'var catSel={};',
    'function catEls(cat){',
    'if(cat==="obs"){return scope.querySelectorAll(".tp-obs', id_suffix, '");}',
    'if(cat==="traj"){return scope.querySelectorAll(".tp-straj', id_suffix, '");}',
    'var m=scope.querySelectorAll(".tp-mark', id_suffix, '");var out=[];',
    'for(var i=0;i<m.length;i++){',
    'if(m[i].getAttribute("data-v")===cat){out.push(m[i]);}}',
    'return out;}',
    'function showCats(active){',
    'for(var i=0;i<cats.length;i++){',
    'var on=(active===null)||(active[cats[i]]===true);',
    'var els=catEls(cats[i]);',
    'for(var j=0;j<els.length;j++){els[j].style.display=on?"inline":"none";}}',
    'var lg=scope.querySelectorAll(".tp-leg', id_suffix, '");',
    'for(var k=0;k<lg.length;k++){',
    'var c=lg[k].getAttribute("data-cat");',
    'lg[k].style.opacity=(active===null||active[c]===true)?"1":"0.35";}}',
    'function selEmpty(){',
    'for(var k in catSel){if(catSel[k]===true){return false;}}return true;}',
    'function applyCats(){showCats(selEmpty()?null:catSel);}',

    'function clearFocus(){var r=svgRoot();',
    'if(r){r.classList.remove("tp-focus', id_suffix, '");}',
    'var e=ems();for(var i=0;i<e.length;i++){e[i].style.display="none";}',
    'var g=subs();for(var j=0;j<g.length;j++){',
    'g[j].classList.remove("tp-son', id_suffix, '");}}',
    'function focusOn(mk){',
    'var id=mk.getAttribute("data-m");',
    'var sid=mk.getAttribute("data-s");',
    'var r=svgRoot();if(r){r.classList.add("tp-focus', id_suffix, '");}',
    'var g=subs();for(var j=0;j<g.length;j++){',
    'if(g[j].getAttribute("data-s")===sid){',
    'g[j].classList.add("tp-son', id_suffix, '");}}',
    'var e=ems();',
    'for(var i=0;i<e.length;i++){',
    'e[i].style.display=(e[i].getAttribute("data-m")===id)?"inline":"none";}}',

    # Submission filtering by first-horizon date
    'var playTimer=null,playSeq=[],playIdx=0,playing=false,pinned=null;',
    'function dateBoxes(){return scope.querySelectorAll(".tp-datebox', id_suffix, '");}',
    'function allBox(){return scope.querySelector(".tp-dateall', id_suffix, '");}',
    'function playBtn(){return scope.querySelector(".tp-playbtn', id_suffix, '");}',
    'function playLab(){return scope.querySelector(".tp-playlab', id_suffix, '");}',
    'function selIds(){var b=dateBoxes(),o={},any=false;',
    'for(var i=0;i<b.length;i++){if(b[i].checked){o[b[i].value]=true;any=true;}}',
    'return any?o:null;}',
    'function applyFilter(){if(playing){return;}var ids=selIds();var g=subs();',
    'for(var j=0;j<g.length;j++){var s=g[j].getAttribute("data-s");',
    'g[j].style.display=(ids===null||ids[s])?"inline":"none";}}',
    'function syncAll(){var b=dateBoxes(),all=true;',
    'for(var i=0;i<b.length;i++){if(!b[i].checked){all=false;break;}}',
    'var a=allBox();if(a){a.checked=all;}}',

    # Time-lapse playback
    'function stopPlay(){playing=false;',
    'if(playTimer){clearInterval(playTimer);playTimer=null;}',
    'var pb=playBtn();if(pb){pb.innerHTML="&#9654; <strong>Time-lapse</strong>";}',
    'var pl=playLab();if(pl){pl.textContent="";}applyFilter();}',
    'function playStep(){var g=subs();',
    'if(playIdx>=playSeq.length){stopPlay();return;}',
    'for(var j=0;j<g.length;j++){g[j].style.display="none";}',
    'var cur=playSeq[playIdx];cur.style.display="inline";',
    'var pl=playLab();if(pl){pl.textContent="Forecast of "+',
    '(cur.getAttribute("data-flab")||"")+',
    '" ("+(playIdx+1)+"/"+playSeq.length+")";}',
    'playIdx++;}',
    'function startPlay(){var ids=selIds();var g=subs();playSeq=[];',
    'for(var j=0;j<g.length;j++){var s=g[j].getAttribute("data-s");',
    'if(ids===null||ids[s]){playSeq.push(g[j]);}}',
    'playSeq.sort(function(a,b){var x=a.getAttribute("data-fiso")||"";',
    'var y=b.getAttribute("data-fiso")||"";return x<y?-1:(x>y?1:0);});',
    'if(playSeq.length===0){return;}',
    'playing=true;playIdx=0;',
    'var pb=playBtn();if(pb){pb.innerHTML="&#9632; <strong>Stop</strong>";}',
    'pinned=null;clearFocus();playStep();playTimer=setInterval(playStep,900);}',

    'function wire(){',
    'var marks=scope.querySelectorAll(".tp-mark', id_suffix, '");',
    'for(var i=0;i<marks.length;i++){',
    'if(marks[i].getAttribute("data-hov")==="1"){continue;}',
    'marks[i].setAttribute("data-hov","1");',
    'marks[i].addEventListener("mouseenter",function(ev){',
    'if(!pinned){focusOn(ev.currentTarget);}});',
    'marks[i].addEventListener("mouseleave",function(){',
    'if(!pinned){clearFocus();}});',
    'marks[i].addEventListener("click",function(ev){',
    'ev.stopPropagation();',
    'if(pinned===ev.currentTarget){pinned=null;clearFocus();}',
    'else{pinned=ev.currentTarget;focusOn(ev.currentTarget);}});}',
    'var rt=svgRoot();',
    'if(rt&&rt.getAttribute("data-pinwired")!=="1"){',
    'rt.setAttribute("data-pinwired","1");',
    'document.addEventListener("click",function(){',
    'if(pinned){pinned=null;clearFocus();}});}',

    # Legend rows: hover previews only that item, click toggles it into the
    # persistent selection (several can be clicked on at once)
    'var lg=scope.querySelectorAll(".tp-leg', id_suffix, '");',
    'for(var L=0;L<lg.length;L++){',
    'if(lg[L].getAttribute("data-wired")==="1"){continue;}',
    'lg[L].setAttribute("data-wired","1");',
    'lg[L].addEventListener("mouseenter",function(ev){',
    'var c=ev.currentTarget.getAttribute("data-cat");',
    'var o={};o[c]=true;showCats(o);});',
    'lg[L].addEventListener("mouseleave",function(){applyCats();});',
    'lg[L].addEventListener("click",function(ev){',
    'var c=ev.currentTarget.getAttribute("data-cat");',
    'catSel[c]=(catSel[c]!==true);applyCats();});}',

    # Wiring the forecast-date checkboxes and the time-lapse button
    'var db=dateBoxes();',
    'for(var d=0;d<db.length;d++){',
    'if(db[d].getAttribute("data-wired")==="1"){continue;}',
    'db[d].setAttribute("data-wired","1");',
    'db[d].addEventListener("change",function(){',
    'if(playing){stopPlay();}syncAll();applyFilter();});}',
    'var ab=allBox();',
    'if(ab&&ab.getAttribute("data-wired")!=="1"){',
    'ab.setAttribute("data-wired","1");',
    'ab.addEventListener("change",function(ev){',
    'if(playing){stopPlay();}',
    'var b=dateBoxes();for(var i=0;i<b.length;i++){',
    'b[i].checked=ev.currentTarget.checked;}',
    'applyFilter();});}',
    'var pb=playBtn();',
    'if(pb&&pb.getAttribute("data-wired")!=="1"){',
    'pb.setAttribute("data-wired","1");',
    'pb.addEventListener("click",function(){',
    'if(playing){stopPlay();}else{startPlay();}});}}',
    'if(document.readyState==="loading"){',
    'document.addEventListener("DOMContentLoaded",wire);',
    '}else{wire();}',
    'setTimeout(wire,500);',
    '})();</script>'
  )

  dim_style <- paste0(
    '<style>',
    '.tp-straj', id_suffix, '{transition:opacity 0.12s,stroke 0.12s;}',
    '.tp-mark', id_suffix, '{transition:opacity 0.12s;}',
    '.tp-focus', id_suffix, ' .tp-straj', id_suffix,
    '{opacity:0.12;}',
    '.tp-focus', id_suffix, ' .tp-mark', id_suffix,
    '{opacity:0.15;}',
    '.tp-focus', id_suffix, ' .tp-obs', id_suffix,
    '{opacity:0.25;transition:opacity 0.12s;}',
    '.tp-focus', id_suffix, ' .tp-son', id_suffix, ' .tp-straj', id_suffix,
    '{opacity:1;stroke:#0b0b0b;stroke-width:2.2;}',
    '.tp-focus', id_suffix, ' .tp-son', id_suffix, ' .tp-mark', id_suffix,
    '{opacity:1;}',
    '.tp-legbox', id_suffix, '{transition:opacity 0.12s;}',
    '.tp-focus', id_suffix, ' .tp-legbox', id_suffix, '{opacity:0.15;}',
    '.tp-leg', id_suffix, '{transition:opacity 0.12s;}',
    '.tp-datepick', id_suffix, ' summary{list-style:none;}',
    '.tp-datepick', id_suffix, ' summary::-webkit-details-marker{display:none;}',
    '.tp-datepick', id_suffix, ' summary::marker{content:"";display:none;}',
    # The report accordion.css adds a blue chevron to every summary inside a
    # details.accordion; this dropdown is nested in one, so suppress it here
    '.tp-datepick', id_suffix, ' summary::after{content:none !important;}',
    '</style>'
  )

#------------------------------------------------------------------------------#
# Returning the figure ----------------------------------------------------------
#------------------------------------------------------------------------------#

  paste0(
    '<div style="margin:0 0 0.5rem;">',
    dim_style,
    '<svg id="tpSvg', id_suffix, '" width="100%" viewBox="0 0 ',
    width, ' ', height,
    '" role="img" preserveAspectRatio="xMidYMid meet"',
    ' aria-label="Forecast submission trajectories on the observed curve">',

    band_svg,
    value_ticks,

    '<line x1="', plot_left, '" y1="', plot_bottom,
    '" x2="', plot_right, '" y2="', plot_bottom,
    '" stroke="#c3c2b7" stroke-width="1"/>',

    '<g class="tp-obs', id_suffix, '" pointer-events="none"',
    ' style="display:inline;">',
    '<path d="', curve_path,
    '" fill="none" stroke="#E8912D" stroke-width="2"',
    ' stroke-dasharray="7 5" stroke-linejoin="round" stroke-linecap="round"/>',
    local({
      ok <- is.finite(observed$Observed)
      paste0(
        '<circle cx="', sprintf("%.1f", scale_x(observed$target_end_date[ok])),
        '" cy="', sprintf("%.1f", scale_y(observed$Observed[ok])),
        '" r="3.2" fill="#E8912D"/>',
        collapse = ""
      )
    }),
    '</g>',

    paste0(group_svg, collapse = ""),
    '<g class="tp-legbox', id_suffix, '">', marker_legend, '</g>',
    paste0(emphasis_all, collapse = ""),
    date_ticks,
    outcome_label,

    '<text x="', plot_left, '" y="', plot_bottom + 44,
    '" font-size="12" fill="#898781">',
    'Hover any marker to focus its submission and compare the asserted ',
    'trend with the observed trend.',
    '</text>',

    '</svg>',
    sub_controls,
    toggle_script,
    '</div>'
  )
}
