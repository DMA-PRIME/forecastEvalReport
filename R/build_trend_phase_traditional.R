#' Build the dedicated Trend/Phase-Specific Performance section
#'
#' @keywords internal
#' @noRd
build_trend_phase_traditional <- function(performance_summary,
                                          location_codes,
                                          location_labels,
                                          phase_data = NULL,
                                          status_message = NULL,
                                          time_step = 7){

#------------------------------------------------------------------------------#
# Handling unavailable results ------------------------------------------------#
#------------------------------------------------------------------------------#

  title <- "Trend/Phase-Specific Performance"
  if(is.null(performance_summary) || !is.data.frame(performance_summary) ||
     nrow(performance_summary) == 0L){
    if(is.null(status_message) || !nzchar(status_message)){
      status_message <- paste(
        "No trend/phase performance tables could be calculated. At least two",
        "consecutive observed target periods and median forecasts are required."
      )
    }
    return(htmltools::tags$details(
      class = "accordion",
      htmltools::tags$summary(htmltools::tags$strong(title)),
      htmltools::div(class = "accordion-body",
        htmltools::tags$p(
          style = "font-size:14px;line-height:1.65;color:#555;margin:0;",
          status_message
        )
      )
    ))
  }

  keep <- location_codes %in% unique(performance_summary$location)
  location_codes <- location_codes[keep]
  location_labels <- location_labels[keep]
  if(length(location_codes) == 0L) return(NULL)

#------------------------------------------------------------------------------#
# Preparing controls and table dimensions -------------------------------------#
#------------------------------------------------------------------------------#

  location_options <- paste0(vapply(seq_along(location_codes), function(i){
    paste0('<option value="', htmltools::htmlEscape(location_codes[i]), '">',
           htmltools::htmlEscape(location_labels[i]), '</option>')
  }, character(1)), collapse = "")

  horizon_values <- unique(as.character(performance_summary$horizon))
  horizon_values <- horizon_values[horizon_values != "Overall"]
  numeric_horizons <- suppressWarnings(as.numeric(horizon_values))
  if(all(!is.na(numeric_horizons))){
    horizon_values <- horizon_values[order(numeric_horizons)]
  }else{
    horizon_values <- sort(horizon_values)
  }

  phase_values <- c("Ascension", "Peak", "Decline")
  trend_values <- c("large increase", "increase", "stable",
                    "decrease", "large decrease")
  trend_labels <- c("Large Increase", "Increase", "Stable",
                    "Decrease", "Large Decrease")

#------------------------------------------------------------------------------#
# Formatting labels and metric cells ------------------------------------------#
#------------------------------------------------------------------------------#

  row_label <- function(location, breakdown, group){
    label <- if(breakdown == "trend"){
      trend_labels[match(group, trend_values)]
    }else{
      group
    }
    if(is.na(label) || !nzchar(label)) label <- group

    date_html <- ""
    if(breakdown == "phase"){
      date_range <- format_trend_phase_date_ranges(phase_data, location, group)
      if(nzchar(date_range)) date_html <- paste0(
        '<div style="font-size:11px;color:#777;margin-top:2px;white-space:normal;">',
        date_range, '</div>'
      )
    }
    if(breakdown == "overall"){
      date_html <- '<div style="font-size:11px;color:#777;margin-top:2px;">All eligible transmission dates</div>'
    }

    paste0('<div style="font-size:15px;font-weight:700;color:#555;">',
           label, '</div>', date_html)
  }

  get_row <- function(location, horizon, breakdown, group){
    performance_summary[
      performance_summary$location == location &
        as.character(performance_summary$horizon) == as.character(horizon) &
        performance_summary$breakdown == breakdown &
        performance_summary$group == group, , drop = FALSE
    ]
  }

  metric_cell <- function(value, n, suffix = "", digits = 1){
    display <- if(length(value) == 0L || is.na(value) || !is.finite(value)){
      "&mdash;"
    }else{
      paste0(formatC(value, format = "f", digits = digits), suffix)
    }
    count <- if(length(n) && !is.na(n) && n > 0L) paste0("n = ", n) else ""
    paste0(
      '<td style="padding:13px;text-align:center;vertical-align:middle;">',
      '<div style="font-size:14px;font-weight:700;color:#522D80;white-space:nowrap;">',
      display, '</div><div style="font-size:11px;color:#888;margin-top:2px;">',
      count, '</div></td>'
    )
  }

  metric_values <- function(hit, metric){
    if(metric == "agreement"){
      list(value = if(nrow(hit)) hit$agreement_pct[1] else NA_real_,
           n = if(nrow(hit)) hit$agreement_n[1] else 0L,
           suffix = "%", digits = 1)
    }else{
      list(value = if(nrow(hit)) hit$trend_mae[1] else NA_real_,
           n = if(nrow(hit)) hit$trend_mae_n[1] else 0L,
           suffix = "", digits = 2)
    }
  }

#------------------------------------------------------------------------------#
# Building overall tables ------------------------------------------------------#
#------------------------------------------------------------------------------#

  overall_table <- function(location, breakdown){
    groups <- if(breakdown == "phase") phase_values else trend_values
    first_title <- if(breakdown == "phase") "Observed Phase" else "Observed Trend"

    group_rows <- c("Overall", groups)
    rows <- paste0(vapply(group_rows, function(group){
      row_breakdown <- if(group == "Overall") "overall" else breakdown
      hit <- get_row(location, "Overall", row_breakdown, group)
      agreement <- metric_values(hit, "agreement")
      difference <- metric_values(hit, "difference")
      paste0(
        '<tr style="border-bottom:1px solid #e6e6e6;">',
        '<td style="padding:13px 16px;text-align:center;vertical-align:middle;border-right:1px solid #e0e0e0;">',
        row_label(location, row_breakdown, group), '</td>',
        metric_cell(agreement$value, agreement$n, agreement$suffix,
                    agreement$digits),
        metric_cell(difference$value, difference$n, difference$suffix,
                    difference$digits), '</tr>'
      )
    }, character(1)), collapse = "")

    paste0(
      '<div style="overflow-x:auto;">',
      '<table style="width:100%;border-collapse:collapse;border-top:1px solid #333;border-bottom:1px solid #333;">',
      '<thead><tr style="border-bottom:1px solid #333;">',
      '<th class="sum-th" style="width:190px;text-align:center;font-size:12px;border-right:1px solid #e0e0e0;">', first_title, '</th>',
      '<th class="sum-th" style="font-size:12px;border-right:1px solid #e0e0e0;">Trend Label Agreement<div class="sum-th-sub" style="color:#C9B8E8;">Higher Is Better</div></th>',
      '<th class="sum-th" style="font-size:12px;">Mean Absolute Trend Difference<div class="sum-th-sub" style="color:#C9B8E8;">Rate Change per 100,000 &middot; Lower Is Better</div></th>',
      '</tr></thead><tbody>', rows, '</tbody></table></div>'
    )
  }
#------------------------------------------------------------------------------#
# Building horizon matrices ---------------------------------------------------#
#------------------------------------------------------------------------------#

  horizon_table <- function(location, metric, breakdown){
    groups <- if(breakdown == "phase") phase_values else trend_values
    first_title <- if(breakdown == "phase") "Observed Phase" else "Observed Trend"
    header <- paste0(vapply(horizon_values, function(horizon){
      paste0('<th class="sum-th" style="font-size:12px;">Horizon ',
             htmltools::htmlEscape(horizon), '</th>')
    }, character(1)), collapse = "")

    group_rows <- c("Overall", groups)
    rows <- paste0(vapply(group_rows, function(group){
      row_breakdown <- if(group == "Overall") "overall" else breakdown
      cells <- paste0(vapply(horizon_values, function(horizon){
        hit <- get_row(location, horizon, row_breakdown, group)
        values <- metric_values(hit, metric)
        metric_cell(values$value, values$n, values$suffix, values$digits)
      }, character(1)), collapse = "")

      paste0(
        '<tr style="border-bottom:1px solid #e6e6e6;">',
        '<td style="padding:13px 16px;text-align:center;vertical-align:middle;border-right:1px solid #e0e0e0;">',
        row_label(location, row_breakdown, group), '</td>', cells, '</tr>'
      )
    }, character(1)), collapse = "")

    paste0(
      '<div style="overflow-x:auto;">',
      '<table style="width:100%;border-collapse:collapse;border-top:1px solid #333;border-bottom:1px solid #333;">',
      '<thead><tr style="border-bottom:1px solid #333;">',
      '<th class="sum-th" style="width:190px;text-align:center;border-right:1px solid #e0e0e0;font-size:12px;">',
      first_title, '</th>', header, '</tr></thead><tbody>', rows,
      '</tbody></table></div>'
    )
  }

#------------------------------------------------------------------------------#
# Building the per-location panel groups ---------------------------------------#
#------------------------------------------------------------------------------#
# About: Rather than one top-level location switch that swaps entire panels,   #
# each figure and sub-accordion carries its own location filter. Three panel   #
# groups are built (summary figure, week-by-week figure, and horizon tables),  #
# each holding one panel per location and switched independently by its own    #
# dropdown, so every figure and table is filterable by location in place.      #
#------------------------------------------------------------------------------#

  multi_loc <- length(location_codes) > 1L

  loc_filter <- function(select_id){
    if(!multi_loc) return("")
    paste0(
      '<div style="display:flex;justify-content:center;align-items:center;',
      'gap:16px;margin:0.25rem 0 1rem;">',
      '<label for="', select_id, '" style="font-size:15px;font-weight:700;',
      'color:#555;line-height:1;">Location</label>',
      '<select id="', select_id, '" class="geo-select"',
      ' style="font-size:15px;padding:6px 12px;border-radius:8px;">',
      location_options, '</select>',
      '</div>'
    )
  }

  loc_panel <- function(location, i, cls, content){
    paste0(
      '<div class="', cls, '" data-location="',
      htmltools::htmlEscape(location), '" style="',
      if(i == 1L) '' else 'display:none;', '">',
      content,
      '</div>'
    )
  }

  summary_panels <- paste0(vapply(seq_along(location_codes), function(i){
    loc_panel(location_codes[i], i, "tps-summary-panel",
      make_trend_capture_summary_plot(
        phase_data = phase_data,
        location   = location_codes[i],
        id_suffix  = paste0("L", i),
        time_step  = time_step
      )
    )
  }, character(1)), collapse = "")

  detail_panels <- paste0(vapply(seq_along(location_codes), function(i){
    loc_panel(location_codes[i], i, "tps-detail-panel",
      make_trend_phase_curve_plot(
        phase_data = phase_data,
        location   = location_codes[i],
        id_suffix  = paste0("L", i)
      )
    )
  }, character(1)), collapse = "")

  # ---- Magnitude missed: offset between forecast and observed % change --------#
  # For every eligible forecast-target pair the forecasted week-over-week       #
  # percent change (against its own anchor, as in the figures) is compared      #
  # with the observed percent change for the same target week. The difference,  #
  # in percentage points, is the magnitude missed: positive = overshot the      #
  # change, negative = undershot it.                                            #

  mm_trend_values <- c("large increase", "increase", "stable",
                       "decrease", "large decrease")
  mm_trend_labels <- c("Large Increase", "Increase", "Stable",
                       "Decrease", "Large Decrease")
  mm_phases <- c("Ascension", "Peak", "Decline")

  mm_horizons <- local({
    hu <- unique(as.character(phase_data$horizon))
    hu <- hu[!is.na(hu) & nzchar(hu)]
    hu <- hu[order(suppressWarnings(as.numeric(hu)))]
    c("Overall", hu)
  })

  offset_summary <- local({
    # Tibbles are strict about logical subscripts, and the real pipeline may
    # name the forecast column differently, so everything below is resolved
    # defensively: plain data.frame, column lookups with NA fallbacks, and
    # every derived vector guaranteed to match nrow()
    pd <- as.data.frame(phase_data, stringsAsFactors = FALSE)
    if(!"horizon" %in% names(pd))  pd$horizon  <- "Overall"
    if(!"location" %in% names(pd)) pd$location <- location_codes[1]

    # Same forecast-column resolution as the week-by-week figure
    fc_col <- intersect(c("forecastValue", "value", "forecast_value"),
                        names(pd))[1]

    num_col <- function(d, nm){
      if(length(nm) == 1L && !is.na(nm) && nm %in% names(d)){
        suppressWarnings(as.numeric(d[[nm]]))
      }else{
        rep(NA_real_, nrow(d))
      }
    }

    out <- list()
    if(!is.na(fc_col)) for(loc in location_codes){
      rows <- pd[as.character(pd$location) == as.character(loc), ,
                 drop = FALSE]
      if(!nrow(rows)) next
      rows$target_end_date <- as.Date(rows$target_end_date)

      obs_series <- rows[!duplicated(rows$target_end_date),
                         c("target_end_date", "Observed")]
      obs_series <- obs_series[order(obs_series$target_end_date), ,
                               drop = FALSE]
      idx <- match(rows$target_end_date, obs_series$target_end_date)
      prev_obs <- ifelse(!is.na(idx) & idx > 1L,
                         suppressWarnings(as.numeric(
                           obs_series$Observed[pmax(idx - 1L, 1L)])),
                         NA_real_)

      obs_now <- num_col(rows, "Observed")
      obs_pct <- ifelse(is.finite(prev_obs) & prev_obs > 0 & is.finite(obs_now),
                        (obs_now - prev_obs) / prev_obs * 100, NA_real_)

      # The forecast change anchors to the submission's own previous value
      # when available, matching the figure; otherwise to the last observed
      base <- num_col(rows, "prev_anchor_value")
      base <- ifelse(is.finite(base), base, prev_obs)
      fcv  <- num_col(rows, fc_col)
      fc_pct <- ifelse(is.finite(base) & base > 0 & is.finite(fcv),
                       (fcv - base) / base * 100, NA_real_)

      off  <- fc_pct - obs_pct
      keep <- is.finite(off) & !is.na(rows$phase) &
        !is.na(rows$observed_trend)
      keep[is.na(keep)] <- FALSE
      if(!any(keep)) next
      rows <- rows[keep, , drop = FALSE]
      off  <- off[keep]

      hz <- as.character(rows$horizon)
      for(h in mm_horizons){
        sel <- if(h == "Overall") rep(TRUE, nrow(rows)) else hz == h
        if(!any(sel)) next
        sub_ph <- as.character(rows$phase[sel])
        sub_tr <- tolower(as.character(rows$observed_trend[sel]))
        o      <- off[sel]
        for(ph in mm_phases){
          for(tr in mm_trend_values){
            m <- sub_ph == ph & sub_tr == tr
            if(!any(m)) next
            v <- o[m]
            out[[length(out) + 1L]] <- data.frame(
              location = loc, horizon = h, phase = ph, observed_trend = tr,
              median = stats::median(v), minimum = min(v), maximum = max(v),
              n = sum(m), stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    if(length(out)) do.call(rbind, out) else data.frame(
      location = character(0), horizon = character(0), phase = character(0),
      observed_trend = character(0), median = numeric(0), minimum = numeric(0),
      maximum = numeric(0), n = integer(0), stringsAsFactors = FALSE
    )
  })

  mm_phase_header <- function(location, phase){
    date_range <- format_trend_phase_date_ranges(phase_data, location, phase)
    date_html <- if(nzchar(date_range)) paste0(
      '<div style="font-size:11px;font-weight:600;color:#666;margin-top:3px;white-space:normal;">',
      date_range, '</div>'
    ) else ""
    paste0(
      phase, date_html,
      '<div class="sum-th-sub" style="color:#C9B8E8;">Median % Off (Range)</div>'
    )
  }

  mm_fmt <- function(x) sprintf("%+.1f", x)

  mm_cell <- function(location, horizon, trend, phase){
    hit <- offset_summary[
      offset_summary$location == location &
        offset_summary$horizon == horizon &
        offset_summary$observed_trend == trend &
        offset_summary$phase == phase, , drop = FALSE
    ]

    if(nrow(hit) == 0L || is.na(hit$median[1])){
      return('<td style="padding:13px;text-align:center;color:#777;">&mdash;</td>')
    }

    paste0('
      <td style="padding:13px;text-align:center;vertical-align:middle;">
        <div style="font-size:14px;font-weight:700;color:#522D80;white-space:nowrap;">',
        mm_fmt(hit$median[1]), '%</div>
        <div style="font-size:12px;color:#666;white-space:nowrap;">(',
        mm_fmt(hit$minimum[1]), ' &ndash; ', mm_fmt(hit$maximum[1]), ')</div>
        <div style="font-size:11px;color:#888;margin-top:2px;">n = ',
        as.integer(hit$n[1]), '</div>
      </td>')
  }

  mm_panel_index <- 0L
  table_panels <- paste0(vapply(seq_along(location_codes), function(i){
    paste0(vapply(mm_horizons, function(horizon){
      mm_panel_index <<- mm_panel_index + 1L
      body_rows <- paste0(vapply(seq_along(mm_trend_values), function(j){
        cells <- paste0(vapply(mm_phases, function(phase){
          mm_cell(location_codes[i], horizon, mm_trend_values[j], phase)
        }, character(1)), collapse = "")
        paste0(
          '<tr', if(j < length(mm_trend_values))
            ' style="border-bottom:1px solid #e6e6e6;"' else '', '>',
          '<td style="padding:13px 16px;font-size:15px;font-weight:700;color:#555;',
          'border-right:1px solid #e0e0e0;white-space:nowrap;text-align:center;vertical-align:middle;">',
          mm_trend_labels[j], '</td>', cells, '</tr>'
        )
      }, character(1)), collapse = "")

      paste0('
        <div class="tps-table-panel"
             data-location="', htmltools::htmlEscape(location_codes[i]), '"
             data-horizon="', htmltools::htmlEscape(horizon), '"
             style="', if(mm_panel_index == 1L) '' else 'display:none;', '">
          <div style="overflow-x:auto;">
            <table style="width:100%;border-collapse:collapse;border-top:1px solid #333;border-bottom:1px solid #333;">
              <thead>
                <tr style="border-bottom:1px solid #333;">
                  <th class="sum-th" style="width:170px;border-right:1px solid #e0e0e0;text-align:center;">Observed Trend</th>
                  <th class="sum-th">', mm_phase_header(location_codes[i], "Ascension"), '</th>
                  <th class="sum-th" style="background:#f7f4fc;color:#522D80;">', mm_phase_header(location_codes[i], "Peak"), '</th>
                  <th class="sum-th">', mm_phase_header(location_codes[i], "Decline"), '</th>
                </tr>
              </thead>
              <tbody>', body_rows, '</tbody>
            </table>
          </div>
        </div>')
    }, character(1)), collapse = "")
  }, character(1)), collapse = "")

  mm_horizon_options <- paste0(vapply(mm_horizons, function(horizon){
    label <- if(horizon == "Overall") "Overall" else paste("Horizon", horizon)
    paste0('<option value="', htmltools::htmlEscape(horizon), '">',
           htmltools::htmlEscape(label), '</option>')
  }, character(1)), collapse = "")

  mm_filter_row <- paste0(
    '<div style="display:flex;justify-content:center;align-items:center;',
    'gap:18px;flex-wrap:wrap;margin:0 0 1.75rem;">',
    if(multi_loc) paste0(
      '<div style="display:flex;align-items:center;gap:16px;">',
      '<label for="tpsLocTables" style="font-size:15px;font-weight:700;',
      'color:#555;line-height:1;">Location</label>',
      '<select id="tpsLocTables" class="geo-select"',
      ' style="font-size:15px;padding:6px 12px;border-radius:8px;">',
      location_options, '</select>',
      '</div>'
    ) else "",
    '<div style="display:flex;align-items:center;gap:16px;">',
    '<label for="tpsHorTables" style="font-size:15px;font-weight:700;',
    'color:#555;line-height:1;">Forecast horizon</label>',
    '<select id="tpsHorTables" class="geo-select"',
    ' style="font-size:15px;padding:6px 12px;border-radius:8px;">',
    mm_horizon_options, '</select>',
    '</div>',
    '</div>'
  )

#------------------------------------------------------------------------------#
# Detailed methods accordion ----------------------------------------------------
#------------------------------------------------------------------------------#

  methods_accordion <- paste0(
    '<details class="accordion" style="margin-top:1rem;">',
    '<summary><strong>Detailed Methods (Trend/Phase Performance)</strong></summary>',
    '<div class="accordion-body" style="font-size:14px;line-height:1.7;color:#333;">',
    '<p><strong>Trend labels.</strong> Forecast and observed counts are converted to rates per 100,000. ',
    'The <strong>observed</strong> change for a target week is the observed value minus the observed value one week earlier. ',
    'The <strong>forecast</strong> change is taken within a single submission: the value at horizon <em>h</em> minus the value at horizon <em>h</em>&nbsp;&#8722;&nbsp;1 issued on the same reference date, so it is the week-over-week change the forecast itself asserted. ',
    'The shortest horizon in a submission has no <em>h</em>&nbsp;&#8722;&nbsp;1 and anchors to the last observed value available at forecast time. ',
    trend_methods_html(), '</p>',
    '<p><strong>Trend Label Agreement.</strong> A forecast is correct when its five-level trend label exactly matches the observed label for the same target date. The percentage is 100 times the number of matches divided by the number of eligible forecast-target pairs.</p>',
    '<p><strong>Mean Absolute Trend Difference.</strong> The absolute difference between forecast and observed weekly rate changes is calculated for each eligible pair and then averaged. Its unit is weekly rate-change points per 100,000 population; lower is better.</p>',
    '<p><strong>Phase breakdown.</strong> Peak is the contiguous period around the observed seasonal maximum whose values remain within the configured peak window. Earlier dates are Ascension and later dates are Decline.</p>',
    '<p><strong>Observed-trend breakdown.</strong> Forecast performance is grouped by the trend label assigned to the observed data: Large Increase, Increase, Stable, Decrease, or Large Decrease. This shows which observed directions are easiest or hardest to forecast.</p>',
    '<p><strong>Horizons and sample size.</strong> Overall pools eligible forecast-target pairs across horizons. Horizon tables retain only the indicated horizon. The displayed <em>n</em> can differ when forecasts, consecutive weeks, or observations are missing.</p>',
    '</div></details>'
  )

#------------------------------------------------------------------------------#
# Location-switch script ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: One small script wires all three dropdowns. Each dropdown switches     #
# only its own panel class, so the summary figure, week-by-week figure, and     #
# horizon tables filter independently. Wiring is guarded so re-runs are safe.   #
#------------------------------------------------------------------------------#

  first_loc_js <- gsub('"', '\\\\"', as.character(location_codes[1]), fixed = TRUE)

  switch_script <- paste0(
      '<script>(function(){',
      'function wireLoc(selId,panelCls){',
      'var sel=document.getElementById(selId);',
      'if(!sel||sel.getAttribute("data-wired")==="1"){return;}',
      'sel.setAttribute("data-wired","1");',
      'function apply(){var v=sel.value;',
      'var ps=document.querySelectorAll("."+panelCls);',
      'for(var i=0;i<ps.length;i++){',
      'ps[i].style.display=',
      'ps[i].getAttribute("data-location")===v?"block":"none";}}',
      'sel.addEventListener("change",apply);apply();}',

      # The magnitude-missed table filters on location and horizon together;
      # in single-location reports the location select is absent and the
      # first (only) location is used
      'function wireTables(){',
      'var hs=document.getElementById("tpsHorTables");',
      'if(!hs||hs.getAttribute("data-wired")==="1"){return;}',
      'hs.setAttribute("data-wired","1");',
      'var ls=document.getElementById("tpsLocTables");',
      'function apply(){',
      'var lv=ls?ls.value:"', first_loc_js, '";',
      'var hv=hs.value;',
      'var ps=document.querySelectorAll(".tps-table-panel");',
      'for(var i=0;i<ps.length;i++){',
      'ps[i].style.display=',
      '(ps[i].getAttribute("data-location")===lv&&',
      'ps[i].getAttribute("data-horizon")===hv)?"block":"none";}}',
      'if(ls&&ls.getAttribute("data-wired")!=="1"){',
      'ls.setAttribute("data-wired","1");',
      'ls.addEventListener("change",apply);}',
      'hs.addEventListener("change",apply);apply();}',

      'function wireAll(){',
      'wireLoc("tpsLocSummary","tps-summary-panel");',
      'wireLoc("tpsLocDetail","tps-detail-panel");',
      'wireTables();}',
      'if(document.readyState==="loading"){',
      'document.addEventListener("DOMContentLoaded",wireAll);',
      '}else{wireAll();}',
      'setTimeout(wireAll,500);',
      '})();</script>'
  )

#------------------------------------------------------------------------------#
# Assembling the dedicated main dropdown --------------------------------------#
#------------------------------------------------------------------------------#

  body <- htmltools::HTML(paste0(
    '<p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1rem 0;">',
    'This section evaluates how often forecasts <strong>captured the observed ',
    'week-to-week trend</strong> &#8212; the five-level call of Large Decrease, ',
    'Decrease, Stable, Increase, or Large Increase &#8212; within each observed ',
    'epidemic phase (Ascension, Peak, and Decline) and at each forecast ',
    'horizon.</p>',

    # Figure title, styled like the other main dropdowns, then the
    # "To Navigate" call out describing only the main figure
    '<div style="margin-top: 0.75em;"></div>',
    '<h3><strong>Trend Capture by Epidemic Phase and Forecast Horizon</strong></h3>',
    '<div style="margin-top: 0.5em;"></div>',

    '<div class="section-intro" style="margin: 0 auto;">',
    '<div style="background: #f7f4fc; border-left: 4px solid #522D80; border-radius: 4px;',
    ' padding: 10px 15px; margin-bottom: 1.5rem;">',
    '<span style="font-size: 13px; font-weight: 700; color: #522D80; display: block;',
    ' margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.5px;">',
    'To Navigate</span>',
    '<p style="font-size: 15px; color: #555; line-height: 1.6; margin: 0;">',
    'More green and a higher bold percent are better. Hover a bar for ',
    'exact counts, or a legend entry to highlight that outcome across ',
    'all panels.</p>',
    '</div></div>',

    loc_filter("tpsLocSummary"),
    '<div id="tpsSummaryPanels">', summary_panels, '</div>',

    '<details class="accordion" style="margin:0 0 0.75rem;">',
    '<summary><strong>Week-by-Week Detail</strong></summary>',
    '<div class="accordion-body">',
    '<p style="font-size:14px;color:#555;line-height:1.6;margin:0 0 0.75rem;">',
    'Each marker is one forecast, placed at its forecasted value and judged ',
    'on its <strong>week-over-week trend call</strong> rather than its value. ',
    '<span style="color:#0f6e56;font-weight:700;">Green circles</span> mark ',
    'weeks where the forecast called the same trend as observed; ',
    '<span style="color:#e34948;font-weight:700;">red triangles (&#9650;)</span> ',
    'mark weeks where it overshot the trend (called it more upward than it ',
    'was) and <span style="color:#2a78d6;font-weight:700;">blue triangles ',
    '(&#9660;)</span> weeks where it undershot. Hover any marker to compare ',
    'the observed and forecasted trends directly.</p>',
    '<div style="margin-top:1.15rem;"></div>',
    loc_filter("tpsLocDetail"),
    '<div id="tpsDetailPanels">', detail_panels, '</div>',
    '</div></details>',

    '<details class="accordion" style="margin-top:1rem;">',
    '<summary><strong>Forecasted vs. Observed Change</strong></summary>',
    '<div class="accordion-body">',
    '<p style="font-size:14px;color:#555;line-height:1.6;margin:0 0 1.25rem;">',
    'How far off were the forecasted changes? Each forecast&#39;s asserted ',
    'week-over-week percent change is compared with the observed percent ',
    'change for the same target week. The difference &#8212; forecasted ',
    'minus observed &#8212; is how far the forecast was ',
    '<strong>% off</strong>: positive values overshot the change and ',
    'negative values undershot it. Cells show the <strong>median % off ',
    '(range)</strong>; <em>n</em> is the number of eligible forecast-target ',
    'pairs.</p>',
    mm_filter_row,
    '<div id="tpsTablePanels">', table_panels, '</div>',
    '<p style="font-size:12px;color:#777;line-height:1.5;margin:0.6rem 0 0;">',
    '&mdash; indicates no eligible forecast-target pairs for that ',
    'combination of observed trend and phase.</p>',
    '</div></details>',

    methods_accordion,
    switch_script
  ))

  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong(title)),
    htmltools::div(class = "accordion-body", body)
  )
}
