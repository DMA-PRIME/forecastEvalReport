#' Build the nested traditional-metric trend/phase table
#'
#' @keywords internal
#' @noRd
build_trend_phase_traditional_metrics <- function(performance_summary,
                                                  location_codes,
                                                  location_labels,
                                                  phase_data = NULL,
                                                  status_message = NULL,
                                                  available_metrics = NULL){

#------------------------------------------------------------------------------#
# Handling unavailable results ------------------------------------------------#
#------------------------------------------------------------------------------#

  if(is.null(performance_summary) || !is.data.frame(performance_summary) ||
     nrow(performance_summary) == 0L){
    if(is.null(status_message) || !nzchar(status_message)){
      status_message <- paste(
        "No traditional-metric trend/phase table could be calculated.",
        "Eligible median forecasts and observed target periods are required."
      )
    }
    return(htmltools::tags$details(
      class = "accordion",
      htmltools::tags$summary(
        htmltools::tags$strong("Trend/Phase-Specific Performance")
      ),
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
# Preparing selectors ----------------------------------------------------------#
#------------------------------------------------------------------------------#

  horizons <- unique(as.character(performance_summary$horizon))
  horizons <- horizons[horizons != "Overall"]
  numeric_horizons <- suppressWarnings(as.numeric(horizons))
  horizons <- if(all(!is.na(numeric_horizons))){
    horizons[order(numeric_horizons)]
  }else{sort(horizons)}
  horizons <- c("Overall", horizons)

#------------------------------------------------------------------------------#
# Metric availability ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: The metric selector only offers metrics that are available for these  #
# forecasts, as determined by the main traditional-metrics table (passed in    #
# via available_metrics). The default metric, header sub-labels, and initial   #
# cell values all follow the first available metric.                           #
#------------------------------------------------------------------------------#

  metric_option_labels <- c(wis   = "Average WIS",
                            mae   = "Average MAE",
                            under = "Underprediction",
                            over  = "Overprediction",
                            cov50 = "50% PI Coverage",
                            cov80 = "80% PI Coverage",
                            cov95 = "95% PI Coverage")
  metric_notes <- c(wis   = "Lower Is Better",
                    mae   = "Lower Is Better",
                    under = "Lower Is Better",
                    over  = "Lower Is Better",
                    cov50 = "Target 50%",
                    cov80 = "Target 80%",
                    cov95 = "Target 95%")

  all_metric_values <- names(metric_option_labels)
  metric_values <- if(!is.null(available_metrics)){
    intersect(all_metric_values, available_metrics)
  }else{
    all_metric_values
  }

  # Defensive fallback: never render an empty selector
  if(length(metric_values) == 0L) metric_values <- all_metric_values

  default_metric   <- metric_values[1]
  metrics_filtered <- length(metric_values) < length(all_metric_values)

  metric_options <- paste0('<option value="', metric_values, '">',
                           metric_option_labels[metric_values], '</option>',
                           collapse = "")

  location_options <- paste0(vapply(seq_along(location_codes), function(i){
    paste0('<option value="', htmltools::htmlEscape(location_codes[i]), '">',
           htmltools::htmlEscape(location_labels[i]), '</option>')
  }, character(1)), collapse = "")

  # Location renders as one group inside the shared filter row below, sitting
  # inline with the Forecast horizon and Metric dropdowns
  location_group <- if(length(location_codes) > 1L){
    paste0('
      <div style="display:flex;align-items:center;gap:16px;">
        <label for="tradPhaseLocation" style="font-size:13px;font-weight:700;color:#555;">Location</label>
        <select id="tradPhaseLocation" class="geo-select" style="font-size:15px;padding:6px 12px;">',
        location_options, '</select>
      </div>')
  }else{""}

  horizon_options <- paste0(vapply(horizons, function(horizon){
    label <- if(horizon == "Overall") "Overall" else paste("Horizon", horizon)
    paste0('<option value="', htmltools::htmlEscape(horizon), '">',
           htmltools::htmlEscape(label), '</option>')
  }, character(1)), collapse = "")

  controls <- paste0('
    <div style="display:flex;justify-content:center;align-items:center;gap:18px;flex-wrap:wrap;margin:0 0 1.25rem;">',
      location_group, '
      <div style="display:flex;align-items:center;gap:16px;">
        <label for="tradPhaseHorizon" style="font-size:13px;font-weight:700;color:#555;">Forecast horizon</label>
        <select id="tradPhaseHorizon" class="geo-select" style="font-size:15px;padding:6px 12px;">', horizon_options, '</select>
      </div>
      <div style="display:flex;align-items:center;gap:16px;">
        <label for="tradPhaseMetric" style="font-size:13px;font-weight:700;color:#555;">Metric</label>
        <select id="tradPhaseMetric" class="geo-select" style="font-size:15px;padding:6px 12px;">', metric_options, '</select>
      </div>
    </div>')

#------------------------------------------------------------------------------#
# Building the observed-trend by phase panels ---------------------------------#
#------------------------------------------------------------------------------#

  trend_values <- c("large increase", "increase", "stable",
                    "decrease", "large decrease")
  trend_labels <- c("Large Increase", "Increase", "Stable",
                    "Decrease", "Large Decrease")
  phase_values <- c("Ascension", "Peak", "Decline")

  phase_header <- function(location, phase){
    date_range <- format_trend_phase_date_ranges(phase_data, location, phase)
    paste0(
      phase,
      if(nzchar(date_range)) paste0(
        '<div style="font-size:11px;font-weight:600;color:#666;margin-top:3px;white-space:normal;">',
        date_range, '</div>'
      ) else "",
      '<div class="sum-th-sub trad-phase-unit" style="color:#C9B8E8;">',
      metric_option_labels[[default_metric]], ' &middot; ',
      metric_notes[[default_metric]], '</div>'
    )
  }

  attr_number <- function(value){
    if(length(value) == 0L || is.na(value) || !is.finite(value)) "" else
      formatC(value, format = "f", digits = 8)
  }

  cell_html <- function(location, horizon, trend, phase){
    hit <- performance_summary[
      performance_summary$location == location &
        as.character(performance_summary$horizon) == horizon &
        performance_summary$observed_trend == trend &
        performance_summary$phase == phase, , drop = FALSE
    ]
    attributes <- paste0(vapply(metric_values, function(metric){
      row <- hit[hit$metric == metric, , drop = FALSE]
      value <- if(nrow(row)) row$mean[1] else NA_real_
      n <- if(nrow(row)) row$n[1] else 0L
      paste0(' data-', metric, '-value="', attr_number(value),
             '" data-', metric, '-n="', as.integer(n), '"')
    }, character(1)), collapse = "")

    first <- hit[hit$metric == default_metric, , drop = FALSE]
    value <- if(nrow(first)) first$mean[1] else NA_real_
    n <- if(nrow(first)) first$n[1] else 0L
    display <- if(is.na(value) || !is.finite(value)){
      "&mdash;"
    }else if(startsWith(default_metric, "cov")){
      paste0(formatC(value * 100, format = "f", digits = 1), "%")
    }else{
      formatC(value, format = "f", digits = 2)
    }

    paste0(
      '<td class="trad-phase-cell" style="padding:13px;text-align:center;vertical-align:middle;"',
      attributes, '><div class="trad-phase-main" style="font-size:14px;font-weight:700;color:#522D80;white-space:nowrap;">',
      display, '</div><div class="trad-phase-n" style="font-size:11px;color:#888;margin-top:2px;">',
      if(n > 0L) paste0("n = ", n) else "", '</div></td>'
    )
  }

  panels <- character()
  panel_index <- 0L
  for(i in seq_along(location_codes)){
    for(horizon in horizons){
      panel_index <- panel_index + 1L
      rows <- paste0(vapply(seq_along(trend_values), function(j){
        cells <- paste0(vapply(phase_values, function(phase){
          cell_html(location_codes[i], horizon, trend_values[j], phase)
        }, character(1)), collapse = "")
        paste0(
          '<tr style="border-bottom:1px solid #e6e6e6;">',
          '<td style="padding:13px 16px;font-size:15px;font-weight:700;color:#555;border-right:1px solid #e0e0e0;white-space:nowrap;text-align:center;vertical-align:middle;">',
          trend_labels[j], '</td>', cells, '</tr>'
        )
      }, character(1)), collapse = "")

      panels <- c(panels, paste0(
        '<div class="trad-phase-panel" data-location="',
        htmltools::htmlEscape(location_codes[i]), '" data-horizon="',
        htmltools::htmlEscape(horizon), '" style="',
        if(panel_index == 1L) '' else 'display:none;', '">',
        '<div style="overflow-x:auto;"><table style="width:100%;border-collapse:collapse;border-top:1px solid #333;border-bottom:1px solid #333;">',
        '<thead><tr style="border-bottom:1px solid #333;">',
        '<th class="sum-th" style="width:170px;text-align:center;border-right:1px solid #e0e0e0;">Observed Trend</th>',
        '<th class="sum-th">', phase_header(location_codes[i], "Ascension"), '</th>',
        '<th class="sum-th" style="background:#f7f4fc;color:#522D80;">', phase_header(location_codes[i], "Peak"), '</th>',
        '<th class="sum-th">', phase_header(location_codes[i], "Decline"), '</th>',
        '</tr></thead><tbody>', rows, '</tbody></table></div></div>'
      ))
    }
  }

#------------------------------------------------------------------------------#
# Assembling the nested dropdown ----------------------------------------------#
#------------------------------------------------------------------------------#

  first_location <- htmltools::htmlEscape(location_codes[1])
  body <- htmltools::HTML(paste0(
    '<p style="font-size:14px;line-height:1.65;color:#444;margin:0 0 1rem;">',
    'Explore each traditional metric within combinations of the observed ',
    'five-level trend label and epidemic phase. Values are averages; ',
    '<em>n</em> is the number of eligible forecast-target pairs.',
    if(metrics_filtered) paste0(' Metrics that could not be computed for ',
                                'these forecasts are omitted from the metric ',
                                'selector.') else '',
    '</p>',
    controls, '<div id="tradPhasePanels">', paste0(panels, collapse = ""), '</div>',
    '<p style="font-size:12px;line-height:1.55;color:#666;margin:0.9rem 0 0;">A dash (&mdash;) means the required forecast quantiles or eligible scores were unavailable.</p>',
    '<script>(function(){',
    'var meta={wis:{label:"Average WIS",suffix:"",digits:2,note:"Lower Is Better"},mae:{label:"Average MAE",suffix:"",digits:2,note:"Lower Is Better"},under:{label:"Underprediction",suffix:"",digits:2,note:"Lower Is Better"},over:{label:"Overprediction",suffix:"",digits:2,note:"Lower Is Better"},cov50:{label:"50% PI Coverage",suffix:"%",digits:1,note:"Target 50%"},cov80:{label:"80% PI Coverage",suffix:"%",digits:1,note:"Target 80%"},cov95:{label:"95% PI Coverage",suffix:"%",digits:1,note:"Target 95%"}};',
    'function metric(){var s=document.getElementById("tradPhaseMetric");var m=s?s.value:"', default_metric, '";var x=meta[m];document.querySelectorAll("#tradPhasePanels .trad-phase-cell").forEach(function(c){var v=parseFloat(c.getAttribute("data-"+m+"-value"));var n=parseInt(c.getAttribute("data-"+m+"-n"),10);var d=c.querySelector(".trad-phase-main");var ne=c.querySelector(".trad-phase-n");if(d)d.innerHTML=isNaN(v)?"&mdash;":((m.indexOf("cov")===0?v*100:v).toFixed(x.digits)+x.suffix);if(ne)ne.textContent=n>0?"n = "+n:"";});document.querySelectorAll("#tradPhasePanels .trad-phase-unit").forEach(function(u){u.textContent=x.label+" · "+x.note;});}',
    'function panel(){var l=document.getElementById("tradPhaseLocation");var h=document.getElementById("tradPhaseHorizon");var lv=l?l.value:"', first_location, '";var hv=h?h.value:"Overall";document.querySelectorAll("#tradPhasePanels .trad-phase-panel").forEach(function(p){p.style.display=p.getAttribute("data-location")===lv&&p.getAttribute("data-horizon")===hv?"block":"none";});metric();}',
    'var l=document.getElementById("tradPhaseLocation"),h=document.getElementById("tradPhaseHorizon"),m=document.getElementById("tradPhaseMetric");if(l)l.addEventListener("change",panel);if(h)h.addEventListener("change",panel);if(m)m.addEventListener("change",metric);panel();})();</script>'
  ))

  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(
      htmltools::tags$strong("Trend/Phase-Specific Performance")
    ),
    htmltools::div(class = "accordion-body", body)
  )
}
