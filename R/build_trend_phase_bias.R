#' Build the trend- and phase-specific Forecast Bias table
#'
#' @keywords internal
#' @noRd
build_trend_phase_bias <- function(performance_summary,
                                   location_codes,
                                   location_labels,
                                   phase_data = NULL,
                                   status_message = NULL){

#------------------------------------------------------------------------------#
# Handling unavailable results ------------------------------------------------
#------------------------------------------------------------------------------#

  if(is.null(performance_summary) || !is.data.frame(performance_summary) ||
     nrow(performance_summary) == 0L){
    if(is.null(status_message) || !nzchar(status_message)){
      status_message <- paste(
        "No trend/phase bias table could be calculated. At least two",
        "consecutive observed target periods with eligible forecast-bias",
        "values are required."
      )
    }

    return(htmltools::tags$details(
      class = "accordion",
      htmltools::tags$summary(
        htmltools::tags$strong("Trend/Phase-Specific Performance")
      ),
      htmltools::div(
        class = "accordion-body",
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
# Preparing the controls ------------------------------------------------------
#------------------------------------------------------------------------------#

  horizon_values <- unique(as.character(performance_summary$horizon))
  horizon_values <- horizon_values[horizon_values != "Overall"]
  numeric_horizons <- suppressWarnings(as.numeric(horizon_values))
  if(all(!is.na(numeric_horizons))){
    horizon_values <- horizon_values[order(numeric_horizons)]
  }else{
    horizon_values <- sort(horizon_values)
  }
  horizon_values <- c("Overall", horizon_values)

  location_options <- paste0(vapply(seq_along(location_codes), function(i){
    paste0(
      '<option value="', htmltools::htmlEscape(location_codes[i]), '">',
      htmltools::htmlEscape(location_labels[i]), '</option>'
    )
  }, character(1)), collapse = "")

  # Location renders as one group inside the shared filter row below, sitting
  # inline with the Forecast horizon and Bias measure dropdowns
  location_group <- if(length(location_codes) > 1L){
    paste0('
      <div style="display:flex;align-items:center;gap:16px;">
        <label for="trendPhaseBiasLocation" style="font-size:13px;font-weight:700;color:#555;">Location</label>
        <select id="trendPhaseBiasLocation" class="geo-select" style="font-size:15px;padding:6px 12px;">', location_options, '</select>
      </div>')
  }else{""}

  horizon_options <- paste0(vapply(horizon_values, function(horizon){
    label <- if(horizon == "Overall") "Overall" else paste("Horizon", horizon)
    paste0('<option value="', htmltools::htmlEscape(horizon), '">',
           htmltools::htmlEscape(label), '</option>')
  }, character(1)), collapse = "")

  controls <- paste0('
    <div style="display:flex;justify-content:center;align-items:center;gap:18px;flex-wrap:wrap;margin:0 0 1.25rem;">',
      location_group, '
      <div style="display:flex;align-items:center;gap:16px;">
        <label for="trendPhaseBiasHorizon" style="font-size:13px;font-weight:700;color:#555;">Forecast horizon</label>
        <select id="trendPhaseBiasHorizon" class="geo-select" style="font-size:15px;padding:6px 12px;">', horizon_options, '</select>
      </div>
      <div style="display:flex;align-items:center;gap:16px;">
        <label for="trendPhaseBiasMetric" style="font-size:13px;font-weight:700;color:#555;">Bias measure</label>
        <select id="trendPhaseBiasMetric" class="geo-select" style="font-size:15px;padding:6px 12px;">
          <option value="pct">Bias (%)</option>
          <option value="raw">Raw Counts</option>
        </select>
      </div>
    </div>')

#------------------------------------------------------------------------------#
# Building the tables ---------------------------------------------------------
#------------------------------------------------------------------------------#

  trend_values <- c("large increase", "increase", "stable",
                    "decrease", "large decrease")
  trend_labels <- c("Large Increase", "Increase", "Stable",
                    "Decrease", "Large Decrease")
  phase_values <- c("Ascension", "Peak", "Decline")

  phase_header <- function(location, phase, subtitle){
    date_range <- format_trend_phase_date_ranges(phase_data, location, phase)
    date_html <- if(nzchar(date_range)) paste0(
      '<div style="font-size:11px;font-weight:600;color:#666;margin-top:3px;white-space:normal;">',
      date_range, '</div>'
    ) else ""

    paste0(
      phase, date_html,
      '<div class="sum-th-sub trend-phase-bias-unit" style="color:#C9B8E8;">',
      subtitle, '</div>'
    )
  }

  bias_color <- function(value){
    if(is.na(value)) return("#522D80")
    if(value > 0) return("#B85C30")
    if(value < 0) return("#2D6A9F")
    "#522D80"
  }

  cell_html <- function(location, horizon, trend, phase){
    hit <- performance_summary[
      performance_summary$location == location &
        as.character(performance_summary$horizon) == horizon &
        performance_summary$observed_trend == trend &
        performance_summary$phase == phase, , drop = FALSE
    ]

    if(nrow(hit) == 0L){
      hit <- data.frame(
        pct_median = NA_real_, pct_minimum = NA_real_, pct_maximum = NA_real_,
        n_pct = 0L, raw_median = NA_real_, raw_minimum = NA_real_,
        raw_maximum = NA_real_, n_raw = 0L
      )
    }

    pct <- round(as.numeric(hit$pct_median[1]), 1)
    pct_lo <- round(as.numeric(hit$pct_minimum[1]), 1)
    pct_hi <- round(as.numeric(hit$pct_maximum[1]), 1)
    raw <- round(as.numeric(hit$raw_median[1]), 1)
    raw_lo <- round(as.numeric(hit$raw_minimum[1]), 1)
    raw_hi <- round(as.numeric(hit$raw_maximum[1]), 1)

    signed_pct <- function(value){
      if(is.na(value)) return("&mdash;")
      paste0(if(value > 0) "+" else "", value, "%")
    }
    signed_raw <- function(value){
      if(is.na(value)) return("&mdash;")
      paste0(if(value > 0) "+" else "", value)
    }

    pct_range <- if(is.na(pct)) "" else
      paste0("(", signed_pct(pct_lo), " &ndash; ", signed_pct(pct_hi), ")")
    raw_range <- if(is.na(raw)) "" else
      paste0("(", signed_raw(raw_lo), " &ndash; ", signed_raw(raw_hi), ")")

    paste0('
      <td class="trend-phase-bias-cell" style="padding:13px;text-align:center;vertical-align:middle;"
          data-pct-med="', pct, '" data-pct-lo="', pct_lo, '" data-pct-hi="', pct_hi, '" data-pct-n="', hit$n_pct[1], '"
          data-raw-med="', raw, '" data-raw-lo="', raw_lo, '" data-raw-hi="', raw_hi, '" data-raw-n="', hit$n_raw[1], '">
        <div class="trend-phase-bias-main" style="font-size:14px;font-weight:700;color:', bias_color(pct), ';white-space:nowrap;">', signed_pct(pct), '</div>
        <div class="trend-phase-bias-range" style="font-size:12px;color:#666;white-space:nowrap;">', pct_range, '</div>
        <div class="trend-phase-bias-n" style="font-size:11px;color:#888;margin-top:2px;">',
          if(hit$n_pct[1] > 0) paste0("n = ", hit$n_pct[1]) else "", '</div>
      </td>')
  }

  panels <- character()
  panel_index <- 0L
  for(i in seq_along(location_codes)){
    for(horizon in horizon_values){
      panel_index <- panel_index + 1L
      rows <- paste0(vapply(seq_along(trend_values), function(j){
        cells <- paste0(vapply(phase_values, function(phase){
          cell_html(location_codes[i], horizon, trend_values[j], phase)
        }, character(1)), collapse = "")
        paste0(
          '<tr', if(j < length(trend_values))
            ' style="border-bottom:1px solid #e6e6e6;"' else '', '>',
          '<td style="padding:13px 16px;font-size:15px;font-weight:700;color:#555;border-right:1px solid #e0e0e0;white-space:nowrap;text-align:center;vertical-align:middle;">',
          trend_labels[j], '</td>', cells, '</tr>'
        )
      }, character(1)), collapse = "")

      panels <- c(panels, paste0('
        <div class="trend-phase-bias-panel" data-location="',
          htmltools::htmlEscape(location_codes[i]), '" data-horizon="',
          htmltools::htmlEscape(horizon), '" style="',
          if(panel_index == 1L) '' else 'display:none;', '">
          <div style="overflow-x:auto;">
            <table style="width:100%;border-collapse:collapse;border-top:1px solid #333;border-bottom:1px solid #333;">
              <thead><tr style="border-bottom:1px solid #333;">
                <th class="sum-th" style="width:170px;border-right:1px solid #e0e0e0;text-align:center;">Observed Trend</th>
                <th class="sum-th">', phase_header(location_codes[i], "Ascension", "Bias (%) &middot; Median (Range)"), '</th>
                <th class="sum-th" style="background:#f7f4fc;color:#522D80;">', phase_header(location_codes[i], "Peak", "Bias (%) &middot; Median (Range)"), '</th>
                <th class="sum-th">', phase_header(location_codes[i], "Decline", "Bias (%) &middot; Median (Range)"), '</th>
              </tr></thead>
              <tbody>', rows, '</tbody>
            </table>
          </div>
        </div>'))
    }
  }

#------------------------------------------------------------------------------#
# Assembling the interactive subdropdown --------------------------------------
#------------------------------------------------------------------------------#

  first_location <- htmltools::htmlEscape(location_codes[1])
  body <- htmltools::HTML(paste0('
    <p style="font-size:14px;line-height:1.65;color:#444;margin:0 0 1rem;">
      Explore forecast bias within each observed trend and epidemic phase.
      Positive values indicate overestimation and negative values indicate
      underestimation. Values are shown as <strong>median (range)</strong>.
    </p>', controls,
    '<div id="trendPhaseBiasPanels">', paste0(panels, collapse = ""), '</div>
    <p style="font-size:12px;line-height:1.55;color:#666;margin:0.9rem 0 0;">
      Percentage bias uses only observations at or above the stability threshold.
      Select <strong>Raw Counts</strong> to include all eligible transmission-season
      observations. A dash means no eligible values were available.
    </p>
    <script>
      (function() {
        function signed(value, suffix) {
          return isNaN(value) ? "&mdash;" : (value > 0 ? "+" : "") + value.toFixed(1) + suffix;
        }
        function color(value) {
          if (isNaN(value) || value === 0) return "#522D80";
          return value > 0 ? "#B85C30" : "#2D6A9F";
        }
        window.setTrendPhaseBiasMode = function(mode) {
          var raw = mode === "raw";
          document.querySelectorAll("#trendPhaseBiasPanels .trend-phase-bias-cell").forEach(function(cell) {
            var prefix = raw ? "data-raw-" : "data-pct-";
            var med = parseFloat(cell.getAttribute(prefix + "med"));
            var lo = parseFloat(cell.getAttribute(prefix + "lo"));
            var hi = parseFloat(cell.getAttribute(prefix + "hi"));
            var n = parseInt(cell.getAttribute(prefix + "n"), 10);
            var suffix = raw ? "" : "%";
            cell.querySelector(".trend-phase-bias-main").innerHTML = signed(med, suffix);
            cell.querySelector(".trend-phase-bias-main").style.color = color(med);
            cell.querySelector(".trend-phase-bias-range").innerHTML = isNaN(med) ? "" : "(" + signed(lo, suffix) + " &ndash; " + signed(hi, suffix) + ")";
            cell.querySelector(".trend-phase-bias-n").innerHTML = n > 0 ? "n = " + n : "";
          });
          document.querySelectorAll("#trendPhaseBiasPanels .trend-phase-bias-unit").forEach(function(unit) {
            unit.innerHTML = (raw ? "Raw Counts" : "Bias (%)") + " &middot; Median (Range)";
          });
          var metricSelect = document.getElementById("trendPhaseBiasMetric");
          if (metricSelect) metricSelect.value = mode;
        };
        function updatePanel() {
          var locationSelect = document.getElementById("trendPhaseBiasLocation");
          var horizonSelect = document.getElementById("trendPhaseBiasHorizon");
          var locationValue = locationSelect ? locationSelect.value : "', first_location, '";
          var horizonValue = horizonSelect ? horizonSelect.value : "Overall";
          var activePanel = null;
          document.querySelectorAll("#trendPhaseBiasPanels .trend-phase-bias-panel").forEach(function(panel) {
            var show = panel.getAttribute("data-location") === locationValue &&
              panel.getAttribute("data-horizon") === horizonValue;
            panel.style.display = show ? "block" : "none";
            if (show) activePanel = panel;
          });

          // If the selected table has no stable percentage-bias values, expose
          // only Raw Counts, matching the parent Forecast Bias stability rule.
          var metricSelect = document.getElementById("trendPhaseBiasMetric");
          if (activePanel && metricSelect) {
            var cells = activePanel.querySelectorAll(".trend-phase-bias-cell");
            var hasPct = Array.from(cells).some(function(cell) {
              return !isNaN(parseFloat(cell.getAttribute("data-pct-med")));
            });
            var pctOption = metricSelect.querySelector("option[value=pct]");
            if (pctOption) pctOption.disabled = !hasPct;
            if (!hasPct) {
              metricSelect.value = "raw";
              if (typeof window.setBiasTableMode === "function") window.setBiasTableMode("raw");
              else window.setTrendPhaseBiasMode("raw");
            }
          }
        }
        var locationSelect = document.getElementById("trendPhaseBiasLocation");
        var horizonSelect = document.getElementById("trendPhaseBiasHorizon");
        var metricSelect = document.getElementById("trendPhaseBiasMetric");
        if (locationSelect) locationSelect.addEventListener("change", updatePanel);
        if (horizonSelect) horizonSelect.addEventListener("change", updatePanel);
        if (metricSelect) metricSelect.addEventListener("change", function() {
          if (typeof window.setBiasTableMode === "function") window.setBiasTableMode(this.value);
          else window.setTrendPhaseBiasMode(this.value);
        });
        window.setTrendPhaseBiasMode("pct");
        updatePanel();
      })();
    </script>'))

  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(
      htmltools::tags$strong("Trend/Phase-Specific Performance")
    ),
    htmltools::div(class = "accordion-body", body)
  )
}
