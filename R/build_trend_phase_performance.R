#' Build the trend- and phase-specific Percent Accuracy table
#'
#' @keywords internal
#' @noRd
build_trend_phase_performance <- function(performance_summary,
                                          location_codes,
                                          location_labels,
                                          phase_data = NULL,
                                          status_message = NULL){

#------------------------------------------------------------------------------#
# Confirming content is available ---------------------------------------------
#------------------------------------------------------------------------------#
# About: The subdropdown is omitted when no trend/phase rows can be scored.    #
#------------------------------------------------------------------------------#

  if(is.null(performance_summary) ||
     !is.data.frame(performance_summary) ||
     nrow(performance_summary) == 0L){
    if(is.null(status_message) || !nzchar(status_message)){
      status_message <- paste(
        "No trend/phase table could be calculated. At least two consecutive",
        "observed target periods with eligible Percent Accuracy values are",
        "required."
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
# Preparing selectors ---------------------------------------------------------
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
  # inline with the Forecast horizon dropdown
  location_group <- if(length(location_codes) > 1L){
    paste0('
      <div style="display:flex;align-items:center;gap:16px;">
        <label for="trendPhaseLocation" style="font-size:13px;font-weight:700;color:#555;">Location</label>
        <select id="trendPhaseLocation" class="geo-select" style="font-size:15px;padding:6px 12px;">', location_options, '</select>
      </div>')
  }else{""}

  horizon_options <- paste0(vapply(horizon_values, function(horizon){
    label <- if(horizon == "Overall") "Overall" else paste("Horizon", horizon)
    paste0(
      '<option value="', htmltools::htmlEscape(horizon), '">',
      htmltools::htmlEscape(label), '</option>'
    )
  }, character(1)), collapse = "")

  horizon_selector <- paste0('
    <div style="display:flex;justify-content:center;align-items:center;gap:18px;flex-wrap:wrap;margin:0 0 1.25rem;">',
      location_group, '
      <div style="display:flex;align-items:center;gap:16px;">
        <label for="trendPhaseHorizon" style="font-size:13px;font-weight:700;color:#555;">Forecast horizon</label>
        <select id="trendPhaseHorizon" class="geo-select" style="font-size:15px;padding:6px 12px;">', horizon_options, '</select>
      </div>
    </div>')

#------------------------------------------------------------------------------#
# Building table panels -------------------------------------------------------
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
      '<div class="sum-th-sub" style="color:#C9B8E8;">', subtitle, '</div>'
    )
  }

  cell_html <- function(location, horizon, trend, phase){
    hit <- performance_summary[
      performance_summary$location == location &
        as.character(performance_summary$horizon) == horizon &
        performance_summary$observed_trend == trend &
        performance_summary$phase == phase, , drop = FALSE
    ]

    if(nrow(hit) == 0L || is.na(hit$median[1])){
      return('<td style="padding:13px;text-align:center;color:#777;">&mdash;</td>')
    }

    med <- round(hit$median[1], 1)
    lo <- round(hit$minimum[1], 1)
    hi <- round(hit$maximum[1], 1)
    n <- as.integer(hit$n[1])

    paste0('
      <td style="padding:13px;text-align:center;vertical-align:middle;">
        <div style="font-size:14px;font-weight:700;color:#522D80;white-space:nowrap;">', med, '%</div>
        <div style="font-size:12px;color:#666;white-space:nowrap;">(', lo, '% &ndash; ', hi, '%)</div>
        <div style="font-size:11px;color:#888;margin-top:2px;">n = ', n, '</div>
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
          '<td style="padding:13px 16px;font-size:15px;font-weight:700;color:#555;',
          'border-right:1px solid #e0e0e0;white-space:nowrap;text-align:center;vertical-align:middle;">',
          trend_labels[j], '</td>', cells, '</tr>'
        )
      }, character(1)), collapse = "")

      panels <- c(panels, paste0('
        <div class="trend-phase-panel"
             data-location="', htmltools::htmlEscape(location_codes[i]), '"
             data-horizon="', htmltools::htmlEscape(horizon), '"
             style="', if(panel_index == 1L) '' else 'display:none;', '">
          <div style="overflow-x:auto;">
            <table style="width:100%;border-collapse:collapse;border-top:1px solid #333;border-bottom:1px solid #333;">
              <thead>
                <tr style="border-bottom:1px solid #333;">
                  <th class="sum-th" style="width:170px;border-right:1px solid #e0e0e0;text-align:center;">Observed Trend</th>
                  <th class="sum-th">', phase_header(location_codes[i], "Ascension", "Median (Range)"), '</th>
                  <th class="sum-th" style="background:#f7f4fc;color:#522D80;">', phase_header(location_codes[i], "Peak", "Median (Range)"), '</th>
                  <th class="sum-th">', phase_header(location_codes[i], "Decline", "Median (Range)"), '</th>
                </tr>
              </thead>
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
      Explore the existing Percent Accuracy measure within each observed trend
      and epidemic phase. Values are shown as <strong>median (range)</strong>;
      <em>n</em> is the number of eligible forecast-target pairs.
    </p>',
    horizon_selector,
    '<div id="trendPhasePanels">', paste0(panels, collapse = ""), '</div>
    <p style="font-size:12px;line-height:1.55;color:#666;margin:0.9rem 0 0;">
      A dash (&mdash;) means that the observed trend did not occur in that phase
      for the selected location and horizon, or no eligible Percent Accuracy
      values were available.
    </p>
    <script>
      (function() {
        function updateTrendPhaseTable() {
          var locationSelect = document.getElementById("trendPhaseLocation");
          var horizonSelect = document.getElementById("trendPhaseHorizon");
          var locationValue = locationSelect ? locationSelect.value : "', first_location, '";
          var horizonValue = horizonSelect ? horizonSelect.value : "Overall";
          var panels = document.querySelectorAll("#trendPhasePanels .trend-phase-panel");
          panels.forEach(function(panel) {
            var show = panel.getAttribute("data-location") === locationValue &&
                       panel.getAttribute("data-horizon") === horizonValue;
            panel.style.display = show ? "block" : "none";
          });
        }
        var locationSelect = document.getElementById("trendPhaseLocation");
        var horizonSelect = document.getElementById("trendPhaseHorizon");
        if (locationSelect) locationSelect.addEventListener("change", updateTrendPhaseTable);
        if (horizonSelect) horizonSelect.addEventListener("change", updateTrendPhaseTable);
        updateTrendPhaseTable();
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
