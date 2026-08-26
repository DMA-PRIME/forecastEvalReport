#' Render the Percent Agreement drop-down section
#'
#' Builds the full Percent Agreement accordion for the testing-period block:
#' an intro, the "Percent Agreement Over Time" figure (one panel per location,
#' switched by a geography dropdown), a synced Median (Range) summary table by
#' forecast horizon, and a Detailed Methods accordion. Renders nothing when no
#' testing data is present, matching every other testing-period section.
#'
#' Locations are keyed in the DOM by their display label (`location_display`)
#' so the dropdown, plot panels, and table stay in sync; the underlying data is
#' filtered by the raw `location` code, which is what `make_realtime_percent_agreement_plot()`
#' expects.
#'
#' @param percentAgreement.data Output of `percentAgreementCalculation()` — the
#'   evaluation frame with row-level `per_agreement`, `is_transmission`,
#'   `horizon`, `location`, and (when available) `location_display`.
#' @param impl_meta Metadata list from `extract_evaluation_data()`. Used for the
#'   testing-data presence gate and as a fallback location label source.
#' @param outcome Character label for the outcome (right axis / hover). When
#'   `NULL`, the clean outcome name is taken from `variables_crosswalk`, then
#'   `impl_meta$outcome`, then a generic fallback.
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`. Supplies the clean outcome
#'   label (`clean_name_full` of the `outcome` rows).
#' @param eval_config Evaluation config from `create_evaluation_config()`. Only
#'   `non_transmission_months` is used here (to phrase the methods text). When
#'   `NULL`, defaults are used.
#'
#' @return Rendered HTML via [htmltools::tagList()], or `invisible(NULL)` when
#'   no testing data is available.
#'
#' @keywords internal
#' @noRd
section_realtime_percent_agreement <- function(percentAgreement.data,
                                      impl_meta,
                                      outcome             = NULL,
                                      variables_crosswalk = NULL,
                                      eval_config         = NULL) {

#------------------------------------------------------------------------------#
# Guard: real-time percent-agreement data must be present ----------------------#
#------------------------------------------------------------------------------#
# About: The real-time block is driven entirely by the implementation model's  #
# forecast archive, not by an evaluation/testing model. There is no testing    #
# gate here -- the section renders whenever there is percent-agreement data to #
# show, and disappears otherwise.                                              #
#------------------------------------------------------------------------------#

  if(is.null(percentAgreement.data) ||
     !is.data.frame(percentAgreement.data) ||
     nrow(percentAgreement.data) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Resolving inputs -------------------------------------------------------------
#------------------------------------------------------------------------------#

  ###########################
  # Evaluation config       #
  ###########################
  if(is.null(eval_config)) eval_config <- create_evaluation_config()

  ##########################################################
  # Outcome label: prefer the clean name from the variables #
  # crosswalk (outcome rows), then impl_meta$outcome, then  #
  # a generic fallback. Mirrors section_overview_text.      #
  ##########################################################
  if(is.null(outcome)){

    outcome <- NA_character_

    # Clean outcome name from the crosswalk
    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk) &&
       all(c("variable_type", "clean_name_full") %in% names(variables_crosswalk))){

      outcome_rows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "outcome", ]

      if(nrow(outcome_rows) > 0){
        clean_names <- unique(outcome_rows$clean_name_full)
        clean_names <- clean_names[
          !is.na(clean_names) &
            nchar(trimws(clean_names)) > 0 &
            clean_names != "USER: provide a definition"]
        if(length(clean_names) > 0) outcome <- paste(clean_names, collapse = ", ")
      }
    }

    # Fallbacks
    if(is.na(outcome) || !nzchar(outcome)){
      outcome <- if(!is.null(impl_meta$outcome)) paste(impl_meta$outcome, collapse = ", ") else "Observed"
    }
  }

  ###############################################
  # Non-transmission month label for the prose  #
  ###############################################
  nt <- integer(0)  # real-time scores every realized week; no non-transmission exclusion
  nt_label <- if(length(nt) == 0){
    "none"
  }else if(identical(as.integer(nt), as.integer(min(nt):max(nt)))){
    paste0(month.name[min(nt)], " \u2013 ", month.name[max(nt)])
  }else{
    paste(month.name[nt], collapse = ", ")
  }

  ##############################################################
  # Only mention the No Evaluation period when (a) months are  #
  # actually excluded and (b) those months fall within the     #
  # testing data shown in the plot.                            #
  ##############################################################
  data_months  <- unique(as.integer(format(as.Date(percentAgreement.data$target_end_date), "%m")))
  show_no_eval <- length(nt) > 0 && any(nt %in% data_months)

  no_eval_sentence <- if(show_no_eval){
    paste0(' Periods marked <strong>No Evaluation</strong> (', nt_label,
           ') are excluded from all summaries.')
  }else{
    ''
  }

#------------------------------------------------------------------------------#
# Resolving location codes and display labels ----------------------------------
#------------------------------------------------------------------------------#
# About: Data is filtered by raw `location`; the DOM (dropdown, panels, table)  #
# is keyed by the display label so all three stay in sync. Labels come from     #
# location_display, then impl_meta$locations, then fall back to the raw code.   #
#------------------------------------------------------------------------------#

  loc_codes <- sort(unique(percentAgreement.data$location))

  resolve_label <- function(cd){
    lab <- NA_character_
    if("location_display" %in% names(percentAgreement.data)){
      cand <- unique(percentAgreement.data$location_display[percentAgreement.data$location == cd])
      cand <- cand[!is.na(cand) & nzchar(cand)]
      if(length(cand) >= 1) lab <- cand[1]
    }
    if(is.na(lab) && !is.null(impl_meta$locations) && cd %in% names(impl_meta$locations)){
      lab <- unname(impl_meta$locations[[cd]])
    }
    if(is.na(lab) || !nzchar(lab)) lab <- cd
    lab
  }

  loc_labels  <- vapply(loc_codes, resolve_label, character(1))
  n_loc       <- length(loc_codes)
  interactive <- n_loc > 1   # sort / search / sync only make sense for >1 location

#------------------------------------------------------------------------------#
# Intro paragraph --------------------------------------------------------------
#------------------------------------------------------------------------------#

  intro_html <- htmltools::HTML('
  <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1rem 0;">
    Percent agreement measures how closely forecasted values align with observed
    counts, ranging from 0% to 100% where <strong>higher</strong> values indicate
    <strong>stronger</strong> agreement. The figure and table below summarize agreement
    over time across the overall median and for all forecast horizons.
  </p>
  ')

#------------------------------------------------------------------------------#
# "To Navigate" box (config-aware non-transmission label) ----------------------
#------------------------------------------------------------------------------#

  navigate_html <- htmltools::HTML(paste0('
  <!-- Reader orientation for the figure and table -->
  <div class="section-intro" style="margin: 0 auto;">
    <div style="background: #f7f4fc; border-left: 4px solid #522D80; border-radius: 4px;
                padding: 10px 15px; margin-bottom: 1.5rem;">
      <span style="font-size: 13px; font-weight: 700; color: #522D80; display: block;
                   margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.5px;">
        To Navigate
      </span>
      <p style="font-size: 15px; color: #555; line-height: 1.6; margin: 0;">
        The solid black line shows the <strong>overall median percent agreement</strong>
        over time; click a horizon in the legend to overlay individual horizon lines.', no_eval_sentence, '
        In the table, columns show median agreement and range by horizon, with an
        overall summary in purple.
      </p>
    </div>
  </div>
  '))

#------------------------------------------------------------------------------#
# Section header ---------------------------------------------------------------
#------------------------------------------------------------------------------#

  header_html <- htmltools::tagList(
    htmltools::div(style = "margin-top: 1.5em;"),
    htmltools::tags$h3(htmltools::tags$strong("Percent Agreement Over Time by Forecast Horizon")),
    htmltools::div(style = "margin-top: 2em;")
  )

#------------------------------------------------------------------------------#
# Geography dropdown (multi-location only) -------------------------------------
#------------------------------------------------------------------------------#

  dropdown_html <- if(n_loc > 1){
    htmltools::tagList(
      build_geo_dropdown(loc_labels, select_id = "geoSelect_RTPA"),
      htmltools::div(style = "margin-top: 2em;")
    )
  }else{
    htmltools::div(style = "margin-top: 1.5em;")
  }

#------------------------------------------------------------------------------#
# Building the per-location plots ----------------------------------------------
#------------------------------------------------------------------------------#

  wrap_id <- "wrap-rtPctAgreePlot"

  plotlyList <- setNames(
    lapply(loc_codes, function(loc){
      make_realtime_percent_agreement_plot(
        data    = percentAgreement.data,
        loc     = loc,
        outcome = outcome
      )
    }),
    loc_labels
  )

#------------------------------------------------------------------------------#
# Rendering the figure (single vs multiple locations) --------------------------
#------------------------------------------------------------------------------#

  if(n_loc == 1){

    #############################
    # Single location: render   #
    #############################
    plot_block <- htmltools::div(
      style = "display: flex; justify-content: center; width: 100%;",
      htmltools::div(
        class = "plot-panel",
        style = "display: flex; justify-content: center;",
        plotlyList[[1]]
      )
    )

  }else{

    #################################################
    # Multiple locations: panels + dropdown-driven  #
    # show/hide JS, keyed by display label          #
    #################################################
    plot_block <- htmltools::tagList(

      ##############################
      # One panel per location     #
      ##############################
      htmltools::div(
        id    = wrap_id,
        style = "width: 100%; display: flex; justify-content: center;",
        lapply(seq_len(n_loc), function(i){
          htmltools::div(
            class                = "plot-panel",
            style                = if(i == 1) "display: flex; justify-content: center;" else "display: none; justify-content: center;",
            `data-panel-index`   = i - 1,
            `data-location-name` = loc_labels[i],
            plotlyList[[i]]
          )
        })
      ),

      ##############################
      # Panel-switch JS            #
      ##############################
      htmltools::tags$script(htmltools::HTML(sprintf('
        (function() {

          function applyRtPctAgreePlotLocation(locName) {
            var wrap = document.getElementById("%s");
            if (!wrap) return;
            var panels = wrap.querySelectorAll("[data-panel-index]");
            panels.forEach(function(p) {
              var isActive = p.getAttribute("data-location-name") === locName;
              p.style.display        = isActive ? "flex" : "none";
              p.style.justifyContent = "center";
              if (isActive && window.Plotly) {
                var gd = p.querySelector(".plotly");
                if (gd) requestAnimationFrame(function() { Plotly.Plots.resize(gd); });
              }
            });
            if (typeof syncRtPctTableToPlot === "function") syncRtPctTableToPlot(locName);
          }

          var sel = document.getElementById("geoSelect_RTPA");
          if (sel) {
            sel.addEventListener("change", function() {
              var opt = this.options[this.selectedIndex];
              applyRtPctAgreePlotLocation(opt.getAttribute("data-location-name"));
            });
          }

          window.addEventListener("load", function() {
            var sel = document.getElementById("geoSelect_RTPA");
            if (sel && sel.options[0]) {
              applyRtPctAgreePlotLocation(sel.options[0].getAttribute("data-location-name"));
            }
          });

        })();
      ', wrap_id)))
    )
  }

#------------------------------------------------------------------------------#
# Building the location x horizon table (Median + Range) -----------------------
#------------------------------------------------------------------------------#
# About: Median and range (min/max) of row-level percent agreement, computed    #
# on transmission-season rows only. Empty (location x horizon) groups render a  #
# dash instead of crashing quantile()/min()/max() on an empty vector.           #
#------------------------------------------------------------------------------#

  horizons <- sort(unique(percentAgreement.data$horizon))

  ##########################################
  # Safe summary on a possibly-empty vector#
  ##########################################
  safe_round <- function(x, fn){
    x <- x[!is.na(x)]
    if(length(x) == 0) return(NA_real_)
    round(fn(x), 1)
  }

  ##########################################
  # Cell builder: median + (min - max)     #
  ##########################################
  metric_cell <- function(vals, is_overall = FALSE){
    med <- safe_round(vals, stats::median)
    lo  <- safe_round(vals, min)
    hi  <- safe_round(vals, max)

    bg        <- if(is_overall) "background: #f7f4fc;" else "border-right: 1px solid #e0e0e0;"
    med_color <- if(is_overall) "#522D80" else "#222"
    rng_color <- if(is_overall) "#9B85C8" else "#555"

    med_txt <- if(is.na(med)) "\u2014" else paste0(med, "%")
    rng_txt <- if(is.na(med)) "" else paste0("(", lo, "% \u2013 ", hi, "%)")
    dval    <- if(is.na(med)) "" else med

    paste0('
      <td style="padding: 14px 16px; text-align: center; vertical-align: middle; ', bg, '"
           data-value="', dval, '">
        <div style="font-size: 14px; font-weight: 600; color: ', med_color, '; white-space: nowrap;">
          ', med_txt, '
        </div>
        <div style="font-size: 13px; font-weight: 400; color: ', rng_color, '; white-space: nowrap;">
          ', rng_txt, '
        </div>
      </td>')
  }

  ##########################################
  # One row per location                   #
  ##########################################
  all_rows <- paste0(
    sapply(seq_len(n_loc), function(i){
      code   <- loc_codes[i]
      label  <- loc_labels[i]
      border <- if(i < n_loc) "border-bottom: 1px solid #e0e0e0;" else ""

      loc_data <- percentAgreement.data %>%
        dplyr::filter(location == code, is_transmission == TRUE)

      ################
      # Horizon cells#
      ################
      horizon_cells <- paste0(
        sapply(horizons, function(h){
          metric_cell(loc_data$per_agreement[loc_data$horizon == h])
        }),
        collapse = ""
      )

      ################
      # Overall cell #
      ################
      overall_cell <- metric_cell(loc_data$per_agreement, is_overall = TRUE)

      ################
      # Location cell#
      ################
      loc_cell <- paste0('
        <td data-location="', label, '" style="padding: 14px 16px; font-size: 14px;
            font-weight: 700; color: #555; text-align: center; vertical-align: middle;
            width: 120px; border-right: 1px solid #e0e0e0; white-space: nowrap;">',
            label, '</td>')

      paste0('<tr style="', border, '">', loc_cell, horizon_cells, overall_cell, '</tr>')
    }),
    collapse = ""
  )

  ##########################################
  # Column headers                         #
  ##########################################
  horizon_headers <- paste0(
    sapply(horizons, function(h){
      paste0('
        <th class="sum-th" style="border-right: 1px solid #e0e0e0;">
          <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
            Horizon ', h, '
          </div>
          <div class="sum-th-sub" style="color:#C9B8E8;">Median (Range)</div>
        </th>')
    }),
    collapse = ""
  )

  show_search <- interactive

#------------------------------------------------------------------------------#
# Rendering the table ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: One row per location, but only one location is shown at a time. For    #
# multi-location reports a minimal script filters the visible row to match the  #
# geography dropdown. No sort, search, or highlight star -- they do not apply   #
# when a single location is displayed.                                          #
#------------------------------------------------------------------------------#

  ##############################################################
  # Dropdown sync (multi-location only): show only the         #
  # selected location's row.                                   #
  ##############################################################
  sync_script <- if(interactive){
    '<script>
    (function() {
      function showRtPctRow(locName) {
        var tbody = document.querySelector("#rtPctAgreementTable tbody");
        if (!tbody) return;
        Array.from(tbody.querySelectorAll("tr")).forEach(function(row) {
          var lc = row.querySelector("td[data-location]");
          if (lc) row.style.display = (lc.getAttribute("data-location") === locName) ? "" : "none";
        });
      }
      // Exposed so the plot panel-switch JS can drive the table
      window.syncRtPctTableToPlot = function(locName) { showRtPctRow(locName); };
      function initPctTable() {
        var sel = document.getElementById("geoSelect_RTPA");
        if (!sel) return;
        sel.addEventListener("change", function() {
          var opt = this.options[this.selectedIndex];
          if (opt) showRtPctRow(opt.getAttribute("data-location-name"));
        });
        var opt0 = sel.options[sel.selectedIndex] || sel.options[0];
        if (opt0) showRtPctRow(opt0.getAttribute("data-location-name"));
      }
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", initPctTable);
      } else { initPctTable(); }
    })();
    </script>'
  }else{
    ''
  }

  table_html <- htmltools::HTML(paste0('
<div style="font-family:sans-serif;padding:1rem 0;overflow-x:auto;">
', sync_script, '
  <table id="rtPctAgreementTable" style="width:100%;border-collapse:collapse;
                                        border-top:1px solid #333;border-bottom:1px solid #333;">
    <thead>
      <tr style="border-bottom:1px solid #333;">
        <th class="sum-th" style="border-right: 1px solid #e0e0e0; width: 120px;">Location</th>',
        horizon_headers,
        '<th class="sum-th" style="background:#f7f4fc;color:#522D80;">
          <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
            Overall
          </div>
          <div class="sum-th-sub" style="color:#C9B8E8;">Median (Range)</div>
        </th>
      </tr>
    </thead>
    <tbody>', all_rows, '</tbody>
  </table>
</div>
'))

#------------------------------------------------------------------------------#
# Detailed Methods accordion (config-aware, Median/Range) ----------------------
#------------------------------------------------------------------------------#

  ##############################################################
  # Transmission Season Filter block: only included when the   #
  # No Evaluation period is actually shown (months excluded    #
  # AND present in the testing data).                          #
  ##############################################################
  transmission_filter_block <- if(show_no_eval){
    paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Transmission Season Filter</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Target end dates falling in the non-transmission season (<strong>', nt_label, '</strong>)
      are excluded from all summary statistics. During this period low and highly variable
      counts can distort agreement metrics. Row-level values are retained in the data for
      visual continuity in plots but are not included in any median or range calculations.
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> A forecast whose target end date falls within the
      non-transmission window (', nt_label, ') will appear in the time series plot but
      will not contribute to the reported median percent agreement for any horizon or the
      overall summary.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
')
  }else{
    ''
  }

  methods_html <- htmltools::HTML(paste0('
  <div style="font-family: sans-serif; padding: 0.5rem 0;">

    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      The following definitions describe the methods used to calculate and summarize
      percent agreement between forecasted and observed values, providing a transparent
      and interpretable measure of how closely model predictions align with observed
      counts across forecast horizons and locations.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Row-Level Percent Agreement</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Percent agreement is calculated at the individual forecast level as the ratio of
      the smaller value to the larger value between the forecasted and observed counts,
      expressed as a percentage. This symmetric measure is bounded between 0% and 100%,
      where 100% indicates a perfect match. Rows where the observed count is missing or
      zero are excluded.
    </p>
    <div id="eq-pa-row" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> If the model forecasted 450 ', outcome, ' and 500 were
      observed, percent agreement is (450 / 500) &times; 100 = 90%. If the model
      forecasted 600 and 500 were observed, percent agreement is (500 / 600) &times;
      100 = 83.3%. The direction of the error does not affect the result.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
', transmission_filter_block, '
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Horizon-Level Summaries</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Row-level percent agreement values are grouped by <strong>forecast horizon and
      location</strong>. Within each group, the <strong>median</strong> and
      <strong>range</strong> (minimum and maximum) are computed across all
      transmission-season rows. These summaries capture how forecast accuracy changes as
      the prediction window extends — shorter horizons are generally expected to show
      higher agreement than longer ones.
    </p>
    <div id="eq-pa-horizon" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> If horizon 1 forecasts across all transmission-season
      weeks have a median percent agreement of 88% (range: 70% – 98%), the model is
      typically within 12% of the observed count one week ahead.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Overall Summary</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      An overall summary collapses across all horizons within each location, computing
      the <strong>median</strong> and <strong>range</strong> of percent agreement across
      all transmission-season rows regardless of horizon. This provides a single
      high-level benchmark of forecast performance for each location.
    </p>
    <div id="eq-pa-overall" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> An overall median of 85% indicates that across all
      horizons and transmission-season weeks, the model forecast was within 15% of
      the observed count more than half the time.
    </p>

  </div>

  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css">
  <script src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js"></script>
  <script>
    katex.render(
      "\\\\text{Percent Agreement} = \\\\frac{\\\\min(\\\\text{Forecasted},\\\\, \\\\text{Observed})}{\\\\max(\\\\text{Forecasted},\\\\, \\\\text{Observed})} \\\\times 100",
      document.getElementById("eq-pa-row"),
      { throwOnError: false, displayMode: true }
    );
    katex.render(
      "\\\\text{Horizon Median}_{h,\\\\, l} = \\\\text{median}\\\\left( \\\\{ \\\\text{PA}_{i} : \\\\text{horizon}_i = h,\\\\; \\\\text{location}_i = l,\\\\; \\\\text{is\\\\_transmission}_i = \\\\text{TRUE} \\\\} \\\\right)",
      document.getElementById("eq-pa-horizon"),
      { throwOnError: false, displayMode: true }
    );
    katex.render(
      "\\\\text{Overall Median}_{l} = \\\\text{median}\\\\left( \\\\{ \\\\text{PA}_{i} : \\\\text{location}_i = l,\\\\; \\\\text{is\\\\_transmission}_i = \\\\text{TRUE} \\\\} \\\\right)",
      document.getElementById("eq-pa-overall"),
      { throwOnError: false, displayMode: true }
    );
  </script>
  '))

  methods_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Detailed Methods (Real-Time Percent Agreement)")),
    htmltools::div(class = "accordion-body", methods_html)
  )

#------------------------------------------------------------------------------#
# Location-to-Location Comparison accordion (multi-location only) --------------
#------------------------------------------------------------------------------#
# About: All locations shown at once, sortable and searchable, starting sorted #
# best-to-worst (highest overall median agreement first). Only meaningful when #
# more than one location is present, so it is omitted for single-location      #
# reports. Uses its own table id and sort function so it does not interfere     #
# with the one-location-at-a-time table above.                                 #
#------------------------------------------------------------------------------#

  compare_accordion <- if(interactive){

    ##################################
    # Sortable comparison headers     #
    ##################################
    compare_horizon_headers <- paste0(
      sapply(seq_along(horizons), function(i){
        h <- horizons[i]
        paste0('
          <th class="sum-th" onclick="sortRtPctCompare(', i, ', \'num\')"
              style="border-right: 1px solid #e0e0e0;">
            <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
              Horizon ', h, ' <span style="font-size:10px;">&#8597;</span>
            </div>
            <div class="sum-th-sub" style="color:#C9B8E8;">Median (Range)</div>
          </th>')
      }),
      collapse = ""
    )

    ##################################
    # Intro + sortable/searchable     #
    # all-location table              #
    ##################################
    compare_body <- htmltools::HTML(paste0('
    <p style="font-size: 14px; line-height: 1.6; color: #444; margin: 0 0 1rem 0;">
      Compare percent agreement across all locations at once. The table starts sorted from
      best to worst by overall median agreement; click any column to re-sort, or use the
      search box to find a specific location.
    </p>

    <div style="font-family:sans-serif;padding:0.5rem 0;overflow-x:auto;">
      <script>
        var rtPctCmpSortDir = {};
        function sortRtPctCompare(colIndex, type) {
          var tbody = document.querySelector("#rtPctCompareTable tbody");
          if (!tbody) return;
          var rows = Array.from(tbody.querySelectorAll("tr"));
          var asc  = rtPctCmpSortDir[colIndex] !== true;
          rtPctCmpSortDir[colIndex] = asc;
          rows.sort(function(a, b) {
            var aC = a.querySelectorAll("td")[colIndex];
            var bC = b.querySelectorAll("td")[colIndex];
            if (!aC || !bC) return 0;
            if (type === "num") {
              var av = parseFloat(aC.getAttribute("data-value"));
              var bv = parseFloat(bC.getAttribute("data-value"));
              if (isNaN(av)) av = -Infinity;
              if (isNaN(bv)) bv = -Infinity;
              return asc ? av - bv : bv - av;
            }
            var at = aC.textContent.trim().toLowerCase();
            var bt = bC.textContent.trim().toLowerCase();
            return asc ? at.localeCompare(bt) : bt.localeCompare(at);
          });
          rows.forEach(function(row, i) {
            row.style.borderBottom = i < rows.length - 1 ? "1px solid #e0e0e0" : "";
            tbody.appendChild(row);
          });
        }
        document.addEventListener("DOMContentLoaded", function() {
          var overallCol = ', length(horizons) + 1, ';
          rtPctCmpSortDir[overallCol] = true;   // flip so first sort is descending = best first
          sortRtPctCompare(overallCol, "num");
        });
      </script>

      <table id="rtPctCompareTable" style="width:100%;border-collapse:collapse;
                                          border-top:1px solid #333;border-bottom:1px solid #333;">
        <thead>
          <tr style="border-bottom:1px solid #333;">
            <th class="sum-th" onclick="sortRtPctCompare(0, \'text\')"
                style="border-right: 1px solid #e0e0e0; width: 120px;">
              Location
              <br/>
              <input type="text" id="rtPctCompareSearch" placeholder="Search..."
                onclick="event.stopPropagation();"
                onkeyup="
                  var val = this.value.toLowerCase();
                  var rows = document.querySelectorAll(\'#rtPctCompareTable tbody tr\');
                  rows.forEach(function(row) {
                    var loc = row.querySelector(\'td\');
                    if (loc) row.style.display = loc.textContent.toLowerCase().includes(val) ? \'\' : \'none\';
                  });
                "
                style="margin-top:6px;margin-bottom:6px;padding:4px 8px;font-size:11px;font-weight:400;
                       border:1px solid #ddd;border-radius:4px;width:90%;color:#333;text-transform:none;
                       letter-spacing:0;display:block;margin-left:auto;margin-right:auto;"
              />
            </th>',
            compare_horizon_headers,
            '<th class="sum-th" onclick="sortRtPctCompare(', length(horizons) + 1, ', \'num\')"
                style="background:#f7f4fc;color:#522D80;">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
                Overall <span style="font-size:10px;">&#8597;</span>
              </div>
              <div class="sum-th-sub" style="color:#C9B8E8;">Median (Range)</div>
            </th>
          </tr>
        </thead>
        <tbody>', all_rows, '</tbody>
      </table>
    </div>'))

    htmltools::tags$details(
      class = "accordion",
      htmltools::tags$summary(htmltools::tags$strong("Location-to-Location Comparison")),
      htmltools::div(class = "accordion-body", compare_body)
    )

  }else{
    NULL
  }

#------------------------------------------------------------------------------#
# Assembling the full drop-down ------------------------------------------------
#------------------------------------------------------------------------------#

  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Real-Time Percent Agreement")),
    htmltools::div(
      class = "accordion-body",
      intro_html,
      header_html,
      navigate_html,
      dropdown_html,
      plot_block,
      table_html,
      compare_accordion,
      methods_accordion
    )
  )

}
