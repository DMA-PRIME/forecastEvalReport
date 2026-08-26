#' Render the Forecast Bias drop-down section
#'
#' Builds the full Forecast Bias accordion for the testing-period block: an
#' intro, the "Forecast Bias Over Time" figure (one panel per location, switched
#' by a geography dropdown), a synced Median (Range) summary table by forecast
#' horizon, a Location-to-Location Comparison accordion (all locations at once,
#' ranked closest-to-zero), and a Detailed Methods accordion. Renders nothing
#' when no testing data is present, matching every other testing-period section.
#'
#' Each cell carries both a percentage-error track (stable rows) and a raw-error
#' track (all transmission rows). The plot's Bias (%) / Raw Counts toggle calls
#' `window.setBiasTableMode("pct" | "raw")`, which swaps the displayed track in
#' both tables and re-ranks the comparison table.
#'
#' Locations are keyed in the DOM by their display label (`location_display`) so
#' the dropdown, plot panels, and table stay in sync; the underlying data is
#' filtered by the raw `location` code, which is what `make_forecast_bias_plot()`
#' expects.
#'
#' @param forecastBias.data Output of `forecastBiasCalculation()` — the
#'   evaluation frame with row-level (`raw_error`, `pct_error`, `is_transmission`,
#'   `is_stable`, `bias_group`) and broadcast horizon/overall summary columns
#'   (`medianPctStableHorizon`, `minRawHorizon`, `medianRawOverall`, ...).
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Used for the
#'   testing-data presence gate and as a fallback location label source.
#' @param outcome Character label for the outcome. When `NULL`, the clean outcome
#'   name is taken from `variables_crosswalk`, then `eval_meta$outcome`, then a
#'   generic fallback.
#' @param variables_crosswalk Validated crosswalk data frame from
#'   `validate_variables_crosswalk()`, or `NULL`. Supplies the clean outcome
#'   label (`clean_name_full` of the `outcome` rows).
#' @param eval_config Evaluation config from `create_evaluation_config()`.
#'   `non_transmission_months`, `stable_threshold`, and `pct_error_cushion` are
#'   used to phrase the methods text. When `NULL`, defaults are used.
#'
#' @return Rendered HTML via [htmltools::tagList()], or `invisible(NULL)` when no
#'   testing data is available.
#'
#' @keywords internal
#' @noRd
section_forecast_bias <- function(forecastBias.data,
                                  eval_meta,
                                  outcome             = NULL,
                                  variables_crosswalk = NULL,
                                  eval_config         = NULL) {

#------------------------------------------------------------------------------#
# Guard: testing data must be present ------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks that the testing data and the forecast bias data  #
# are available before running the rest of the script. If either is missing or #
# empty, the section renders nothing.                                          #
#------------------------------------------------------------------------------#

  ###################################
  # Presence of usable testing data #
  ###################################
  has_testing <- !is.null(eval_meta) &&
    !is.null(eval_meta$testing_data) &&
    is.data.frame(eval_meta$testing_data) &&
    nrow(eval_meta$testing_data) > 0

  ###############################################
  # Render nothing when no testing data present #
  ###############################################
  if(!has_testing) return(invisible(NULL))

  # Rendering nothing when no forecast bias metrics are present
  if(is.null(forecastBias.data) ||
     !is.data.frame(forecastBias.data) ||
     nrow(forecastBias.data) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Resolving inputs -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section resolves the evaluation config, the outcome label, and   #
# the non-transmission month label and footnote used in the prose, falling     #
# back to defaults when inputs are not supplied.                               #
#------------------------------------------------------------------------------#

  #####################
  # Evaluation config #
  #####################
  # Create the config when one is not supplied
  if(is.null(eval_config)) eval_config <- create_evaluation_config()

  # Stable-count threshold for percentage-error rows
  stable_thr <- eval_config$stable_threshold

  # Plus/minus cushion for the Within Range bias group
  cushion    <- eval_config$pct_error_cushion

  #################
  # Outcome label #
  #################
  if(is.null(outcome)){

    # Start from NA and fill from the best available source
    outcome <- NA_character_

    # Clean outcome name from the crosswalk
    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk) &&
       all(c("variable_type", "clean_name_full") %in% names(variables_crosswalk))){

      # Outcome rows from the crosswalk
      outcome_rows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "outcome", ]

      # When outcome rows exist, pull and clean their names
      if(nrow(outcome_rows) > 0){

        # Unique clean names
        clean_names <- unique(outcome_rows$clean_name_full)

        # Drop missing, blank, and placeholder names
        clean_names <- clean_names[
          !is.na(clean_names) &
            nchar(trimws(clean_names)) > 0 &
            clean_names != "USER: provide a definition"]

        # Use the cleaned names when any remain
        if(length(clean_names) > 0) outcome <- paste(clean_names, collapse = ", ")
      }
    }

    # Fallbacks
    if(is.na(outcome) || !nzchar(outcome)){

      # eval_meta outcome if present, else a generic label
      outcome <- if(!is.null(eval_meta$outcome)) paste(eval_meta$outcome, collapse = ", ") else "Observed"
    }
  }

  ##############################################
  # Non-transmission month label for the prose #
  ##############################################
  # Non-transmission months from the config
  nt <- sort(unique(eval_config$non_transmission_months))

  # Build the label across the none, range, and list cases
  nt_label <- if(length(nt) == 0){

    # No months are excluded
    "none"

  # A contiguous range of months
  }else if(identical(as.integer(nt), as.integer(min(nt):max(nt)))){

    # Creating the list of monthes
    paste0(month.name[min(nt)], " \u2013 ", month.name[max(nt)])

  # A discrete list of months
  }else{paste(month.name[nt], collapse = ", ")}

  ########################
  # No Evaluation gating #
  ########################
  data_months  <- unique(as.integer(format(as.Date(forecastBias.data$target_end_date), "%m")))

  # Show the footnote only if excluded months appear in the data
  show_no_eval <- length(nt) > 0 && any(nt %in% data_months)

  # Footnote sentence shown when a No Evaluation period applies
  no_eval_sentence <- if(show_no_eval){

    # The footnote sentence
    paste0(' Periods marked <strong>No Evaluation</strong> (', nt_label,
           ') are excluded from all summaries.')

  # No footnote when nothing is excluded
  }else{''}

#------------------------------------------------------------------------------#
# Resolving location codes and display labels ----------------------------------
#------------------------------------------------------------------------------#
# About: This section filters data by raw `location`; the DOM (dropdown,       #
# panels, table) is keyed by the display label so all three stay in sync. The  #
# labels come from location_display, then eval_meta$locations, then fall back  #
# to the raw code.                                                             #
#------------------------------------------------------------------------------#

  #########################
  # Unique location codes #
  #########################
  loc_codes <- sort(unique(forecastBias.data$location))

  ######################################
  # Resolving a display label per code #
  ######################################
  resolve_label <- function(cd){

    # Start with a NA label
    lab <- NA_character_

    # Prefer the location_display column when present
    if("location_display" %in% names(forecastBias.data)){

      # Candidate display labels for this code
      cand <- unique(forecastBias.data$location_display[forecastBias.data$location == cd])

      # Drop missing and blank candidates
      cand <- cand[!is.na(cand) & nzchar(cand)]

      # Use the first candidate
      if(length(cand) >= 1) lab <- cand[1]
    }

    # Fall back to the eval_meta locations map
    if(is.na(lab) && !is.null(eval_meta$locations) && cd %in% names(eval_meta$locations)){

      # Label from eval_meta
      lab <- unname(eval_meta$locations[[cd]])
    }

    # Final fallback: the raw code
    if(is.na(lab) || !nzchar(lab)) lab <- cd

    # Return the resolved label
    lab
  }

  ######################################
  # Applying the resolver to all codes #
  ######################################
  loc_labels  <- vapply(loc_codes, resolve_label, character(1))

  # Number of locations
  n_loc       <- length(loc_codes)

  # Comparison table only applies with more than one location
  interactive <- n_loc > 1

#------------------------------------------------------------------------------#
# Intro paragraph --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the introduction paragraph shown at the top of   #
# the Forecast Bias drop down, orienting the reader to what bias measures and  #
# what the figure and table show.                                              #
#------------------------------------------------------------------------------#

  #######################################
  # Creating the introduction paragraph #
  #######################################
  intro_html <- htmltools::HTML('
  <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1rem 0;">
    Forecast bias measures the <strong>direction and magnitude</strong> of deviation
    between forecasted and observed counts, where values near zero indicate strong
    alignment, <strong>positive</strong> values indicate <strong>overestimation</strong>,
    and <strong>negative</strong> values indicate <strong>underestimation</strong>. The
    figure and table below summarize bias over time across the overall median and for all
    forecast horizons.
  </p>
  ')

#------------------------------------------------------------------------------#
# Section header ---------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the section header shown above the figure,       #
# making clear that the figure shows forecast bias over time and by forecast   #
# horizon.                                                                     #
#------------------------------------------------------------------------------#

  ###############################
  # Creating the section header #
  ###############################
  header_html <- htmltools::tagList(
    htmltools::div(style = "margin-top: 1.5em;"),
    htmltools::tags$h3(htmltools::tags$strong("Forecast Bias Over Time by Forecast Horizon")),
    htmltools::div(style = "margin-top: 2em;")
  )

#------------------------------------------------------------------------------#
# "To Navigate" box (config-aware non-transmission label) ----------------------
#------------------------------------------------------------------------------#
# About: This section creates the navigation call out, giving the reader clear #
# instructions for the figure, the legend, and the Bias (%) / Raw Counts       #
# toggle.                                                                      #
#------------------------------------------------------------------------------#

  ####################################
  # Creating the navigation call out #
  ####################################
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
        The solid black line shows the <strong>overall median bias</strong> by week, with a
        dashed line for each <strong>forecast horizon</strong>; use the legend to toggle any
        series on or off, including the observed counts. Use <strong>Bias (%)</strong>
        to view error relative to observed counts, or <strong>Raw Counts</strong> to view
        forecast error on the observed scale.', no_eval_sentence, '
      </p>
    </div>
  </div>
  '))

#------------------------------------------------------------------------------#
# Geography dropdown (multi-location only) -------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the geography dropdown when more than one        #
# location is present, letting the reader switch between the per-location      #
# figures.                                                                     #
#------------------------------------------------------------------------------#

  ###################################
  # Creating the geography dropdown #
  ###################################
  dropdown_html <- if(n_loc > 1){

    # Dropdown and spacing for multiple locations
    htmltools::tagList(
      build_geo_dropdown(loc_labels, select_id = "geoSelect_FB"),
      htmltools::div(style = "margin-top: 2em;")
    )

  # Just spacing for a single location
  }else{

    htmltools::div(style = "margin-top: 1.5em;")
  }

#------------------------------------------------------------------------------#
# Building the per-location plots ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds one interactive forecast bias plot per location   #
# by calling make_forecast_bias_plot, keyed by display label so the dropdown,  #
# panels, and table stay in sync.                                              #
#------------------------------------------------------------------------------#

  # DOM id for the multi-panel wrapper
  wrap_id <- "wrap-forecastBiasPlot"

  ##################################
  # Building one plot per location #
  ##################################
  plotlyList <- setNames(
    lapply(loc_codes, function(loc){
      make_forecast_bias_plot(
        data    = forecastBias.data,
        loc     = loc,
        outcome = outcome
      )
    }),
    loc_labels
  )

#------------------------------------------------------------------------------#
# Rendering the figure (single vs multiple locations) --------------------------
#------------------------------------------------------------------------------#
# About: This section renders the figure. A single location is centered        #
# directly; multiple locations are drawn one panel each, shown or hidden by    #
# dropdown-driven JavaScript keyed by display label.                           #
#------------------------------------------------------------------------------#

  if(n_loc == 1){

    ###########################
    # Single location: render #
    ###########################
    plot_block <- htmltools::div(
      style = "display: flex; justify-content: center; width: 100%;",
      htmltools::div(
        class = "plot-panel",
        style = "display: flex; justify-content: center;",
        plotlyList[[1]]
      )
    )

  }else{

    ############################
    # Multiple location panels #
    ############################
    # Panels keyed by display label, shown/hidden by dropdown-driven JS.
    plot_block <- htmltools::tagList(

      ##########################
      # One panel per location #
      ##########################
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

      ###################
      # Panel-switch JS #
      ###################
      htmltools::tags$script(htmltools::HTML(sprintf('
        (function() {

          function applyBiasPlotLocation(locName) {
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
            if (typeof syncBiasTableToPlot === "function") syncBiasTableToPlot(locName);
          }

          var sel = document.getElementById("geoSelect_FB");
          if (sel) {
            sel.addEventListener("change", function() {
              var opt = this.options[this.selectedIndex];
              applyBiasPlotLocation(opt.getAttribute("data-location-name"));
            });
          }

          window.addEventListener("load", function() {
            var sel = document.getElementById("geoSelect_FB");
            if (sel && sel.options[0]) {
              applyBiasPlotLocation(sel.options[0].getAttribute("data-location-name"));
            }
          });

        })();
      ', wrap_id)))
    )
  }

#------------------------------------------------------------------------------#
# Building the location x horizon table (Median + Range, pct/raw tracks) -------
#------------------------------------------------------------------------------#
# About: This section builds the location/horizon table. Each cell carries a   #
# percentage-error track (stable rows) and a raw-error track (all transmission #
# rows), plus the colors for each. The cell renders the percentage track by    #
# default; setBiasTableMode() swaps in the raw track. The summary columns are  #
# broadcast onto every row by the calculator, so slice(1) per (location,       #
# horizon) and per location reads them off directly.                           #                                       #
#------------------------------------------------------------------------------#

  #########################################
  # Forecast horizons present in the data #
  #########################################
  horizons <- sort(unique(forecastBias.data$horizon))

  #########################################
  # Safe scalar round (handles NULL / NA) #
  #########################################
  safe_round1 <- function(x){

    # Coerce to numeric, suppressing warnings
    x <- suppressWarnings(as.numeric(x))

    # Empty or NA in gives NA out
    if(length(x) == 0 || is.na(x[1])) return(NA_real_)

    # Round the first value to one decimal
    round(x[1], 1)
  }

  ########################################
  # Signed formatters (percentage / raw) #
  ########################################
  fmt_pct_r <- function(v) if(is.na(v)) "&mdash;" else paste0(if(v > 0) "+" else "", v, "%")

  #####################################
  # Over / under / neutral cell color #
  #####################################
  bias_color <- function(v, is_overall){

    # Neutral color: purple for overall, dark gray otherwise
    neutral <- if(is_overall) "#522D80" else "#222"

    # Missing value renders neutral
    if(is.na(v)) return(neutral)

    # Positive bias renders as an overestimate
    if(v > 0) return("#B85C30")

    # Negative bias renders as an underestimate
    if(v < 0) return("#2D6A9F")

    # Otherwise neutral
    neutral
  }

  ###########################################
  # Cell builder: median (range), pct + raw #
  ###########################################
  bias_cell <- function(mp, lop, hip, mr, lor, hir, is_overall = FALSE){

    # Cell background: tinted for overall, divider for horizon
    bg        <- if(is_overall) "background: #f7f4fc;" else "border-right: 1px solid #e0e0e0;"

    # Color for the percentage track
    pct_color <- bias_color(mp, is_overall)

    # Color for the raw track
    raw_color <- bias_color(mr, is_overall)

    # Range sub-text color
    sub_color <- if(is_overall) "#9B85C8" else "#555"

    # Main number: the formatted median
    main_txt  <- fmt_pct_r(mp)

    # Sub-text: the (low - high) range
    sub_txt   <- if(is.na(mp)) "" else paste0("(", fmt_pct_r(lop), " \u2013 ", fmt_pct_r(hip), ")")

    # Value carried for sort and rank
    dval      <- if(is.na(mp)) "" else mp

    # Emit the cell with both tracks as data-attributes
    paste0('
      <td style="padding: 14px 16px; text-align: center; vertical-align: middle; ', bg, '"
           data-value="', dval, '"
           data-pct-med="', mp, '" data-pct-lo="', lop, '" data-pct-hi="', hip, '"
           data-raw-med="', mr, '" data-raw-lo="', lor, '" data-raw-hi="', hir, '">
        <div class="bias-main" style="font-size: 14px; font-weight: 600; color: ', pct_color, '; white-space: nowrap;"
             data-pct-color="', pct_color, '" data-raw-color="', raw_color, '">', main_txt, '</div>
        <div class="bias-sub" style="font-size: 13px; font-weight: 400; color: ', sub_color, '; white-space: nowrap;">', sub_txt, '</div>
      </td>')
  }

  ########################
  # One row per location #
  ########################
  all_rows <- paste0(
    sapply(seq_len(n_loc), function(i){

      # Raw location code
      code   <- loc_codes[i]

      # Display label
      label  <- loc_labels[i]

      # Row divider except on the last row
      border <- if(i < n_loc) "border-bottom: 1px solid #e0e0e0;" else ""

      # Rows for this location
      loc_data <- forecastBias.data %>% dplyr::filter(location == code)

      #################
      # Horizon cells #
      #################
      horizon_cells <- paste0(
        sapply(horizons, function(h){

          # First row for this horizon (summaries are broadcast)
          hr <- loc_data %>% dplyr::filter(horizon == h) %>% dplyr::slice(1)
          if(nrow(hr) == 0){

            # Empty cell when the horizon is absent
            bias_cell(NA, NA, NA, NA, NA, NA, is_overall = FALSE)

          # Otherwise the horizon median and range
          }else{

            bias_cell(
              safe_round1(hr$medianPctStableHorizon), safe_round1(hr$minPctStableHorizon), safe_round1(hr$maxPctStableHorizon),
              safe_round1(hr$medianRawHorizon),       safe_round1(hr$minRawHorizon),       safe_round1(hr$maxRawHorizon),
              is_overall = FALSE
            )
          }
        }),
        collapse = ""
      )

      ################
      # Overall cell #
      ################

      # First row carries the overall summaries
      orow <- loc_data %>% dplyr::slice(1)

      # Creating the horizon-by-horizon summary
      overall_cell <- if(nrow(orow) == 0){

        # Empty cell when the location has no rows
        bias_cell(NA, NA, NA, NA, NA, NA, is_overall = TRUE)

      # Otherwise the overall median and range
      }else{

        # Creating the bias cell
        bias_cell(
          safe_round1(orow$medianPctStableOverall), safe_round1(orow$minPctStableOverall), safe_round1(orow$maxPctStableOverall),
          safe_round1(orow$medianRawOverall),       safe_round1(orow$minRawOverall),       safe_round1(orow$maxRawOverall),
          is_overall = TRUE
        )

      }

      #################
      # Location cell #
      #################
      loc_cell <- paste0('
        <td data-location="', label, '" style="padding: 14px 16px; font-size: 14px;
            font-weight: 700; color: #555; text-align: center; vertical-align: middle;
            width: 120px; border-right: 1px solid #e0e0e0; white-space: nowrap;">',
            label, '</td>')

      # Assemble the full row
      paste0('<tr style="', border, '">', loc_cell, horizon_cells, overall_cell, '</tr>')
    }),
    collapse = ""
  )

  #####################################
  # Plain column headers (main table) #
  #####################################
  horizon_headers <- paste0(
    sapply(horizons, function(h){

      # Header cell for this horizon
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

#------------------------------------------------------------------------------#
# Rendering the main table -----------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the main table, which has one row per location,  #
# but only one location is shown at a time. The always-present script defines  #
# setBiasTableMode (the plot's pct/raw toggle target) and, for multi-location  #
# reports, the dropdown row sync. No sort, search, or highlight star -- they   #
# do not apply when one location is shown.                                     #
#------------------------------------------------------------------------------#

  ###############################
  # Always-present table script #
  ###############################
  main_script <- '<script>
    (function() {

      // Mode toggle (pct <-> raw): swap the displayed track in BOTH tables,
      // then re-rank the comparison table. Called by the plot toggle.
      window.setBiasTableMode = function(mode) {
        var fmtPct = function(v){ return isNaN(v) ? "&mdash;" : (v > 0 ? "+" : "") + parseFloat(v).toFixed(1) + "%"; };
        var fmtRaw = function(v){ return isNaN(v) ? "&mdash;" : (v > 0 ? "+" : "") + parseFloat(v).toFixed(1); };
        var raw    = (mode === "raw");
        var cells  = document.querySelectorAll("#forecastBiasTable tbody td[data-pct-med], #biasCompareTable tbody td[data-pct-med]");
        cells.forEach(function(td) {
          var mainEl = td.querySelector(".bias-main");
          var subEl  = td.querySelector(".bias-sub");
          if (!mainEl || !subEl) return;
          var pre = raw ? "data-raw-" : "data-pct-";
          var med = parseFloat(td.getAttribute(pre + "med"));
          var lo  = parseFloat(td.getAttribute(pre + "lo"));
          var hi  = parseFloat(td.getAttribute(pre + "hi"));
          var fmt = raw ? fmtRaw : fmtPct;
          mainEl.innerHTML   = fmt(med);
          mainEl.style.color = mainEl.getAttribute(raw ? "data-raw-color" : "data-pct-color");
          subEl.innerHTML    = isNaN(med) ? "" : "(" + fmt(lo) + " \u2013 " + fmt(hi) + ")";
          td.setAttribute("data-value", isNaN(med) ? "" : med);
        });
        if (typeof window.reRankBiasCompare === "function") window.reRankBiasCompare();
      };

      // Show only the selected location row (multi-location)
      function showBiasRow(locName) {
        var tbody = document.querySelector("#forecastBiasTable tbody");
        if (!tbody) return;
        Array.from(tbody.querySelectorAll("tr")).forEach(function(row) {
          var lc = row.querySelector("td[data-location]");
          if (lc) row.style.display = (lc.getAttribute("data-location") === locName) ? "" : "none";
        });
      }
      window.syncBiasTableToPlot = function(locName) { showBiasRow(locName); };

      function initBiasTable() {
        var sel = document.getElementById("geoSelect_FB");
        if (!sel) return;
        sel.addEventListener("change", function() {
          var opt = this.options[this.selectedIndex];
          if (opt) showBiasRow(opt.getAttribute("data-location-name"));
        });
        var opt0 = sel.options[sel.selectedIndex] || sel.options[0];
        if (opt0) showBiasRow(opt0.getAttribute("data-location-name"));
      }
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", initBiasTable);
      } else { initBiasTable(); }

    })();
  </script>'

  #################################
  # Rendering the main table HTML #
  #################################
  table_html <- htmltools::HTML(paste0('
<div style="font-family:sans-serif;padding:1rem 0;overflow-x:auto;">
', main_script, '
  <table id="forecastBiasTable" style="width:100%;border-collapse:collapse;
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
# Location-to-Location Comparison accordion (multi-location only) --------------
#------------------------------------------------------------------------------#
# About: This section builds the location to location comparison accordian     #
# which shows all locations at once, sortable and searchable, starting ranked  #
# from best to worst -- "best" being the smallest absolute bias (closest to    #
# zero). Numeric columns sort by |value| so the ranking is by distance from    #
# zero in either direction; rows without a value sort to the bottom. A mode    #
# switch re-applies the current ranking via window.reRankBiasCompare().        #
#------------------------------------------------------------------------------#

  ######################################################
  # Building the comparison accordion (multi-location) #
  ######################################################
  compare_accordion <- if(interactive){

    # Overall column index (after the horizon columns)
    overall_col <- length(horizons) + 1

    ###############################
    # Sortable comparison headers #
    ###############################
    compare_horizon_headers <- paste0(
      sapply(seq_along(horizons), function(i){

        # Horizon value for this column
        h <- horizons[i]

        # Sortable header cell for this horizon
        paste0('
          <th class="sum-th" onclick="sortBiasCompare(', i, ', \'num\')"
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
    # Sort / rank script (abs value) #
    ##################################
    compare_script <- paste0('
      <script>
        (function() {
          var biasCmpState = { col: ', overall_col, ', type: "num", dir: "asc" };

          // Closest-to-zero ranking key: |value|, or null when missing
          function cellKey(td) {
            var v = parseFloat(td.getAttribute("data-value"));
            return isNaN(v) ? null : Math.abs(v);
          }

          function applyBiasCompareSort(col, type, dir) {
            var tbody = document.querySelector("#biasCompareTable tbody");
            if (!tbody) return;
            var rows = Array.from(tbody.querySelectorAll("tr"));
            rows.sort(function(a, b) {
              var aC = a.querySelectorAll("td")[col];
              var bC = b.querySelectorAll("td")[col];
              if (!aC || !bC) return 0;
              if (type === "num") {
                var aa = cellKey(aC), bb = cellKey(bC);
                if (aa === null && bb === null) return 0;
                if (aa === null) return 1;   // missing always last
                if (bb === null) return -1;
                return dir === "asc" ? aa - bb : bb - aa;
              }
              var at = aC.textContent.trim().toLowerCase();
              var bt = bC.textContent.trim().toLowerCase();
              return dir === "asc" ? at.localeCompare(bt) : bt.localeCompare(at);
            });
            rows.forEach(function(row, i) {
              row.style.borderBottom = i < rows.length - 1 ? "1px solid #e0e0e0" : "";
              tbody.appendChild(row);
            });
            biasCmpState = { col: col, type: type, dir: dir };
          }

          window.sortBiasCompare = function(col, type) {
            var dir = (biasCmpState.col === col && biasCmpState.dir === "asc") ? "desc" : "asc";
            applyBiasCompareSort(col, type, dir);
          };

          // Re-apply current ranking (used after a pct<->raw mode switch)
          window.reRankBiasCompare = function() {
            applyBiasCompareSort(biasCmpState.col, biasCmpState.type, biasCmpState.dir);
          };

          document.addEventListener("DOMContentLoaded", function() {
            applyBiasCompareSort(', overall_col, ', "num", "asc");   // closest-to-zero first
          });
        })();
      </script>')

    ############################
    # Intro + comparison table #
    ############################
    compare_body <- htmltools::HTML(paste0('
    <p style="font-size: 14px; line-height: 1.6; color: #444; margin: 0 0 1rem 0;">
      Compare forecast bias across all locations at once. The table starts ranked from best
      to worst by overall bias, with the locations closest to zero (least over- or
      under-prediction) first; click any column to re-sort, or use the search box to find a
      specific location.
    </p>

    <div style="font-family:sans-serif;padding:0.5rem 0;overflow-x:auto;">',
      compare_script, '
      <table id="biasCompareTable" style="width:100%;border-collapse:collapse;
                                          border-top:1px solid #333;border-bottom:1px solid #333;">
        <thead>
          <tr style="border-bottom:1px solid #333;">
            <th class="sum-th" onclick="sortBiasCompare(0, \'text\')"
                style="border-right: 1px solid #e0e0e0; width: 120px;">
              Location
              <br/>
              <input type="text" id="biasCompareSearch" placeholder="Search..."
                onclick="event.stopPropagation();"
                onkeyup="
                  var val = this.value.toLowerCase();
                  var rows = document.querySelectorAll(\'#biasCompareTable tbody tr\');
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
            '<th class="sum-th" onclick="sortBiasCompare(', overall_col, ', \'num\')"
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

    ################################
    # Comparison accordion wrapper #
    ################################
    htmltools::tags$details(
      class = "accordion",
      htmltools::tags$summary(htmltools::tags$strong("Location-to-Location Comparison")),
      htmltools::div(class = "accordion-body", compare_body)
    )

  # No comparison table for a single location
  }else{

    NULL
  }

#------------------------------------------------------------------------------#
# Detailed Methods accordion (config-aware) ------------------------------------
#------------------------------------------------------------------------------#
# About: This section builds the Detailed Methods accordion: the row-level     #
# bias measures, the stability filter, the transmission season filter, the     #
# bias group classification, and the horizon and overall summaries, with KaTeX #
# formulas. The transmission filter block appears only when a No Evaluation    #
# period is shown.                                                             #
#------------------------------------------------------------------------------#

  ####################################
  # Transmission Season Filter block #
  ####################################
  # Only included when the No Evaluation period is actually shown (months
  # excluded AND present in the testing data).
  transmission_filter_block <- if(show_no_eval){

    # The filter-block HTML
    paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Transmission Season Filter</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Target end dates falling in the non-transmission season (<strong>', nt_label, '</strong>) are
      excluded from all summary statistics. During this period, low and highly variable counts can
      distort bias metrics. Row-level values are retained in the data for visual continuity in plots
      but are not included in any median or range calculations.
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> A forecast with a target end date inside the non-transmission season
      will appear in the time series plot but will not contribute to any reported bias summary.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
    ')

  # No filter block when nothing is excluded
  }else{

    ''
  }

  #############################
  # Building the methods HTML #
  #############################
  methods_html <- htmltools::HTML(paste0('

  <div style="font-family: sans-serif; padding: 0.5rem 0;">

    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      The following definitions describe the methods used to calculate and summarize forecast bias
      between forecasted and observed values. These methods are designed to provide a transparent and
      interpretable measure of both the direction and magnitude of forecast error across horizons and
      locations.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Row-Level Bias Measures</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Two bias metrics are calculated at the individual forecast level. The <strong>raw error</strong>
      is the signed difference between the forecasted and observed count &mdash; positive values
      indicate overestimation and negative values indicate underestimation. The
      <strong>percentage error</strong> expresses this difference relative to the observed count. Rows
      where the observed count is missing or zero are excluded from percentage error calculations.
    </p>
    <div id="eq-bias-raw" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <div id="eq-bias-pct" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> If the model forecasted 600 ', outcome, ' and 500 were observed, the
      raw error is +100 (overestimate) and the percentage error is +20%. If the model forecasted 400,
      the raw error is &minus;100 (underestimate) and the percentage error is &minus;20%.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Stability Filter</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Percentage error summaries are restricted to <strong>stable observations</strong>, defined as
      weeks where the observed count is <strong>', stable_thr, ' or greater</strong>. When observed
      counts are very low, small absolute errors produce extreme percentage values that can distort
      summaries. Raw error summaries use all transmission-season rows regardless of observed count.
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> If the observed count is 2 and the forecast is 5, the percentage error
      is +150% &mdash; a large value driven entirely by a low denominator rather than a meaningful
      model failure. This row would be excluded from percentage error summaries but retained for raw
      error summaries.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    ', transmission_filter_block, '

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Bias Group Classification</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Each row is classified into one of four bias groups based on the percentage error and the
      stability flag. Rows with observed counts below ', stable_thr, ' are classified as
      <strong>Insufficient Data</strong>. Remaining rows are classified as
      <strong>Overestimate</strong> (percentage error &gt; +', cushion, '%),
      <strong>Underestimate</strong> (percentage error &lt; &minus;', cushion, '%), or
      <strong>Within Range</strong> (percentage error between &minus;', cushion, '% and +', cushion, '%).
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> A week whose percentage error exceeds +', cushion, '% is classified as
      an Overestimate; one below &minus;', cushion, '% is an Underestimate; anything in between is
      Within Range.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Horizon-Level Summaries</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Row-level bias values are grouped by <strong>forecast horizon and location</strong>. Within each
      group, the <strong>median</strong> and <strong>range</strong> (minimum and maximum)
      are computed separately for raw error (all transmission rows) and percentage error
      (stable transmission rows only). These summaries capture how bias changes as the prediction
      window extends.
    </p>
    <div id="eq-bias-horizon-pct" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <div id="eq-bias-horizon-raw" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> If horizon 1 forecasts have a median percentage error of +8%
      (range: &minus;5% to +18%), the model tends to slightly overestimate one week ahead, though with
      meaningful variability across weeks.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Overall Summary</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      An overall summary collapses across all horizons within each location, computing the
      <strong>median</strong> and <strong>range</strong> of both raw error and percentage
      error across all qualifying transmission-season rows. This provides a single high-level benchmark
      of directional forecast tendency for each location.
    </p>
    <div id="eq-bias-overall-pct" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <div id="eq-bias-overall-raw" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> An overall median percentage error of &minus;5% indicates that across
      all horizons and transmission-season weeks, the model tends to slightly underestimate observed
      counts.
    </p>

  </div>

<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css">
<script src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js"></script>
<script>
  katex.render(
    "\\\\text{Raw Error} = \\\\text{Forecasted} - \\\\text{Observed}",
    document.getElementById("eq-bias-raw"),
    { throwOnError: false, displayMode: true }
  );
  katex.render(
    "\\\\text{Percentage Error} = \\\\frac{\\\\text{Forecasted} - \\\\text{Observed}}{\\\\text{Observed}} \\\\times 100",
    document.getElementById("eq-bias-pct"),
    { throwOnError: false, displayMode: true }
  );
  katex.render(
    "\\\\text{Horizon Median}_{h,\\\\, l} = \\\\text{median}\\\\left( \\\\{ \\\\text{PE}_{i} : \\\\text{horizon}_i = h,\\\\; \\\\text{location}_i = l,\\\\; \\\\text{is\\\\_transmission}_i = \\\\text{TRUE},\\\\; \\\\text{is\\\\_stable}_i = \\\\text{TRUE} \\\\} \\\\right)",
    document.getElementById("eq-bias-horizon-pct"),
    { throwOnError: false, displayMode: true }
  );
  katex.render(
    "\\\\text{Horizon Median}_{h,\\\\, l}^{\\\\text{raw}} = \\\\text{median}\\\\left( \\\\{ \\\\text{RE}_{i} : \\\\text{horizon}_i = h,\\\\; \\\\text{location}_i = l,\\\\; \\\\text{is\\\\_transmission}_i = \\\\text{TRUE} \\\\} \\\\right)",
    document.getElementById("eq-bias-horizon-raw"),
    { throwOnError: false, displayMode: true }
  );
  katex.render(
    "\\\\text{Overall Median}_{l} = \\\\text{median}\\\\left( \\\\{ \\\\text{PE}_{i} : \\\\text{location}_i = l,\\\\; \\\\text{is\\\\_transmission}_i = \\\\text{TRUE},\\\\; \\\\text{is\\\\_stable}_i = \\\\text{TRUE} \\\\} \\\\right)",
    document.getElementById("eq-bias-overall-pct"),
    { throwOnError: false, displayMode: true }
  );
  katex.render(
    "\\\\text{Overall Median}_{l}^{\\\\text{raw}} = \\\\text{median}\\\\left( \\\\{ \\\\text{RE}_{i} : \\\\text{location}_i = l,\\\\; \\\\text{is\\\\_transmission}_i = \\\\text{TRUE} \\\\} \\\\right)",
    document.getElementById("eq-bias-overall-raw"),
    { throwOnError: false, displayMode: true }
  );
</script>
'))

  ########################################
  # Wrapping the methods in an accordion #
  ########################################
  methods_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Detailed Methods (Forecast Bias)")),
    htmltools::div(class = "accordion-body", methods_html)
  )

#------------------------------------------------------------------------------#
# Assembling the full drop-down ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section assembles the full Forecast Bias accordion in order:     #
# intro, header, navigation, dropdown, figure, table, the comparison           #
# accordion, and the methods accordion.                                        #
#------------------------------------------------------------------------------#

  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Forecast Bias")),
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
