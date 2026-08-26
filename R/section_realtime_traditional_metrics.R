#' Render the real-time traditional metrics section
#'
#' Builds the real-time "traditional metrics" drop-down: a table of the
#' weighted interval score (WIS), median absolute error (MAE), and 50% / 95%
#' interval coverage, summarized per location over the real-time operational
#' forecast rows. For multiple locations the table is sortable and searchable
#' (one row per location, all shown at once); for a single location a single
#' summary row is shown. A Detailed Methods accordion explains each metric.
#'
#' WIS is reported as `NA` (shown as a dash) when the forecast carries no
#' interval information beyond the median, in which case WIS would equal MAE.
#'
#' @param traditional.data Output of `traditionalMetricsCalculation()` on the
#'   real-time bundle — the evaluation frame with `location`, `horizon`, and the
#'   broadcast summary columns `WIS_Overall`, `MAE_Overall`, `Cov50_Overall`,
#'   `Cov95_Overall` (plus their `*_Horizon` counterparts).
#' @param impl_meta Metadata list from `extract_implementation_data()`. Used to
#'   resolve location display names.
#' @param outcome Optional outcome label. When `NULL`, resolved from the
#'   crosswalk's outcome rows, then `impl_meta$outcome`.
#' @param variables_crosswalk Validated crosswalk data frame, or `NULL`.
#' @param eval_config Evaluation config list from `create_evaluation_config()`.
#'
#' @return An htmltools `tags$details` object (rendered HTML), or
#'   `invisible(NULL)` when no real-time evaluation data is available.
#'
#' @keywords internal
#' @noRd
section_realtime_traditional_metrics <- function(traditional.data,
                                        impl_meta,
                                        outcome             = NULL,
                                        variables_crosswalk = NULL,
                                        eval_config         = NULL) {

#------------------------------------------------------------------------------#
# Guard: real-time evaluation data must be present -----------------------------
#------------------------------------------------------------------------------#

  if(is.null(traditional.data) ||
     !is.data.frame(traditional.data) ||
     nrow(traditional.data) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Resolving inputs -------------------------------------------------------------
#------------------------------------------------------------------------------#

  if(is.null(eval_config)) eval_config <- create_evaluation_config()

  ###########################
  # Outcome display label   #
  ###########################
  if(is.null(outcome)){

    outcome <- NA_character_

    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk) &&
       all(c("variable_type", "clean_name_full") %in% names(variables_crosswalk))){

      outcome_rows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "outcome", ]

      if(nrow(outcome_rows) > 0){
        clean_names <- unique(outcome_rows$clean_name_full)
        clean_names <- clean_names[
          !is.na(clean_names) & nzchar(clean_names) &
            clean_names != "USER: provide a definition"]
        if(length(clean_names) > 0) outcome <- paste(clean_names, collapse = ", ")
      }
    }

    if(is.na(outcome) || !nzchar(outcome)){
      outcome <- if(!is.null(impl_meta$outcome)){
        paste(impl_meta$outcome, collapse = ", ")
      }else{
        "Observed"
      }
    }
  }

  ###########################
  # Locations               #
  ###########################
  loc_codes <- sort(unique(traditional.data$location))

  resolve_label <- function(cd){
    lab <- NA_character_
    if("location_display" %in% names(traditional.data)){
      cand <- unique(traditional.data$location_display[traditional.data$location == cd])
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
  interactive <- n_loc > 1   # sorting / search only make sense for >1 location

#------------------------------------------------------------------------------#
# Pulling per-location overall metrics -----------------------------------------
#------------------------------------------------------------------------------#
# About: The overall summary columns are broadcast across every row for a       #
# location, so the first non-NA value per location is the per-location value.   #
#------------------------------------------------------------------------------#

  get_overall <- function(cd){
    rows <- traditional.data[traditional.data$location == cd, , drop = FALSE]
    pick <- function(col){
      if(!col %in% names(rows)) return(NA_real_)
      v <- rows[[col]][!is.na(rows[[col]])]
      if(length(v) == 0) NA_real_ else v[1]
    }
    list(
      WIS   = pick("WIS_Overall"),
      MAE   = pick("MAE_Overall"),
      Cov50 = pick("Cov50_Overall"),
      Cov95 = pick("Cov95_Overall")
    )
  }

#------------------------------------------------------------------------------#
# Cell + row builders ----------------------------------------------------------
#------------------------------------------------------------------------------#

  num_cell <- function(val, digits = 2, suffix = "", is_overall = FALSE,
                       metric = ""){
    bg  <- if(is_overall) "background:#f7f4fc;" else "border-right:1px solid #e0e0e0;"
    col <- if(is_overall) "#522D80" else "#222"

    if(is.na(val)){
      txt  <- "\u2014"
      dval <- ""
    }else{
      txt  <- paste0(formatC(val, format = "f", digits = digits), suffix)
      dval <- val
    }

    paste0(
      '<td class="trad-cell" data-metric="', metric,
      '" style="padding:14px 16px;text-align:center;vertical-align:middle;', bg,
      '" data-value="', dval, '">',
      '<div class="trad-val" style="font-size:14px;font-weight:600;color:', col,
      ';white-space:nowrap;">', txt, '</div></td>'
    )
  }

  make_row <- function(i){
    cd     <- loc_codes[i]
    label  <- loc_labels[i]
    border <- if(i < n_loc) "border-bottom:1px solid #e0e0e0;" else ""
    o      <- get_overall(cd)

    loc_cell <- paste0(
      '<td data-location="', label, '" style="padding:14px 16px;font-size:14px;',
      'font-weight:700;color:#555;text-align:center;vertical-align:middle;',
      'width:160px;border-right:1px solid #e0e0e0;white-space:nowrap;">',
      label, '</td>')

    paste0(
      '<tr data-loc-code="', cd, '" style="', border, '">', loc_cell,
      num_cell(o$WIS, 2, metric = "wis"),
      num_cell(o$MAE, 2, metric = "mae"),
      num_cell(if(is.na(o$Cov50)) NA_real_ else o$Cov50 * 100, 1, "%",
               metric = "cov50"),
      num_cell(if(is.na(o$Cov95)) NA_real_ else o$Cov95 * 100, 1, "%",
               metric = "cov95"),
      '</tr>'
    )
  }

  all_rows <- paste0(vapply(seq_len(n_loc), make_row, character(1)), collapse = "")

#------------------------------------------------------------------------------#
# Per-season values for the season selector ------------------------------------
#------------------------------------------------------------------------------#
# About: This section gathers each location's per-season WIS, MAE, and 50/95%  #
# coverage so the season drop-down can repopulate the table in place. Values   #
# are stored raw and formatted in the browser to match the table exactly.      #
#------------------------------------------------------------------------------#

  #############################
  # Seasons present in data   #
  #############################
  seasons <- if("season" %in% names(traditional.data)){
    s <- unique(traditional.data$season)
    sort(s[!is.na(s) & nzchar(s)])
  }else{
    character(0)
  }

  ##########################################
  # Per-location, per-season metric getter #
  ##########################################
  get_season <- function(cd, s){
    rows <- traditional.data[
      traditional.data$location == cd &
        !is.na(traditional.data$season) & traditional.data$season == s, ,
      drop = FALSE]
    pick <- function(col){
      if(!col %in% names(rows)) return(NA_real_)
      v <- rows[[col]][!is.na(rows[[col]])]
      if(length(v) == 0) NA_real_ else v[1]
    }
    list(WIS   = pick("WIS_Season"),
         MAE   = pick("MAE_Season"),
         Cov50 = pick("Cov50_Season"),
         Cov95 = pick("Cov95_Season"))
  }

  ####################################
  # One metric value as JS (or null) #
  ####################################
  js_num <- function(v){
    if(is.null(v) || is.na(v)) "null"
    else formatC(v, format = "f", digits = 6)
  }

  ########################################
  # Metric bundle for one location/scope #
  ########################################
  js_bundle <- function(m){
    paste0("{wis:", js_num(m$WIS), ",mae:", js_num(m$MAE),
           ",cov50:", js_num(m$Cov50), ",cov95:", js_num(m$Cov95), "}")
  }

  #####################################
  # Embedded per-location season data #
  #####################################
  season_data_js <- ""
  if(length(seasons) > 1){

    # One entry per location: overall plus each season
    per_loc <- vapply(seq_len(n_loc), function(i){
      cd    <- loc_codes[i]
      parts <- paste0('"__overall__":', js_bundle(get_overall(cd)))
      for(s in seasons){
        parts <- c(parts, paste0('"', s, '":', js_bundle(get_season(cd, s))))
      }
      paste0('"', cd, '":{', paste(parts, collapse = ","), "}")
    }, character(1))

    # The full lookup object
    season_data_js <- paste0("var rtTradSeasonData={",
                             paste(per_loc, collapse = ","), "};")
  }

  ###########################
  # Season selector markup  #
  ###########################
  season_controls <- ""
  season_script   <- ""
  if(length(seasons) > 1){

    # Overall first, then each season
    opts <- paste0(
      '<option value="__overall__">Overall (all seasons)</option>',
      paste0('<option value="', seasons, '">', seasons, '</option>',
             collapse = ""))

    # Rendered outside the table, centered, styled like the location dropdown
    season_controls <- paste0(
      '<div style="display:flex;justify-content:center;margin:1rem 0;">',
      '<div class="geo-filter-row">',
      '<select id="rtTradSeasonSelect" class="geo-select" ',
      'onchange="setRtTradSeason(this.value)">',
      opts, '</select></div></div>')

    # Repopulation: rewrite each row's cells for the chosen season, then re-sort
    season_script <- paste0(
      '<script>', season_data_js,
      'function rtTradFmt(m,v){',
      'if(v===null||v===undefined||isNaN(v))return "\u2014";',
      'if(m==="cov50"||m==="cov95")return (v*100).toFixed(1)+"%";',
      'return v.toFixed(2);}',
      'function rtTradDataVal(m,v){',
      'if(v===null||v===undefined||isNaN(v))return "";',
      'if(m==="cov50"||m==="cov95")return (v*100);return v;}',
      'function setRtTradSeason(season){',
      'var rows=document.querySelectorAll("#rtTradWrap tr[data-loc-code]");',
      'rows.forEach(function(row){',
      'var code=row.getAttribute("data-loc-code");',
      'var rec=(rtTradSeasonData[code]||{})[season]||null;',
      'row.querySelectorAll("td.trad-cell").forEach(function(td){',
      'var m=td.getAttribute("data-metric");var v=rec?rec[m]:null;',
      'td.setAttribute("data-value",rtTradDataVal(m,v));',
      'var d=td.querySelector(".trad-val");',
      'if(d)d.textContent=rtTradFmt(m,v);});});',
      'if(window.rtTradReSort)window.rtTradReSort();}',
      '</script>')
  }

#------------------------------------------------------------------------------#
# Building the table -----------------------------------------------------------
#------------------------------------------------------------------------------#

  if(interactive){

    ##################################
    # Sortable / searchable table    #
    ##################################
    table_inner <- paste0('
    <p style="font-size:14px;line-height:1.6;color:#444;margin:0 0 1rem 0;">
      Compare traditional scores across all locations at once. Lower average WIS and MAE
      indicate better point and probabilistic accuracy; coverage closer to its
      nominal level (50% and 95%) indicates better-calibrated intervals. Each value is averaged across all forecast dates and horizons. The
      table starts sorted from best to worst by MAE; click any column to re-sort,
      or use the search box to find a specific location.
    </p>

    <div style="font-family:sans-serif;padding:0.5rem 0;overflow-x:auto;">
      <script>
        var rtTradCmpSortDir = {};
        var rtTradLastSort   = { col: 2, type: "num", asc: true };

        // Sort the tbody by a column without toggling direction
        function rtTradSortCore(colIndex, type, asc) {
          var tbody = document.querySelector("#rtTradCompareTable tbody");
          if (!tbody) return;
          var rows = Array.from(tbody.querySelectorAll("tr"));
          rows.sort(function(a, b) {
            var aC = a.querySelectorAll("td")[colIndex];
            var bC = b.querySelectorAll("td")[colIndex];
            if (!aC || !bC) return 0;
            if (type === "num") {
              var av = parseFloat(aC.getAttribute("data-value"));
              var bv = parseFloat(bC.getAttribute("data-value"));
              if (isNaN(av)) av = Infinity;
              if (isNaN(bv)) bv = Infinity;
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

        // Column header click: toggle direction, remember it, then sort
        function sortRtTradCompare(colIndex, type) {
          var asc = rtTradCmpSortDir[colIndex] !== true;
          rtTradCmpSortDir[colIndex] = asc;
          rtTradLastSort = { col: colIndex, type: type, asc: asc };
          rtTradSortCore(colIndex, type, asc);
        }

        // Re-apply the current sort after a season repopulates the cells
        window.rtTradReSort = function() {
          rtTradSortCore(rtTradLastSort.col, rtTradLastSort.type, rtTradLastSort.asc);
        };

        document.addEventListener("DOMContentLoaded", function() {
          rtTradCmpSortDir[2] = false;   // first sort on MAE ascending = best first
          sortRtTradCompare(2, "num");
        });
      </script>

      <table id="rtTradCompareTable" style="width:100%;border-collapse:collapse;
                                          border-top:1px solid #333;border-bottom:1px solid #333;">
        <thead>
          <tr style="border-bottom:1px solid #333;">
            <th class="sum-th" onclick="sortRtTradCompare(0, \'text\')"
                style="border-right:1px solid #e0e0e0;width:160px;">
              Location
              <br/>
              <input type="text" id="rtTradCompareSearch" placeholder="Search..."
                onclick="event.stopPropagation();"
                onkeyup="
                  var val = this.value.toLowerCase();
                  var rows = document.querySelectorAll(\'#rtTradCompareTable tbody tr\');
                  rows.forEach(function(row) {
                    var loc = row.querySelector(\'td\');
                    if (loc) row.style.display = loc.textContent.toLowerCase().includes(val) ? \'\' : \'none\';
                  });
                "
                style="margin-top:6px;margin-bottom:6px;padding:4px 8px;font-size:11px;font-weight:400;
                       border:1px solid #ddd;border-radius:4px;width:90%;color:#333;text-transform:none;
                       letter-spacing:0;display:block;margin-left:auto;margin-right:auto;"
              />
            </th>
            <th class="sum-th" onclick="sortRtTradCompare(1, \'num\')" style="border-right:1px solid #e0e0e0;">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
                Average WIS <span style="font-size:10px;">&#8597;</span>
              </div>
              <div class="sum-th-sub" style="color:#C9B8E8;">Lower Is Better</div>
            </th>
            <th class="sum-th" onclick="sortRtTradCompare(2, \'num\')" style="border-right:1px solid #e0e0e0;">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
                Average MAE <span style="font-size:10px;">&#8597;</span>
              </div>
              <div class="sum-th-sub" style="color:#C9B8E8;">Lower Is Better</div>
            </th>
            <th class="sum-th" onclick="sortRtTradCompare(3, \'num\')" style="border-right:1px solid #e0e0e0;">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
                Average Coverage 50% <span style="font-size:10px;">&#8597;</span>
              </div>
              <div class="sum-th-sub" style="color:#C9B8E8;">Target 50%</div>
            </th>
            <th class="sum-th" onclick="sortRtTradCompare(4, \'num\')" style="">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
                Average Coverage 95% <span style="font-size:10px;">&#8597;</span>
              </div>
              <div class="sum-th-sub" style="color:#C9B8E8;">Target 95%</div>
            </th>
          </tr>
        </thead>
        <tbody>', all_rows, '</tbody>
      </table>
    </div>')

  }else{

    ##################################
    # Single-location: one row       #
    ##################################
    table_inner <- paste0('
    <p style="font-size:14px;line-height:1.6;color:#444;margin:0 0 1rem 0;">
      Traditional scores over the real-time operational forecasts, averaged across all forecast dates and horizons. Lower average WIS
      and MAE indicate better accuracy; coverage closer to its nominal level
      (50% and 95%) indicates better-calibrated intervals.
    </p>

    <div style="font-family:sans-serif;padding:0.5rem 0;overflow-x:auto;">
      <table style="width:100%;border-collapse:collapse;
                    border-top:1px solid #333;border-bottom:1px solid #333;">
        <thead>
          <tr style="border-bottom:1px solid #333;">
            <th class="sum-th" style="border-right:1px solid #e0e0e0;width:160px;">Location</th>
            <th class="sum-th" style="border-right:1px solid #e0e0e0;">
              Average WIS<div class="sum-th-sub" style="color:#C9B8E8;">Lower Is Better</div></th>
            <th class="sum-th" style="border-right:1px solid #e0e0e0;">
              Average MAE<div class="sum-th-sub" style="color:#C9B8E8;">Lower Is Better</div></th>
            <th class="sum-th" style="border-right:1px solid #e0e0e0;">
              Average Coverage 50%<div class="sum-th-sub" style="color:#C9B8E8;">Target 50%</div></th>
            <th class="sum-th" style="">
              Average Coverage 95%<div class="sum-th-sub" style="color:#C9B8E8;">Target 95%</div></th>
          </tr>
        </thead>
        <tbody>', all_rows, '</tbody>
      </table>
    </div>')

  }

  ##########################################
  # Wrapping controls + table for the JS   #
  ##########################################
  table_block <- htmltools::HTML(paste0(
    '<div id="rtTradWrap">', season_controls, table_inner, '</div>',
    season_script))

#------------------------------------------------------------------------------#
# Detailed Methods accordion ---------------------------------------------------
#------------------------------------------------------------------------------#

  methods_html <- htmltools::HTML(paste0('
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css">
  <script src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js"></script>

  <div style="font-size:14px;line-height:1.7;color:#333;">

    <p style="margin:0 0 1rem 0;">
      These are standard forecast scoring rules, computed per forecast from the
      full quantile distribution and then averaged across all forecast dates and
      horizons, summarized per location.
    </p>

    <p style="margin:0 0 0.5rem 0;"><strong>Average Weighted Interval Score (WIS).</strong>
      A proper score that rewards both accuracy and well-calibrated uncertainty,
      built from the median and the symmetric prediction intervals. Lower is
      better. When a forecast provides only a median (no intervals), WIS reduces
      to the absolute error of the median, so it is reported as a dash
      (\u2014) and only MAE is shown.</p>

    <p style="margin:0 0 0.5rem 0;"><strong>Average Absolute Error of the Median (MAE).</strong>
      The mean absolute difference between each observed value and the
      forecast median. Lower is better.</p>
    <div id="rt-trad-katex-mae" style="margin:0.25rem 0 1rem 0;"></div>

    <p style="margin:0 0 0.5rem 0;"><strong>Average Interval Coverage.</strong>
      The proportion of observations that fell inside the forecast\'s 50% and
      95% prediction intervals. Well-calibrated forecasts cover close to their
      nominal level (about 50% and 95% respectively); much lower indicates
      overconfident intervals, much higher indicates overly wide intervals.</p>
    <div id="rt-trad-katex-cov" style="margin:0.25rem 0 0 0;"></div>

  </div>

  <script>
    (function() {
      function r(id, tex) {
        var el = document.getElementById(id);
        if (el && window.katex) {
          try { katex.render(tex, el, { throwOnError: false, displayMode: true }); }
          catch (e) {}
        }
      }
      function go() {
        r("rt-trad-katex-mae", "\\\\mathrm{MAE} = \\\\frac{1}{n}\\\\sum_{i=1}^{n} \\\\left| y_i - \\\\hat{y}_i^{(0.5)} \\\\right|");
        r("rt-trad-katex-cov", "\\\\mathrm{Cov}_{\\\\alpha} = \\\\frac{1}{n}\\\\sum_{i=1}^{n} \\\\mathbf{1}\\\\!\\\\left[ L_i^{\\\\alpha} \\\\le y_i \\\\le U_i^{\\\\alpha} \\\\right]");
      }
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", go);
      } else { go(); }
    })();
  </script>
  '))

  methods_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Detailed Methods (Real-Time Traditional Metrics)")),
    htmltools::div(class = "accordion-body", methods_html)
  )

#------------------------------------------------------------------------------#
# Intro --------------------------------------------------------------------- #
#------------------------------------------------------------------------------#

  intro_html <- htmltools::HTML(paste0('
  <p style="font-size:14px;line-height:1.6;color:#444;margin:0 0 1rem 0;">
    Traditional scoring rules for the ', outcome, ' forecasts over the
    real-time operational forecasts: the weighted interval score (WIS),
    median absolute error (MAE), and 50% / 95% interval coverage.
  </p>'))

#------------------------------------------------------------------------------#
# Assembling the full drop-down ------------------------------------------------
#------------------------------------------------------------------------------#

  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Real-Time Traditional Metrics")),
    htmltools::div(
      class = "accordion-body",
      intro_html,
      table_block,
      methods_accordion
    )
  )

}
