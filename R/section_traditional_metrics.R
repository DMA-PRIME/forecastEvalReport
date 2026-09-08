#' Render the traditional metrics section
#'
#' Builds the testing-period "traditional metrics" drop-down: a table of the
#' weighted interval score (WIS), median absolute error (MAE), WIS directional
#' components, and 50% / 80% / 95%
#' interval coverage, summarized per location over the transmission-season
#' testing rows. For multiple locations the table is sortable and searchable
#' (one row per location, all shown at once); for a single location a single
#' summary row is shown. A Detailed Methods accordion explains each metric.
#'
#' WIS is reported as `NA` (shown as a dash) when the forecast carries no
#' interval information beyond the median, in which case WIS would equal MAE.
#'
#' @param traditional.data Output of `traditionalMetricsCalculation()` — the
#'   evaluation frame with `location`, `horizon`, and the broadcast summary
#'   columns `WIS_Overall`, `MAE_Overall`, `Cov50_Overall`, `Cov95_Overall`
#'   (plus their `*_Horizon` counterparts).
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Used to
#'   confirm testing data is present and to resolve location display names.
#' @param outcome Optional outcome label. When `NULL`, resolved from the
#'   crosswalk's outcome rows, then `eval_meta$outcome`.
#' @param variables_crosswalk Validated crosswalk data frame, or `NULL`.
#' @param eval_config Evaluation config list from `create_evaluation_config()`.
#' @param population_crosswalk Optional population lookup used by the observed
#'   trend calls in the trend/phase-specific table.
#'
#' @return An htmltools `tags$details` object (rendered HTML), or
#'   `invisible(NULL)` when no testing data is available.
#'
#' @keywords internal
#' @noRd
section_traditional_metrics <- function(traditional.data,
                                        eval_meta,
                                        outcome             = NULL,
                                        variables_crosswalk = NULL,
                                        eval_config         = NULL,
                                        population_crosswalk = NULL) {

#------------------------------------------------------------------------------#
# Guard: testing data must be present ------------------------------------------
#------------------------------------------------------------------------------#

  has_testing <- !is.null(eval_meta) &&
    !is.null(eval_meta$testing_data) &&
    is.data.frame(eval_meta$testing_data) &&
    nrow(eval_meta$testing_data) > 0

  if(!has_testing) return(invisible(NULL))
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
      outcome <- if(!is.null(eval_meta$outcome)){
        paste(eval_meta$outcome, collapse = ", ")
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
    if(is.na(lab) && !is.null(eval_meta$locations) && cd %in% names(eval_meta$locations)){
      lab <- unname(eval_meta$locations[[cd]])
    }
    if(is.na(lab) || !nzchar(lab)) lab <- cd
    lab
  }

  loc_labels  <- vapply(loc_codes, resolve_label, character(1))
  n_loc       <- length(loc_codes)
  interactive <- n_loc > 1   # sorting / search only make sense for >1 location

#------------------------------------------------------------------------------#
# Non-transmission month label for the Detailed Methods -------------------------
#------------------------------------------------------------------------------#
# About: Mirrors the Percent Accuracy section. The Transmission Season Filter  #
# block in Detailed Methods is only shown when non-transmission months are     #
# configured and actually appear among the target end dates in the data.       #
#------------------------------------------------------------------------------#

  # Pulling the non-transmission months
  nt <- sort(unique(eval_config$non_transmission_months))

  # Creating the label: No Months Provided
  nt_label <- if(length(nt) == 0){"none"

  # Creating the label: One Continuous Span of Months
  }else if(identical(as.integer(nt), as.integer(min(nt):max(nt)))){

    # Creating the label
    paste0(month.name[min(nt)], " \u2013 ", month.name[max(nt)])

  # Creating the label: Multiple Months Provided
  }else{paste(month.name[nt], collapse = ", ")}

  # Pulling months included in data
  data_months <- if("target_end_date" %in% names(traditional.data)){
    unique(as.integer(format(as.Date(traditional.data$target_end_date), "%m")))
  }else{
    integer(0)
  }

  # Checking whether any no-transmission months are included in the data
  show_no_eval <- length(nt) > 0 && any(nt %in% data_months)

#------------------------------------------------------------------------------#
# Metric availability ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: A metric column is only rendered when at least one location has a     #
# non-NA value for it in a scope the table can display (overall or season).    #
# Metrics that were never computed (e.g., WIS-based scores for median-only     #
# forecasts) are dropped from the table entirely, and the descriptive text     #
# below adapts to describe only the metrics actually shown.                    #
#------------------------------------------------------------------------------#

  ############################################
  # Checking each metric family for any data #
  ############################################
  has_any_value <- function(prefix){

    # Broadcast columns belonging to this metric family that the table shows
    cols <- intersect(paste0(prefix, c("_Overall", "_Season")),
                      names(traditional.data))

    # Unavailable when no columns exist or every value is NA
    length(cols) > 0 &&
      any(!is.na(unlist(traditional.data[cols], use.names = FALSE)))

  }

  #####################################################
  # Full metric order and which of them will be shown #
  #####################################################
  metric_keys      <- c("WIS", "MAE", "Under", "Over", "Cov50", "Cov80", "Cov95")
  metric_available <- vapply(metric_keys, has_any_value, logical(1))

  # Defensive fallback: if nothing is available, keep the full layout so the
  # table still renders with dashes rather than as a location-only table
  if(!any(metric_available)) metric_available[] <- TRUE

  # Metrics that will appear as columns, in display order
  shown_metrics <- metric_keys[metric_available]

  ######################################
  # Display attributes for each metric #
  ######################################
  metric_meta <- list(
    WIS   = list(attr = "wis",   header = "Average WIS",          sub = "Lower Is Better"),
    MAE   = list(attr = "mae",   header = "Average MAE",          sub = "Lower Is Better"),
    Under = list(attr = "under", header = "Underprediction",      sub = "Lower Is Better"),
    Over  = list(attr = "over",  header = "Overprediction",       sub = "Lower Is Better"),
    Cov50 = list(attr = "cov50", header = "Average Coverage 50%", sub = "Target 50%"),
    Cov80 = list(attr = "cov80", header = "Average Coverage 80%", sub = "Target 80%"),
    Cov95 = list(attr = "cov95", header = "Average Coverage 95%", sub = "Target 95%")
  )

  # Coverage metrics are stored as proportions and displayed as percentages
  is_coverage <- function(key) key %in% c("Cov50", "Cov80", "Cov95")

  # JS metric keys (wis, mae, ...) for the columns shown, passed to the
  # nested trend/phase accordion so its metric selector matches this table
  available_metric_attrs <- unname(vapply(shown_metrics, function(k){
    metric_meta[[k]]$attr
  }, character(1)))

  #####################################################
  # Human-readable list of the metric columns shown   #
  #####################################################
  metric_phrase_parts <- c(
    if(metric_available[["WIS"]]) "the <strong>weighted interval score (WIS)</strong>",
    if(metric_available[["MAE"]]) "<strong>median absolute error (MAE)</strong>",
    if(metric_available[["Under"]] || metric_available[["Over"]])
      paste0("<strong>WIS ",
             paste(c(if(metric_available[["Under"]]) "underprediction",
                     if(metric_available[["Over"]])  "overprediction"),
                   collapse = " and "),
             if(metric_available[["Under"]] && metric_available[["Over"]])
               " components</strong>" else " component</strong>"),
    if(any(metric_available[c("Cov50", "Cov80", "Cov95")]))
      paste0("<strong>",
             paste(c(if(metric_available[["Cov50"]]) "50%",
                     if(metric_available[["Cov80"]]) "80%",
                     if(metric_available[["Cov95"]]) "95%"),
                   collapse = " / "),
             " interval coverage</strong>")
  )

  # Joining the parts into a natural-language list
  metric_list_text <- if(length(metric_phrase_parts) == 1){
    metric_phrase_parts
  }else if(length(metric_phrase_parts) == 2){
    paste(metric_phrase_parts, collapse = " and ")
  }else{
    paste0(paste(metric_phrase_parts[-length(metric_phrase_parts)],
                 collapse = ", "),
           ", and ", metric_phrase_parts[length(metric_phrase_parts)])
  }

  ############################################
  # Guidance sentences for the table intros  #
  ############################################
  accuracy_sentence <- if(metric_available[["WIS"]] && metric_available[["MAE"]]){
    "Lower average WIS and MAE indicate better point and probabilistic accuracy."
  }else if(metric_available[["MAE"]]){
    "Lower average MAE indicates better point-forecast accuracy."
  }else if(metric_available[["WIS"]]){
    "Lower average WIS indicates better probabilistic accuracy."
  }else{
    ""
  }

  direction_sentence <- if(metric_available[["Under"]] && metric_available[["Over"]]){
    "Smaller underprediction and overprediction components are better."
  }else if(metric_available[["Under"]]){
    "A smaller underprediction component is better."
  }else if(metric_available[["Over"]]){
    "A smaller overprediction component is better."
  }else{
    ""
  }

  cov_levels_shown <- c(if(metric_available[["Cov50"]]) "50%",
                        if(metric_available[["Cov80"]]) "80%",
                        if(metric_available[["Cov95"]]) "95%")
  coverage_sentence <- if(length(cov_levels_shown) > 0){
    paste0("Coverage closer to its nominal level (",
           paste(cov_levels_shown, collapse = ", "),
           ") indicates better-calibrated intervals.")
  }else{
    ""
  }

  # Note shown only when at least one metric column has been dropped
  omitted_note <- if(!all(metric_available)){
    paste0("Metrics that could not be computed for these forecasts (for ",
           "example, interval-based scores when forecasts include only a ",
           "median) are not shown.")
  }else{
    ""
  }

  # Long labels used by the Detailed Methods omission note
  metric_long_labels <- c(
    WIS   = "average weighted interval score (WIS)",
    MAE   = "average absolute error of the median (MAE)",
    Under = "WIS underprediction component",
    Over  = "WIS overprediction component",
    Cov50 = "50% interval coverage",
    Cov80 = "80% interval coverage",
    Cov95 = "95% interval coverage"
  )
  omitted_list_text <- paste(
    metric_long_labels[metric_keys[!metric_available]], collapse = ", ")

  ##########################################
  # Default sort column (metric cols = 1+) #
  ##########################################
  sort_key   <- if(metric_available[["MAE"]]) "MAE" else shown_metrics[1]
  sort_index <- match(sort_key, shown_metrics)
  sort_label <- metric_meta[[sort_key]]$header

  ############################################
  # Header cells for the sortable table      #
  ############################################
  metric_headers_sortable <- paste0(vapply(seq_along(shown_metrics), function(j){
    key  <- shown_metrics[j]
    meta <- metric_meta[[key]]
    last <- j == length(shown_metrics)
    paste0(
      '<th class="sum-th" onclick="sortTradCompare(', j, ', \'num\')" ',
      'style="font-size:12px;',
      if(!last) 'border-right:1px solid #e0e0e0;' else '', '">',
      '<div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">',
      meta$header, ' <span style="font-size:10px;">&#8597;</span></div>',
      '<div class="sum-th-sub" style="color:#C9B8E8;">', meta$sub, '</div></th>'
    )
  }, character(1)), collapse = "")

  ############################################
  # Header cells for the single-location one #
  ############################################
  metric_headers_static <- paste0(vapply(seq_along(shown_metrics), function(j){
    key  <- shown_metrics[j]
    meta <- metric_meta[[key]]
    last <- j == length(shown_metrics)
    paste0(
      '<th class="sum-th" style="font-size:12px;',
      if(!last) 'border-right:1px solid #e0e0e0;' else '', '">',
      meta$header,
      '<div class="sum-th-sub" style="color:#C9B8E8;">', meta$sub, '</div></th>'
    )
  }, character(1)), collapse = "")

#------------------------------------------------------------------------------#
# Calculating trend- and phase-specific traditional metrics -------------------#
#------------------------------------------------------------------------------#

  trend_phase_error <- NULL
  trend_phase_result <- tryCatch(
    trendPhaseTraditionalCalculation(
      traditional.data = traditional.data,
      population = population_crosswalk,
      eval_config = eval_config,
      week_days = if(!is.null(eval_meta$time_step)) eval_meta$time_step else 7
    ),
    error = function(e){
      trend_phase_error <<- paste0(
        "The trend/phase table could not be calculated: ", conditionMessage(e)
      )
      message(trend_phase_error)
      list(summary = data.frame(), data = data.frame())
    }
  )

  trend_phase_accordion <- build_trend_phase_traditional(
    performance_summary = trend_phase_result$summary,
    location_codes = loc_codes,
    location_labels = loc_labels,
    phase_data = trend_phase_result$data,
    status_message = trend_phase_error,
    time_step = if(!is.null(eval_meta$time_step)) eval_meta$time_step else 7
  )

  nested_phase_error <- trend_phase_error
  nested_phase_summary <- tryCatch(
    trendPhaseTraditionalMetricsCalculation(trend_phase_result$data),
    error = function(e){
      nested_phase_error <<- paste0(
        "The traditional-metric trend/phase table could not be calculated: ",
        conditionMessage(e)
      )
      message(nested_phase_error)
      data.frame()
    }
  )

  nested_phase_accordion <- build_trend_phase_traditional_metrics(
    performance_summary = nested_phase_summary,
    location_codes = loc_codes,
    location_labels = loc_labels,
    phase_data = trend_phase_result$data,
    status_message = nested_phase_error,
    available_metrics = available_metric_attrs
  )

#------------------------------------------------------------------------------#
# Pulling per-location overall metrics -----------------------------------------
#------------------------------------------------------------------------------#
# About: The overall summary columns are broadcast across every row for a      #
# location, so the first non-NA value per location is the per-location value.  #
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
      Under = pick("Under_Overall"),
      Over  = pick("Over_Overall"),
      Cov50 = pick("Cov50_Overall"),
      Cov80 = pick("Cov80_Overall"),
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
      paste0(vapply(shown_metrics, function(key){
        if(is_coverage(key)){
          num_cell(if(is.na(o[[key]])) NA_real_ else o[[key]] * 100, 1, "%",
                   metric = metric_meta[[key]]$attr)
        }else{
          num_cell(o[[key]], 2, metric = metric_meta[[key]]$attr)
        }
      }, character(1)), collapse = ""),
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
         Under = pick("Under_Season"),
         Over  = pick("Over_Season"),
         Cov50 = pick("Cov50_Season"),
         Cov80 = pick("Cov80_Season"),
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
           ",under:", js_num(m$Under), ",over:", js_num(m$Over),
           ",cov50:", js_num(m$Cov50), ",cov80:", js_num(m$Cov80),
           ",cov95:", js_num(m$Cov95), "}")
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
    season_data_js <- paste0("var tradSeasonData={",
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
      '<select id="tradSeasonSelect" class="geo-select" ',
      'onchange="setTradSeason(this.value)">',
      opts, '</select></div></div>')

    # Repopulation: rewrite each row's cells for the chosen season, then re-sort
    season_script <- paste0(
      '<script>', season_data_js,
      'function tradFmt(m,v){',
      'if(v===null||v===undefined||isNaN(v))return "\u2014";',
      'if(m==="cov50"||m==="cov80"||m==="cov95")return (v*100).toFixed(1)+"%";',
      'return v.toFixed(2);}',
      'function tradDataVal(m,v){',
      'if(v===null||v===undefined||isNaN(v))return "";',
      'if(m==="cov50"||m==="cov80"||m==="cov95")return (v*100);return v;}',
      'function setTradSeason(season){',
      'var rows=document.querySelectorAll("#tradWrap tr[data-loc-code]");',
      'rows.forEach(function(row){',
      'var code=row.getAttribute("data-loc-code");',
      'var rec=(tradSeasonData[code]||{})[season]||null;',
      'row.querySelectorAll("td.trad-cell").forEach(function(td){',
      'var m=td.getAttribute("data-metric");var v=rec?rec[m]:null;',
      'td.setAttribute("data-value",tradDataVal(m,v));',
      'var d=td.querySelector(".trad-val");',
      'if(d)d.textContent=tradFmt(m,v);});});',
      'if(window.tradReSort)window.tradReSort();}',
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
    <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1rem 0;">
      Compare traditional scores across all locations at once.
      ', accuracy_sentence, '
      ', direction_sentence, '
      ', coverage_sentence, '
      Each value is averaged across all forecast dates and horizons.
      ', omitted_note, '
      The table starts sorted from best to worst by ', sort_label, '; click any
      column to re-sort, or use the search box to find a specific location.
    </p>

    <div style="font-family:sans-serif;padding:0.5rem 0;overflow-x:auto;">
      <script>
        var tradCmpSortDir = {};
        var tradLastSort   = { col: ', sort_index, ', type: "num", asc: true };

        // Sort the tbody by a column without toggling direction
        function tradSortCore(colIndex, type, asc) {
          var tbody = document.querySelector("#tradCompareTable tbody");
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
        function sortTradCompare(colIndex, type) {
          var asc = tradCmpSortDir[colIndex] !== true;
          tradCmpSortDir[colIndex] = asc;
          tradLastSort = { col: colIndex, type: type, asc: asc };
          tradSortCore(colIndex, type, asc);
        }

        // Re-apply the current sort after a season repopulates the cells
        window.tradReSort = function() {
          tradSortCore(tradLastSort.col, tradLastSort.type, tradLastSort.asc);
        };

        document.addEventListener("DOMContentLoaded", function() {
          tradCmpSortDir[', sort_index, '] = false;   // first sort ascending = best first
          sortTradCompare(', sort_index, ', "num");
        });
      </script>

      <table id="tradCompareTable" style="width:100%;border-collapse:collapse;
                                          border-top:1px solid #333;border-bottom:1px solid #333;">
        <thead>
          <tr style="border-bottom:1px solid #333;">
            <th class="sum-th" onclick="sortTradCompare(0, \'text\')"
                style="border-right:1px solid #e0e0e0;width:160px;">
              Location
              <br/>
              <input type="text" id="tradCompareSearch" placeholder="Search..."
                onclick="event.stopPropagation();"
                onkeyup="
                  var val = this.value.toLowerCase();
                  var rows = document.querySelectorAll(\'#tradCompareTable tbody tr\');
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
            ', metric_headers_sortable, '
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
    <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1rem 0;">
      Traditional scores over the transmission-season testing period, averaged
      across all forecast dates and horizons.
      ', accuracy_sentence, '
      ', direction_sentence, '
      ', coverage_sentence, '
      ', omitted_note, '
    </p>

    <div style="font-family:sans-serif;padding:0.5rem 0;overflow-x:auto;">
      <table style="width:100%;border-collapse:collapse;
                    border-top:1px solid #333;border-bottom:1px solid #333;">
        <thead>
          <tr style="border-bottom:1px solid #333;">
            <th class="sum-th" style="border-right:1px solid #e0e0e0;width:160px;">Location</th>
            ', metric_headers_static, '
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
    '<div id="tradWrap">', season_controls, table_inner, '</div>',
    season_script))

#------------------------------------------------------------------------------#
# Detailed Methods accordion ---------------------------------------------------
#------------------------------------------------------------------------------#

  ####################################
  # Transmission Season Filter block #
  ####################################
  transmission_filter_block <- if(show_no_eval){

    # Text to show
    paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Transmission Season Filter</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Target end dates falling in the non-transmission season (<strong>', nt_label, '</strong>)
      are excluded from all summary statistics. During this period low and highly variable
      counts can distort scoring metrics. Scores for these dates are not included in any
      average shown in this section.
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> A forecast whose target end date falls within the
      non-transmission window (', nt_label, ') will not contribute to the averaged
      WIS, MAE, directional components, or interval coverage for any horizon, season,
      or the overall summary.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
  ')

  #######################################
  # No text needed: No evaluation model #
  #######################################
  }else{''}

  ##############################################
  # Trend and epidemic phase explanation block #
  ##############################################
  trend_phase_methods_block <- paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">
      Trend and Phase Breakdown
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      The nested Trend/Phase-Specific Performance table uses the same
      per-forecast <strong>traditional scores</strong> described above. As in
      the Percent Accuracy and Forecast Bias sections, trend and phase labels
      simply divide those scores into clinically meaningful parts of the
      observed epidemic curve; they do not change how any score is calculated.
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      <strong>Observed trend:</strong> Counts are first converted to rates per
      100,000 population, then compared with the preceding target week. Within
      each location, the distribution of observed week-to-week rate changes
      supplies the cut points for <strong>Large Increase</strong>,
      <strong>Increase</strong>, <strong>Stable</strong>,
      <strong>Decrease</strong>, and <strong>Large Decrease</strong>. A raw
      weekly change smaller than <strong>', eval_config$stable_threshold,
      '</strong> counts is treated as Stable so very small count changes are not
      overstated.
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      <strong>Observed phase:</strong> For each location and season, the Peak is
      the continuous set of observed weeks surrounding the seasonal maximum
      that remain within <strong>', eval_config$peak_window,
      '%</strong> of that maximum. Weeks before the Peak are labeled
      <strong>Ascension</strong>; weeks after it are labeled
      <strong>Decline</strong>. The phases are determined only from observed
      target-date data and therefore do not change by forecast horizon.
    </p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      Each table cell reports the <strong>average</strong> of the selected
      traditional metric for rows with that observed trend and phase. The
      metric and horizon selectors determine which score is averaged within
      each combination; <strong>Overall</strong> pools all eligible
      forecast-target pairs across horizons. The displayed <em>n</em> is the
      number of pairs contributing to the cell. A dash means the required
      forecast quantiles or eligible scores were unavailable.
    </p>
  ')

  ####################################################################
  # Creating the remainder of the methods for the traditional scores #
  ####################################################################
  methods_html <- htmltools::HTML(paste0('
  <div style="font-family: sans-serif; padding: 0.5rem 0;">

    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      The following definitions describe the standard forecast scoring rules used in
      this section. Each score is computed per forecast from the full quantile
      distribution, then averaged across all transmission-season forecast dates and
      horizons and summarized per location, providing a transparent and interpretable
      view of both point-forecast accuracy and the calibration of forecast uncertainty.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
', if(metric_available[["WIS"]]) paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Average Weighted Interval Score (WIS)</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      The weighted interval score is a proper scoring rule that evaluates the entire
      forecast distribution, rewarding forecasts that are both close to the observed
      value and honest about their uncertainty. It combines the absolute error of the
      median with a penalty for each symmetric prediction interval, where wider
      intervals and intervals that miss the observation both increase the score.
      Lower is better. WIS is on the same scale as the observed data, so it can be
      read like an absolute error. Here <em>y</em> is the observed value, <em>m</em>
      is the forecast median, <em>K</em> is the number of prediction intervals, and
      IS<sub>&alpha;</sub> is the interval score of the corresponding central
      prediction interval.
    </p>
    <div id="eq-trad-wis" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> If Model A averages a WIS of 12 and Model B averages
      a WIS of 18 on the same ', outcome, ' targets, the forecast distributions from
      Model A were, on average, closer to the observed counts once both accuracy and
      interval calibration are accounted for. When a forecast provides only a median
      (no intervals), WIS reduces to the absolute error of the median, so it is shown
      as a dash (&mdash;) and MAE should be used instead.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
') else '', if(metric_available[["MAE"]]) paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Average Absolute Error of the Median (MAE)</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      The average absolute error of the median measures point-forecast accuracy: the
      mean absolute difference between each observed value and the corresponding
      forecast median. It ignores the rest of the forecast distribution, making it a
      useful companion to WIS for separating point accuracy from interval calibration.
      Lower is better, and MAE is expressed in the same units as the observed data.
    </p>
    <div id="eq-trad-mae" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> If the model issued median forecasts of 450, 520, and
      480 ', outcome, ' in three consecutive weeks while 500 were observed each week,
      the absolute errors are 50, 20, and 20, giving an MAE of 30 &mdash; the median
      forecast missed the observed count by 30 ', outcome, ' per week on average.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
') else '', if(metric_available[["Under"]] || metric_available[["Over"]]) paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Underprediction and Overprediction</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      WIS can be decomposed into three non-negative parts, calculated by
      <code>scoringutils</code>: a dispersion component reflecting the width of the
      prediction intervals, an underprediction penalty accrued when the forecast
      distribution sits below the observation, and an overprediction penalty accrued
      when it sits above. Comparing the two directional components reveals whether a
      model systematically leans low or high. Zero means no penalty in that direction
      was assigned, and smaller average values are better.
    </p>
    <div id="eq-trad-decomp" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> An average underprediction of 8 alongside an average
      overprediction of 2 indicates that when forecasts missed, they usually sat below
      the observed counts &mdash; the model tended to underestimate the trajectory.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
') else '', if(length(cov_levels_shown) > 0) paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Average Interval Coverage</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Interval coverage is the proportion of observations that fell inside the ',
      paste(cov_levels_shown, collapse = ", "), ' prediction intervals, where
      <em>L</em> and <em>U</em> are the lower and upper bounds of the interval. The
      50%, 80%, and 95% intervals use the 25th/75th, 10th/90th, and 2.5th/97.5th
      quantiles, respectively. A well-calibrated forecast covers close to its nominal
      level: coverage far below the target signals overconfident (too-narrow)
      intervals, while coverage far above the target signals overly wide intervals.
      Coverage should be read alongside WIS, since very wide intervals can achieve
      high coverage at the cost of precision.
    </p>
    <div id="eq-trad-cov" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> If 95% coverage is 78.0%, only 78% of observed counts
      fell inside the 95% prediction intervals, meaning the intervals missed roughly
      one week in five when they should have missed only one in twenty &mdash; a sign
      of overconfident intervals. If 50% coverage is 70.0%, the 50% intervals were
      wider than needed.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
') else '', if(!all(metric_available)) paste0('
    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Metrics Not Shown</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      The following metrics could not be computed for these forecasts and are
      omitted from the table: ', omitted_list_text, '. Interval-based scores
      (WIS, its directional components, and interval coverage) require forecasts
      to include prediction-interval quantiles; when forecasts provide only a
      median, these scores cannot be evaluated.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">
') else '',
transmission_filter_block, trend_phase_methods_block, '
  </div>

  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css">
  <script src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js"></script>
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
        r("eq-trad-wis", "\\\\text{WIS} = \\\\frac{1}{K + 1/2}\\\\left( \\\\frac{1}{2}\\\\, \\\\left| y - m \\\\right| + \\\\sum_{k=1}^{K} \\\\frac{\\\\alpha_k}{2}\\\\, \\\\text{IS}_{\\\\alpha_k}(F,\\\\, y) \\\\right)");
        r("eq-trad-mae", "\\\\mathrm{MAE} = \\\\frac{1}{n}\\\\sum_{i=1}^{n} \\\\left| y_i - \\\\hat{y}_i^{(0.5)} \\\\right|");
        r("eq-trad-decomp", "\\\\text{WIS} = \\\\text{Dispersion} + \\\\text{Underprediction} + \\\\text{Overprediction}");
        r("eq-trad-cov", "\\\\mathrm{Cov}_{\\\\alpha} = \\\\frac{1}{n}\\\\sum_{i=1}^{n} \\\\mathbf{1}\\\\!\\\\left[ L_i^{\\\\alpha} \\\\le y_i \\\\le U_i^{\\\\alpha} \\\\right]");
      }
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", go);
      } else { go(); }
    })();
  </script>
  '))

  methods_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Detailed Methods (Traditional Metrics)")),
    htmltools::div(class = "accordion-body", methods_html)
  )

#------------------------------------------------------------------------------#
# Intro --------------------------------------------------------------------- #
#------------------------------------------------------------------------------#

  intro_html <- htmltools::HTML(paste0('
  <p style="font-size: 15px; line-height: 1.8; color: #444; margin: 0 0 1rem 0;">
    Traditional scoring rules for the ', outcome, ' forecasts over the
    transmission-season testing period: ', metric_list_text, '.
    ', omitted_note, '
  </p>'))

#------------------------------------------------------------------------------#
# Assembling the full drop-down ------------------------------------------------
#------------------------------------------------------------------------------#

  traditional_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Traditional Metrics")),
    htmltools::div(
      class = "accordion-body",
      intro_html,
      table_block,
      nested_phase_accordion,
      methods_accordion
    )
  )

  htmltools::tagList(traditional_accordion, trend_phase_accordion)

}
