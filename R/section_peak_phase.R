#' Render the Peak Performance drop-down section
#'
#' Builds the Peak Performance accordion for the testing-period block: an intro
#' (via `build_peak_phase_intro()`), a geography (and, where a location spans
#' more than one season, a season) dropdown, an observed-peak caption, and a
#' per-horizon reference table with a Raw / Relative (%) toggle on the two
#' magnitude columns. Renders nothing when no testing data is present.
#'
#' The table is a single `#peakRefTable`; every `<tr>` carries `data-location`
#' (the display label) and `data-season`, and the dropdowns filter by showing
#' or hiding rows. The Raw / Relative toggle is a CSS show/hide of the
#' `.peak-mag-raw-cell` / `.peak-mag-rel-cell` spans.
#'
#' @param peakPhase.data Output of `calculating_peak_trough_PEAKPHASE()` — one
#'   row per location x season x horizon, with `horizon`, `horizonName`,
#'   `observedPeakDate`, `observedPeakValue`, `predictedPeakTimingOff`,
#'   `predictedPeakMagnitudeOff`, `predictedPeakAccuracy`, `peakWeekMagnitudeOff`,
#'   `peakWeekAccuracy`, `peakWeekForecastExists`, and `horizonReachesPeak`.
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Used for the
#'   testing-data presence gate and as a fallback location label source.
#' @param data.for.evaluation Reserved for the overview plot (not yet wired to
#'   the new schema); currently unused.
#' @param training.data.label Reserved for the overview plot; currently unused.
#' @param outcome Character outcome label. When `NULL`, resolved from
#'   `variables_crosswalk`, then `eval_meta$outcome`, then a generic fallback.
#' @param variables_crosswalk Validated crosswalk data frame, or `NULL`.
#' @param eval_config Evaluation config from `create_evaluation_config()`. When
#'   `NULL`, defaults are used.
#'
#' @return Rendered HTML via `htmltools::tags$details()`, or `invisible(NULL)`
#'   when no testing data is available.
#'
#' @keywords internal
#' @noRd
section_peak_phase <- function(peakPhase.data,
                               eval_meta,
                               data.for.evaluation = NULL,
                               training.data.label = NULL,
                               outcome             = NULL,
                               variables_crosswalk = NULL,
                               eval_config         = NULL) {

#------------------------------------------------------------------------------#
# Guard: testing data must be present ------------------------------------------
#------------------------------------------------------------------------------#

  has_testing <- !is.null(eval_meta) &&
    !is.null(eval_meta$testing_data) &&
    is.data.frame(eval_meta$testing_data) &&
    nrow(eval_meta$testing_data) > 0

  if(!has_testing) return(invisible(NULL))
  if(is.null(peakPhase.data) ||
     !is.data.frame(peakPhase.data) ||
     nrow(peakPhase.data) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Resolving inputs -------------------------------------------------------------
#------------------------------------------------------------------------------#

  if(is.null(eval_config)) eval_config <- create_evaluation_config()

  ##########################################################
  # Outcome label (crosswalk -> eval_meta -> fallback)      #
  ##########################################################
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
          !is.na(clean_names) &
            nchar(trimws(clean_names)) > 0 &
            clean_names != "USER: provide a definition"]
        if(length(clean_names) > 0) outcome <- paste(clean_names, collapse = ", ")
      }
    }
    if(is.na(outcome) || !nzchar(outcome)){
      outcome <- if(!is.null(eval_meta$outcome)) paste(eval_meta$outcome, collapse = ", ") else "Observed"
    }
  }

#------------------------------------------------------------------------------#
# Resolving location codes and display labels ----------------------------------
#------------------------------------------------------------------------------#

  loc_codes <- sort(unique(peakPhase.data$location))

  resolve_label <- function(cd){
    lab <- NA_character_
    if("location_display" %in% names(peakPhase.data)){
      cand <- unique(peakPhase.data$location_display[peakPhase.data$location == cd])
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
  interactive <- n_loc > 1
  has_season  <- "season" %in% names(peakPhase.data)

#------------------------------------------------------------------------------#
# Intro (via build_peak_phase_intro) -------------------------------------------
#------------------------------------------------------------------------------#

  intro_html <- build_peak_phase_intro(multi_location = interactive,
                                       outcome        = outcome)

#------------------------------------------------------------------------------#
# Seasons per location + JS lookups (label-keyed) ------------------------------
#------------------------------------------------------------------------------#
# About: The season dropdown is shown only for a location that spans more than  #
# one season; its options follow the selected location so location + season is  #
# never an empty pair. peakLocSeasons drives the dropdown, peakObs the caption. #
#------------------------------------------------------------------------------#

  seasons_of <- function(cd){
    if(!has_season) return(character(0))
    ss <- peakPhase.data$season[peakPhase.data$location == cd]
    sort(unique(as.character(ss[!is.na(ss)])))
  }

  loc_seasons      <- lapply(loc_codes, seasons_of)
  any_multi_season <- any(vapply(loc_seasons, length, integer(1)) > 1)
  peak_all_seasons <- sort(unique(unlist(loc_seasons)))

  # Escape a value for embedding inside a double-quoted JS string
  js_str <- function(x){
    x <- gsub('\\\\', '\\\\\\\\', x)
    x <- gsub('"', '\\\\"', x)
    paste0('"', x, '"')
  }

  # {display label -> [seasons]} for the season dropdown
  loc_seasons_js <- paste0("{",
    paste(vapply(seq_len(n_loc), function(li){
      vs <- if(length(loc_seasons[[li]]))
        paste(vapply(loc_seasons[[li]], js_str, character(1)), collapse = ",") else ""
      paste0(js_str(loc_labels[li]), ":[", vs, "]")
    }, character(1)), collapse = ","),
    "}")

  # {"label||season" -> observed-peak caption}, plus an Overall entry per
  # multi-season location and a "label||null" entry for season-less data
  obs_entries <- character(0)
  for(li in seq_len(n_loc)){
    cd  <- loc_codes[li]
    lab <- loc_labels[li]
    ss  <- loc_seasons[[li]]

    obs_caption <- function(sub){
      dt  <- as.character(sub$observedPeakDate[1])
      val <- suppressWarnings(as.numeric(sub$observedPeakValue[1]))
      vfm <- if(is.na(val)) "&mdash;" else format(round(val), big.mark = ",", trim = TRUE)
      paste0('*Observed peak: <strong>', dt, '</strong> at <strong>', vfm,
             '</strong> ', outcome, '.')
    }

    if(length(ss) == 0){
      sub <- peakPhase.data[peakPhase.data$location == cd, , drop = FALSE]
      obs_entries <- c(obs_entries,
        paste0(js_str(paste0(lab, "||null")), ":", js_str(obs_caption(sub))))
    }else{
      for(s in ss){
        sub <- peakPhase.data[peakPhase.data$location == cd &
                                as.character(peakPhase.data$season) == s, , drop = FALSE]
        obs_entries <- c(obs_entries,
          paste0(js_str(paste0(lab, "||", s)), ":", js_str(obs_caption(sub))))
      }
      if(length(ss) > 1){
        cap_o <- paste0('Averaged across ', length(ss), ' seasons (',
                        paste(ss, collapse = ", "), ').')
        obs_entries <- c(obs_entries,
          paste0(js_str(paste0(lab, "||Overall")), ":", js_str(cap_o)))
      }
    }
  }
  peak_obs_js <- paste0("{", paste(obs_entries, collapse = ","), "}")

#------------------------------------------------------------------------------#
# Geography + season dropdowns (rendered outside the table) --------------------
#------------------------------------------------------------------------------#

  dropdown_html <- if(interactive || any_multi_season){
    loc_dd    <- if(interactive)
      build_geo_dropdown(loc_labels, select_id = "geoSelect_peak") else NULL
    season_dd <- if(any_multi_season){
      htmltools::div(
        id    = "peakSeasonWrap",
        style = "display:none;",
        build_geo_dropdown(peak_all_seasons, select_id = "seasonSelect_peak")
      )
    }else{
      NULL
    }
    htmltools::tagList(
      htmltools::div(
        style = paste0("display:flex; justify-content:center; align-items:center; ",
                       "gap:1.5em; flex-wrap:wrap;"),
        loc_dd, season_dd
      ),
      htmltools::div(style = "margin-top: 0.5em;")
    )
  }else{
    NULL
  }

#------------------------------------------------------------------------------#
# Cell formatting helpers ------------------------------------------------------
#------------------------------------------------------------------------------#

  smean <- function(x){ x <- x[!is.na(x)]; if(!length(x)) NA_real_ else mean(x) }
  smin  <- function(x){ x <- x[!is.na(x)]; if(!length(x)) NA_real_ else min(x) }
  smax  <- function(x){ x <- x[!is.na(x)]; if(!length(x)) NA_real_ else max(x) }

  # Warm when the forecast ran high, cool when low, neutral at zero / missing
  sign_color <- function(v){
    if(is.na(v)) return("#222")
    if(v > 0)    return("#B85C30")
    if(v < 0)    return("#2D6A9F")
    "#222"
  }

  main_open  <- function(color = "#222") paste0(
    '<div style="font-size:15px;font-weight:600;color:', color, ';white-space:nowrap;">')
  range_open <- paste0('<div style="font-size:12px;font-weight:400;color:#555;',
                       'margin-top:4px;white-space:nowrap;">')

  # Signed integer with a leading + / - and thousands separators
  fmt_signed_int <- function(v){
    vi <- round(v)
    s  <- if(vi > 0) "+" else if(vi < 0) "-" else ""
    paste0(s, format(abs(vi), big.mark = ",", trim = TRUE))
  }
  fmt_signed <- function(v) if(is.na(v)) "&mdash;" else paste0(if(v > 0) "+" else "", round(v, 1))
  fmt_int    <- function(v) format(round(v), big.mark = ",", trim = TRUE)

  # Stacked value-over-range for a signed count (magnitude / peak-week raw track)
  stacked_signed <- function(m, lo, hi, suffix = ""){
    if(is.na(m)) return(paste0(main_open(), '&mdash;', suffix, '</div>'))
    rng <- if(!is.na(lo) && !is.na(hi) && round(lo) != round(hi)) paste0(
      range_open, '(range: ', fmt_signed_int(lo), ' to ', fmt_signed_int(hi), ')</div>') else ''
    paste0(main_open(sign_color(m)), fmt_signed_int(m), suffix, '</div>', rng)
  }

  # Stacked value-over-range for a percent-accuracy score (relative track)
  stacked_pct <- function(m, lo, hi, suffix = ""){
    if(is.na(m)) return(paste0(main_open(), '&mdash;', suffix, '</div>'))
    rng <- if(!is.na(lo) && !is.na(hi) && round(lo) != round(hi)) paste0(
      range_open, '(range: ', round(lo), '% to ', round(hi), '%)</div>') else ''
    paste0(main_open(), round(m), '%', suffix, '</div>', rng)
  }

  # Timing value ("On Target" / "N weeks early|late") coloured (early blue, late
  # dark red, on target neutral); the sub-line is supplied by the caller
  timing_stack <- function(m, sub = ""){
    if(is.na(m)) return(paste0(main_open(), '&mdash;</div>', sub))
    m1 <- round(m, 1)
    if(m1 == 0){
      col <- "#222"
      val <- '<strong>On Target</strong> <span style="font-weight:400;">(0 weeks)</span>'
    }else{
      col  <- if(m1 < 0) "#2D6A9F" else "#A32C2C"
      dirw <- if(m1 < 0) 'early' else 'late'
      a    <- abs(m1)
      unit <- if(a == 1) 'week' else 'weeks'
      val  <- paste0('<strong>', a, '</strong> <span style="font-weight:400;">',
                     unit, '</span> <strong>', dirw, '</strong>')
    }
    paste0(main_open(col), val, '</div>', sub)
  }

#------------------------------------------------------------------------------#
# Building the table rows ------------------------------------------------------
#------------------------------------------------------------------------------#
# About: One <tr> per horizon within each (location, season) block, ordered     #
# longest lead first. Multi-season locations also get an "Overall" block that   #
# averages each horizon across seasons with a min-max range. Rows carry         #
# data-location (label) and data-season so the dropdowns can filter them.       #
#------------------------------------------------------------------------------#

  dagger  <- ' <sup style="color:#B85C30;">&dagger;</sup>'
  ddagger <- ' <sup style="color:#7A7A7A;">&Dagger;</sup>'

  any_censor     <- FALSE
  any_pw_missing <- FALSE

  render_block <- function(d_block, label, season_key){
    hs  <- sort(unique(d_block$horizon), decreasing = TRUE)
    n_h <- length(hs)
    if(n_h == 0) return("")

    rows <- vapply(seq_len(n_h), function(k){
      d       <- d_block[d_block$horizon == hs[k], , drop = FALSE]
      is_last <- k == n_h
      bb      <- if(is_last) "" else "border-bottom:1px solid #e0e0e0;"

      reaches <- all(d$horizonReachesPeak %in% TRUE)
      pw_any  <- any(d$peakWeekForecastExists %in% TRUE)
      pw_all  <- all(d$peakWeekForecastExists %in% TRUE)
      if(!reaches) any_censor     <<- TRUE
      if(!pw_all)  any_pw_missing <<- TRUE

      ###############################
      # Horizon (with censor mark)  #
      ###############################
      hz <- paste0('<td style="padding:14px 16px;text-align:center;vertical-align:middle;',
        'border-right:1px solid #e0e0e0;', bb, '">',
        '<div style="font-size:13px;font-weight:600;color:#777;white-space:nowrap;">',
        d$horizonName[1], if(!reaches) dagger else '', '</div></td>')

      ###############################
      # Peak Timing (single track)  #
      ###############################
      # Beneath the offset, show the two peak weeks that produce it (predicted
      # vs observed) for a single season, or the week range for an Overall row
      if(nrow(d) == 1){
        pd <- as.character(d$predictedPeakDate[1])
        od <- as.character(d$observedPeakDate[1])
        t_sub <- if(!is.na(pd) && !is.na(od) && nzchar(pd) && nzchar(od))
          paste0(range_open, '(', pd, ' vs ', od, ')</div>') else ''
      }else{
        tlo <- smin(d$predictedPeakTimingOff); thi <- smax(d$predictedPeakTimingOff)
        t_sub <- if(!is.na(tlo) && !is.na(thi) && round(tlo, 1) != round(thi, 1))
          paste0(range_open, '(range: ', fmt_signed(tlo), ' to ', fmt_signed(thi),
                 ' wk)</div>') else ''
      }
      tm <- paste0('<td style="padding:14px 16px;text-align:center;vertical-align:middle;',
        'border-right:1px solid #e0e0e0;', bb, '">',
        timing_stack(smean(d$predictedPeakTimingOff), t_sub), '</td>')

      ###############################
      # Peak Magnitude (raw / rel)  #
      ###############################
      # Raw track is neutral (no over/under colour); beneath a single-season
      # value we show the (forecast - observed) counts, and beneath an Overall
      # value the min-max range of the difference across seasons
      mag_off <- d$predictedPeakMagnitudeOff
      if(nrow(d) == 1){
        if(is.na(mag_off[1])){
          mag_raw <- paste0(main_open(), '&mdash;</div>')
        }else{
          obs_v <- suppressWarnings(as.numeric(d$observedPeakValue[1]))
          sub   <- if(!is.na(obs_v)) paste0(range_open, '(', fmt_int(obs_v + mag_off[1]),
                     ' &minus; ', fmt_int(obs_v), ')</div>') else ''
          mag_raw <- paste0(main_open(), fmt_signed_int(mag_off[1]), '</div>', sub)
        }
      }else{
        mm <- smean(mag_off); ml <- smin(mag_off); mh <- smax(mag_off)
        if(is.na(mm)){
          mag_raw <- paste0(main_open(), '&mdash;</div>')
        }else{
          rng <- if(!is.na(ml) && !is.na(mh) && round(ml) != round(mh)) paste0(
            range_open, '(range: ', fmt_signed_int(ml), ' to ', fmt_signed_int(mh), ')</div>') else ''
          mag_raw <- paste0(main_open(), fmt_signed_int(mm), '</div>', rng)
        }
      }
      mag_rel <- stacked_pct(smean(d$predictedPeakAccuracy) * 100,
                             smin(d$predictedPeakAccuracy) * 100,
                             smax(d$predictedPeakAccuracy) * 100)
      mg <- paste0('<td style="padding:14px 16px;text-align:center;vertical-align:middle;',
        'border-right:1px solid #e0e0e0;', bb, '">',
        '<div class="peak-mag-raw-cell">', mag_raw, '</div>',
        '<div class="peak-mag-rel-cell" style="display:none;">', mag_rel, '</div></td>')

      ###############################
      # Same-Day Peak Mag (raw/rel) #
      ###############################
      # Predicted peak height vs the value observed on the predicted-peak day;
      # neutral raw value with (predicted - same-day observed) beneath a single
      # season, or the min-max range across seasons for an Overall row
      sd_off <- if("sameDayMagnitudeOff" %in% names(d)) d$sameDayMagnitudeOff else rep(NA_real_, nrow(d))
      sd_acc <- if("sameDayAccuracy"    %in% names(d)) d$sameDayAccuracy    else rep(NA_real_, nrow(d))
      if(nrow(d) == 1){
        if(is.na(sd_off[1])){
          sd_raw <- paste0(main_open(), '&mdash;</div>')
        }else{
          fc_v <- if("predictedPeakValue"      %in% names(d)) suppressWarnings(as.numeric(d$predictedPeakValue[1]))      else NA_real_
          ob_v <- if("observedAtPredictedPeak" %in% names(d)) suppressWarnings(as.numeric(d$observedAtPredictedPeak[1])) else NA_real_
          sub  <- if(!is.na(fc_v) && !is.na(ob_v)) paste0(range_open, '(', fmt_int(fc_v),
                    ' &minus; ', fmt_int(ob_v), ')</div>') else ''
          sd_raw <- paste0(main_open(), fmt_signed_int(sd_off[1]), '</div>', sub)
        }
      }else{
        sm <- smean(sd_off); sl <- smin(sd_off); sh <- smax(sd_off)
        if(is.na(sm)){
          sd_raw <- paste0(main_open(), '&mdash;</div>')
        }else{
          rng <- if(!is.na(sl) && !is.na(sh) && round(sl) != round(sh)) paste0(
            range_open, '(range: ', fmt_signed_int(sl), ' to ', fmt_signed_int(sh), ')</div>') else ''
          sd_raw <- paste0(main_open(), fmt_signed_int(sm), '</div>', rng)
        }
      }
      sd_rel <- stacked_pct(smean(sd_acc) * 100,
                            smin(sd_acc) * 100,
                            smax(sd_acc) * 100)
      sd <- paste0('<td style="padding:14px 16px;text-align:center;vertical-align:middle;',
        'border-right:1px solid #e0e0e0;', bb, '">',
        '<div class="peak-mag-raw-cell">', sd_raw, '</div>',
        '<div class="peak-mag-rel-cell" style="display:none;">', sd_rel, '</div></td>')

      ###############################
      # Peak-Week (raw / rel)       #
      ###############################
      if(!pw_any){
        pw_raw <- paste0(main_open(), '&mdash;', ddagger, '</div>')
        pw_rel <- pw_raw
      }else{
        sfx    <- if(!pw_all) ddagger else ''
        keep   <- d$peakWeekForecastExists %in% TRUE
        if(nrow(d) == 1){
          fc_v <- suppressWarnings(as.numeric(d$peakWeekForecastValue[1]))
          ob_v <- suppressWarnings(as.numeric(d$observedPeakValue[1]))
          sub  <- if(!is.na(fc_v) && !is.na(ob_v)) paste0(range_open, '(', fmt_int(fc_v),
                    ' &minus; ', fmt_int(ob_v), ')</div>') else ''
          pw_raw <- paste0(main_open(), fmt_signed_int(d$peakWeekMagnitudeOff[1]), sfx, '</div>', sub)
        }else{
          m <- smean(d$peakWeekMagnitudeOff[keep])
          lo <- smin(d$peakWeekMagnitudeOff[keep]); hi <- smax(d$peakWeekMagnitudeOff[keep])
          rng <- if(!is.na(lo) && !is.na(hi) && round(lo) != round(hi)) paste0(
            range_open, '(range: ', fmt_signed_int(lo), ' to ', fmt_signed_int(hi), ')</div>') else ''
          pw_raw <- paste0(main_open(), fmt_signed_int(m), sfx, '</div>', rng)
        }
        pw_rel <- stacked_pct(smean(d$peakWeekAccuracy[keep]) * 100,
                              smin(d$peakWeekAccuracy[keep]) * 100,
                              smax(d$peakWeekAccuracy[keep]) * 100, sfx)
      }
      pw <- paste0('<td style="padding:14px 16px;text-align:center;vertical-align:middle;', bb, '">',
        '<div class="peak-mag-raw-cell">', pw_raw, '</div>',
        '<div class="peak-mag-rel-cell" style="display:none;">', pw_rel, '</div></td>')

      paste0('<tr data-location="', label, '" data-season="', season_key, '">',
             hz, tm, mg, sd, pw, '</tr>')
    }, character(1))

    paste0(rows, collapse = "")
  }

  all_rows <- paste0(vapply(seq_len(n_loc), function(li){
    cd  <- loc_codes[li]
    lab <- loc_labels[li]
    ld  <- peakPhase.data[peakPhase.data$location == cd, , drop = FALSE]
    if(nrow(ld) == 0) return("")

    ss <- loc_seasons[[li]]
    if(length(ss) == 0) return(render_block(ld, lab, "null"))

    out <- ""
    for(s in ss){
      out <- paste0(out, render_block(
        ld[as.character(ld$season) == s, , drop = FALSE], lab, s))
    }
    if(length(ss) > 1) out <- paste0(out, render_block(ld, lab, "Overall"))
    out
  }, character(1)), collapse = "")

#------------------------------------------------------------------------------#
# Reference-table script (filter + Raw/Relative toggle) ------------------------
#------------------------------------------------------------------------------#

  ref_script <- paste0('<script>
    (function() {
      var peakRefIsMultiLocation = ', if(interactive) "true" else "false", ';
      var peakLocSeasons = ', loc_seasons_js, ';
      var peakObs = ', peak_obs_js, ';
      var peakCurLoc = null, peakCurSeason = null, peakCmpSearch = "";

      // Raw <-> Relative magnitude toggle (CSS show/hide of the two spans),
      // applied to both the per-horizon table and the comparison table
      window.setMagModePeakRef = function(mode) {
        var raw = (mode === "raw");
        document.querySelectorAll("#peakRefTable .peak-mag-raw-cell, #peakCompareTable .peak-mag-raw-cell")
          .forEach(function(c) { c.style.display = raw ? "" : "none"; });
        document.querySelectorAll("#peakRefTable .peak-mag-rel-cell, #peakCompareTable .peak-mag-rel-cell")
          .forEach(function(c) { c.style.display = raw ? "none" : ""; });
        var br = document.getElementById("peak-ref-btn-raw");
        var be = document.getElementById("peak-ref-btn-rel");
        if (br) { br.style.background = raw ? "#522D80" : "transparent"; br.style.color = raw ? "#fff" : "#555"; }
        if (be) { be.style.background = raw ? "transparent" : "#522D80"; be.style.color = raw ? "#555" : "#fff"; }
      };

      // Show/hide rows by location + season, update the observed-peak line, and
      // filter the comparison table by the same season (when the season dropdown
      // is in play) plus its own search box
      window.applyPeakFilters = function() {
        var body = document.querySelector("#peakRefTable tbody");
        if (body) {
          Array.from(body.querySelectorAll("tr")).forEach(function(row) {
            var okLoc = (peakCurLoc === null) || row.getAttribute("data-location") === peakCurLoc;
            var okSea = (peakCurSeason === null) || row.getAttribute("data-season") === peakCurSeason;
            row.style.display = (okLoc && okSea) ? "" : "none";
          });
        }
        var cap = document.getElementById("peakObsCaption");
        if (cap) { cap.innerHTML = peakObs[peakCurLoc + "||" + peakCurSeason] || ""; }

        var cmpBody = document.querySelector("#peakCompareTable tbody");
        if (cmpBody) {
          var wrap = document.getElementById("peakSeasonWrap");
          var seasonActive = !!(wrap && wrap.style.display !== "none");
          Array.from(cmpBody.querySelectorAll("tr")).forEach(function(row) {
            var rs = row.getAttribute("data-season");
            var okSea = seasonActive ? (rs === peakCurSeason) : (rs !== "Overall");
            var locTd = row.querySelector("td");
            var okSrch = (peakCmpSearch === "") ||
              (locTd && locTd.textContent.toLowerCase().indexOf(peakCmpSearch) !== -1);
            row.style.display = (okSea && okSrch) ? "" : "none";
          });
        }

        if (typeof window.peakGraphRedraw === "function") {
          window.peakGraphRedraw(peakCurLoc, peakCurSeason);
        }
      };

      // Comparison-table location search
      window.setPeakCompareSearch = function(term) {
        peakCmpSearch = (term || "").toLowerCase();
        window.applyPeakFilters();
      };

      // Repopulate the season dropdown for the selected location (+ Overall when
      // it spans more than one season); hide it entirely for a single season
      window.refreshSeasonDropdown = function(locName) {
        var wrap    = document.getElementById("peakSeasonWrap");
        var ssel    = document.getElementById("seasonSelect_peak");
        var seasons = peakLocSeasons[locName] || [];
        var multi   = seasons.length > 1;
        if (ssel) {
          ssel.innerHTML = "";
          var opts = seasons.slice();
          if (multi) opts.push("Overall");
          opts.forEach(function(s) {
            var o = document.createElement("option");
            o.value = s;
            o.setAttribute("data-location-name", s);
            o.textContent = s;
            ssel.appendChild(o);
          });
        }
        if (multi) {
          if (wrap) wrap.style.display = "";
          peakCurSeason = "Overall";
          if (ssel) ssel.value = "Overall";
        } else {
          if (wrap) wrap.style.display = "none";
          peakCurSeason = seasons.length ? seasons[0] : null;
        }
      };

      function peakOptVal(sel) {
        var o = sel.options[sel.selectedIndex];
        return o ? (o.getAttribute("data-location-name") || o.value) : null;
      }

      function initPeakRef() {
        var sel = document.getElementById("geoSelect_peak");
        if (sel) {
          sel.addEventListener("change", function() {
            var opt = this.options[this.selectedIndex];
            var lab = opt ? opt.getAttribute("data-location-name") : null;
            if (peakRefIsMultiLocation) peakCurLoc = lab;
            window.refreshSeasonDropdown(lab);
            window.applyPeakFilters();
          });
        }
        var ssel = document.getElementById("seasonSelect_peak");
        if (ssel) {
          ssel.addEventListener("change", function() {
            peakCurSeason = peakOptVal(this);
            window.applyPeakFilters();
          });
        }
        var fr = document.querySelector("#peakRefTable tbody tr[data-location]");
        var initLoc = fr ? fr.getAttribute("data-location") : null;
        if (peakRefIsMultiLocation) peakCurLoc = initLoc;
        window.refreshSeasonDropdown(initLoc);
        window.applyPeakFilters();
        window.setMagModePeakRef("raw");
      }

      if (document.readyState !== "loading") { initPeakRef(); }
      else { document.addEventListener("DOMContentLoaded", initPeakRef); }
    })();
  </script>')

#------------------------------------------------------------------------------#
# Rendering the reference table ------------------------------------------------
#------------------------------------------------------------------------------#

  table_html <- htmltools::HTML(paste0('
<div style="font-family:sans-serif;padding:1rem 0;overflow-x:auto;">
', ref_script, '

  <!-- Observed-peak note (left) + Raw / Relative toggle (right), flush to table -->
  <div style="display:flex;justify-content:space-between;align-items:flex-end;gap:1em;margin-bottom:10px;">
    <div id="peakObsCaption" style="text-align:left;font-style:italic;font-size:14px;color:#999;line-height:1.3;flex:1 1 auto;"></div>
    <div style="display:inline-flex;border:1px solid #ddd;border-radius:6px;overflow:hidden;flex:0 0 auto;">
      <button id="peak-ref-btn-raw" type="button" onclick="setMagModePeakRef(\'raw\')"
              style="padding:6px 14px;font-size:12px;font-weight:600;border:none;cursor:pointer;background:#522D80;color:#fff;">
        Raw
      </button>
      <button id="peak-ref-btn-rel" type="button" onclick="setMagModePeakRef(\'rel\')"
              style="padding:6px 14px;font-size:12px;font-weight:600;border:none;cursor:pointer;background:transparent;color:#555;">
        Relative (%)
      </button>
    </div>
  </div>

  <table id="peakRefTable" style="width:100%;border-collapse:collapse;
                                   border-top:1px solid #333;border-bottom:1px solid #333;">
    <thead>
      <tr style="border-bottom:1px solid #333;">
        <th class="sum-th" style="border-right:1px solid #e0e0e0;text-align:center;vertical-align:middle;">Horizon</th>
        <th class="sum-th" style="border-right:1px solid #e0e0e0;">
          <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">Peak Timing</div>
          <div class="sum-th-sub" style="color:#9B85C8;font-size:13px;">(Predicted vs Observed Week)</div>
        </th>
        <th class="sum-th" style="border-right:1px solid #e0e0e0;">
          <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">Peak-to-Peak Magnitude</div>
          <div class="sum-th-sub" style="color:#9B85C8;font-size:13px;">
            <span class="peak-mag-raw-cell">(Predicted Peak &minus; Observed Peak)</span>
            <span class="peak-mag-rel-cell" style="display:none;">(% Accuracy)</span>
          </div>
        </th>
        <th class="sum-th" style="border-right:1px solid #e0e0e0;">
          <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">Magnitude at Predicted Peak</div>
          <div class="sum-th-sub" style="color:#9B85C8;font-size:13px;">
            <span class="peak-mag-raw-cell">(Predicted Peak &minus; Observed That Week)</span>
            <span class="peak-mag-rel-cell" style="display:none;">(% Accuracy)</span>
          </div>
        </th>
        <th class="sum-th" style="border-right:none;">
          <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">Magnitude at Observed Peak</div>
          <div class="sum-th-sub" style="color:#9B85C8;font-size:13px;">
            <span class="peak-mag-raw-cell">(Forecast That Week &minus; Observed Peak)</span>
            <span class="peak-mag-rel-cell" style="display:none;">(% Accuracy)</span>
          </div>
        </th>
      </tr>
    </thead>
    <tbody>', all_rows, '</tbody>
  </table>
</div>
'))

#------------------------------------------------------------------------------#
# Table footnote ---------------------------------------------------------------
#------------------------------------------------------------------------------#

  fn_marks <- ""
  if(any_censor) fn_marks <- paste0(fn_marks,
    '<br/><strong>&dagger;</strong> The forecast horizon did not extend to the ',
    'observed peak week, so these values reflect a truncated view.')
  if(any_pw_missing) fn_marks <- paste0(fn_marks,
    '<br/><strong>&Dagger;</strong> No forecast was available for the observed ',
    'peak week at this horizon.')

  table_note_html <- htmltools::HTML(paste0('
<div style="font-family:sans-serif;padding:0.5rem 0;margin-top:-0.25rem;">
  <p style="font-size:12px;color:#888;line-height:1.6;margin:0;font-style:italic;">
    <strong>Peak Timing</strong> compares the week the forecast placed the peak with the true peak week
    &mdash; negative is early, positive is late. The three magnitude columns all compare heights, differing
    only in which week anchors the comparison: <strong>Peak-to-Peak Magnitude</strong> is the predicted peak
    height against the observed peak height (dates aside); <strong>Magnitude at Predicted Peak</strong> is
    that predicted peak against what was actually observed that same week; and <strong>Magnitude at Observed
    Peak</strong> is the forecast for the true peak week against the observed peak. Use the
    <strong>Raw / Relative (%)</strong> toggle to switch the magnitude columns between the count
    difference and a percent-accuracy score. For a single season each row shows one value; for Overall
    each row shows the mean with its min&ndash;max range beneath.', fn_marks, '
  </p>
</div>
'))

#------------------------------------------------------------------------------#
# Location-to-Location Comparison accordion (multi-location only) --------------#
#------------------------------------------------------------------------------#
# About: One row per (location, season) [plus an Overall row per multi-season   #
# location], each measure the mean across horizons with its min-max range.      #
# Sortable + searchable; follows the season dropdown and the Raw/Relative        #
# toggle; default order is a composite of all four measures, strongest first.   #
#------------------------------------------------------------------------------#

  compare_accordion <- if(interactive){

    # Neutral signed value with a min-max range beneath (magnitude raw track);
    # the "range" is flagged in the subheader, so cells just show "(X to Y)"
    cmp_signed <- function(v){
      m <- smean(v); lo <- smin(v); hi <- smax(v)
      if(is.na(m)) return(paste0(main_open(), '&mdash;</div>'))
      rng <- if(!is.na(lo) && !is.na(hi) && round(lo) != round(hi)) paste0(
        range_open, '(', fmt_signed_int(lo), ' to ', fmt_signed_int(hi), ')</div>') else ''
      paste0(main_open(), fmt_signed_int(m), '</div>', rng)
    }
    # Percent-accuracy value with range (magnitude relative track); v is 0-1
    cmp_pct <- function(v){
      m <- smean(v) * 100; lo <- smin(v) * 100; hi <- smax(v) * 100
      if(is.na(m)) return(paste0(main_open(), '&mdash;</div>'))
      rng <- if(!is.na(lo) && !is.na(hi) && round(lo) != round(hi)) paste0(
        range_open, '(', round(lo), '% to ', round(hi), '%)</div>') else ''
      paste0(main_open(), round(m), '%</div>', rng)
    }
    # Timing headline is MEAN ABSOLUTE weeks off (so early/late cannot cancel);
    # the range beneath is SIGNED (- early / + late) so the direction of the
    # misses is still visible
    cmp_timing <- function(v){
      vv <- v[!is.na(v)]
      if(!length(vv)) return(paste0(main_open(), '&mdash;</div>'))
      m  <- mean(abs(vv)); lo <- min(vv); hi <- max(vv)
      main <- if(round(m, 1) == 0) '<strong>On Target</strong>' else
        paste0('<strong>', round(m, 1), '</strong> <span style="font-weight:400;">wk off</span>')
      rng <- if(round(lo, 1) != round(hi, 1)){
        paste0(range_open, '(', fmt_signed(lo), ' to ', fmt_signed(hi), ' wk)</div>')
      }else if(round(lo, 1) != 0){
        paste0(range_open, '(', fmt_signed(lo), ' wk)</div>')
      }else ''
      paste0(main_open(), main, '</div>', rng)
    }

    cmp_row <- function(df, label, season_key){
      pwk   <- df$peakWeekForecastExists %in% TRUE
      tim_v <- df$predictedPeakTimingOff
      p2p_a <- df$predictedPeakAccuracy
      sd_a  <- df$sameDayAccuracy
      ow_a  <- df$peakWeekAccuracy[pwk]

      # data-value for sorting: timing = mean ABSOLUTE weeks off (smaller=better);
      # magnitude columns = mean % accuracy (higher=better)
      dvpct <- function(x){ m <- smean(x); if(is.na(m)) "" else round(m * 100) }
      tim_a  <- abs(tim_v[!is.na(tim_v)])
      tim_dv <- if(!length(tim_a)) "" else round(mean(tim_a), 1)

      # Composite goodness (higher = better): timing closeness (mean |off|, so
      # early/late cannot cancel) + the three percent accuracies
      tim_s <- if(!length(tim_a)) NA_real_ else 1 / (1 + mean(tim_a))
      comp  <- suppressWarnings(mean(c(tim_s, smean(p2p_a), smean(sd_a), smean(ow_a)), na.rm = TRUE))
      comp  <- if(is.nan(comp)) "" else round(comp, 4)

      mag_td <- function(raw, rel, dv, last = FALSE){
        paste0('<td data-value="', dv, '" style="padding:14px 16px;text-align:center;vertical-align:middle;',
          if(last) '' else 'border-right:1px solid #e0e0e0;', '">',
          '<div class="peak-mag-raw-cell">', raw, '</div>',
          '<div class="peak-mag-rel-cell" style="display:none;">', rel, '</div></td>')
      }

      paste0(
        '<tr data-location="', label, '" data-season="', season_key, '" data-composite="', comp,
          '" style="border-bottom:1px solid #e0e0e0;">',
        '<td data-location="', label, '" style="padding:14px 16px;font-size:14px;font-weight:700;color:#555;',
          'text-align:center;vertical-align:middle;width:130px;border-right:1px solid #e0e0e0;white-space:nowrap;">',
          label, '</td>',
        '<td data-value="', tim_dv, '" style="padding:14px 16px;text-align:center;vertical-align:middle;',
          'border-right:1px solid #e0e0e0;">', cmp_timing(tim_v), '</td>',
        mag_td(cmp_signed(df$predictedPeakMagnitudeOff), cmp_pct(p2p_a), dvpct(p2p_a)),
        mag_td(cmp_signed(df$sameDayMagnitudeOff),       cmp_pct(sd_a),  dvpct(sd_a)),
        mag_td(cmp_signed(df$peakWeekMagnitudeOff[pwk]), cmp_pct(ow_a),  dvpct(ow_a), last = TRUE),
        '</tr>')
    }

    cmp_rows <- paste0(vapply(seq_len(n_loc), function(li){
      cd <- loc_codes[li]; lab <- loc_labels[li]
      ld <- peakPhase.data[peakPhase.data$location == cd, , drop = FALSE]
      if(nrow(ld) == 0) return("")
      ss <- loc_seasons[[li]]
      if(length(ss) == 0) return(cmp_row(ld, lab, "null"))
      out <- ""
      for(s in ss) out <- paste0(out, cmp_row(ld[as.character(ld$season) == s, , drop = FALSE], lab, s))
      if(length(ss) > 1) out <- paste0(out, cmp_row(ld, lab, "Overall"))
      out
    }, character(1)), collapse = "")

    compare_script <- '<script>
      (function() {
        var pkCmpState = { col: -1, dir: "" };
        function cellVal(td) { var v = parseFloat(td.getAttribute("data-value")); return isNaN(v) ? null : v; }
        function reorder(rows, tbody) {
          rows.forEach(function(r, i) {
            r.style.borderBottom = i < rows.length - 1 ? "1px solid #e0e0e0" : "";
            tbody.appendChild(r);
          });
        }
        function applyPeakCompositeSort() {
          var tbody = document.querySelector("#peakCompareTable tbody");
          if (!tbody) return;
          var rows = Array.from(tbody.querySelectorAll("tr"));
          rows.sort(function(a, b) {
            var av = parseFloat(a.getAttribute("data-composite")), bv = parseFloat(b.getAttribute("data-composite"));
            var aa = isNaN(av) ? null : av, bb = isNaN(bv) ? null : bv;
            if (aa === null && bb === null) return 0;
            if (aa === null) return 1;
            if (bb === null) return -1;
            return bb - aa;
          });
          reorder(rows, tbody);
          pkCmpState = { col: -1, dir: "" };
        }
        window.sortPeakCompare = function(col, type) {
          var tbody = document.querySelector("#peakCompareTable tbody");
          if (!tbody) return;
          var rows = Array.from(tbody.querySelectorAll("tr"));
          var dir;
          if (pkCmpState.col === col) { dir = pkCmpState.dir === "asc" ? "desc" : "asc"; }
          else { dir = (type === "numabs" || type === "text") ? "asc" : "desc"; }
          rows.sort(function(a, b) {
            var aC = a.querySelectorAll("td")[col], bC = b.querySelectorAll("td")[col];
            if (!aC || !bC) return 0;
            if (type === "text") {
              var at = aC.textContent.trim().toLowerCase(), bt = bC.textContent.trim().toLowerCase();
              return dir === "asc" ? at.localeCompare(bt) : bt.localeCompare(at);
            }
            var av = cellVal(aC), bv = cellVal(bC);
            if (type === "numabs") { av = (av === null) ? null : Math.abs(av); bv = (bv === null) ? null : Math.abs(bv); }
            if (av === null && bv === null) return 0;
            if (av === null) return 1;
            if (bv === null) return -1;
            return dir === "asc" ? av - bv : bv - av;
          });
          reorder(rows, tbody);
          pkCmpState = { col: col, dir: dir };
        };
        if (document.readyState !== "loading") { applyPeakCompositeSort(); }
        else { document.addEventListener("DOMContentLoaded", applyPeakCompositeSort); }
      })();
    </script>'

    arrow  <- ' <span style="font-size:10px;">&#8597;</span>'
    sub_st <- 'class="sum-th-sub" style="color:#9B85C8;font-size:13px;"'

    compare_body <- htmltools::HTML(paste0('
    <p style="font-size:14px;line-height:1.6;color:#444;margin:0 0 1rem 0;">
      Compare all locations at once, each measure averaged across every forecast horizon (with its
      min&ndash;max range beneath). Timing is summarised as mean <em>absolute</em> weeks off (its signed range beneath shows the
      direction &mdash; minus early, plus late), and the ranking is a composite of accuracy across all four
      measures, so early/late and over/under misses cannot cancel out. Strongest performer first; click any column to re-sort, or search for a location. Raw shows
      the signed count difference (its range reveals any cancellation), Relative shows percent accuracy; the
      table follows the season selected above and the Raw / Relative (%) toggle.
    </p>

    <div style="font-family:sans-serif;padding:0.5rem 0;overflow-x:auto;">',
      compare_script, '
      <table id="peakCompareTable" style="width:100%;border-collapse:collapse;
                                          border-top:1px solid #333;border-bottom:1px solid #333;">
        <thead>
          <tr style="border-bottom:1px solid #333;">
            <th class="sum-th" onclick="sortPeakCompare(0, \'text\')" style="border-right:1px solid #e0e0e0;width:130px;">
              Location<br/>
              <input type="text" id="peakCompareSearch" placeholder="Search..." onclick="event.stopPropagation();"
                onkeyup="window.setPeakCompareSearch(this.value);"
                style="margin:6px auto;padding:4px 8px;font-size:11px;font-weight:400;border:1px solid #ddd;
                       border-radius:4px;width:90%;color:#333;text-transform:none;letter-spacing:0;display:block;"/>
            </th>
            <th class="sum-th" onclick="sortPeakCompare(1, \'numabs\')" style="border-right:1px solid #e0e0e0;">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">Peak Timing', arrow, '</div>
              <div ', sub_st, '>Mean Abs. Weeks Off (Range)</div>
            </th>
            <th class="sum-th" onclick="sortPeakCompare(2, \'num\')" style="border-right:1px solid #e0e0e0;">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">Peak-to-Peak Magnitude', arrow, '</div>
              <div ', sub_st, '>
                <span class="peak-mag-raw-cell">Mean (Range)</span>
                <span class="peak-mag-rel-cell" style="display:none;">Avg. % Accuracy (Range)</span></div>
            </th>
            <th class="sum-th" onclick="sortPeakCompare(3, \'num\')" style="border-right:1px solid #e0e0e0;">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">Magnitude at Predicted Peak', arrow, '</div>
              <div ', sub_st, '>
                <span class="peak-mag-raw-cell">Mean (Range)</span>
                <span class="peak-mag-rel-cell" style="display:none;">Avg. % Accuracy (Range)</span></div>
            </th>
            <th class="sum-th" onclick="sortPeakCompare(4, \'num\')" style="border-right:none;">
              <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">Magnitude at Observed Peak', arrow, '</div>
              <div ', sub_st, '>
                <span class="peak-mag-raw-cell">Mean (Range)</span>
                <span class="peak-mag-rel-cell" style="display:none;">Avg. % Accuracy (Range)</span></div>
            </th>
          </tr>
        </thead>
        <tbody>', cmp_rows, '</tbody>
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
# Peak-forecast plot carousel --------------------------------------------------
#------------------------------------------------------------------------------#
# About: Three linked plots (timing + peak-to-peak, magnitude at predicted      #
# peak, magnitude at observed peak) the viewer pages through. Driven from the    #
# same dropdowns via window.peakGraphRedraw(). Omitted if the evaluation frame  #
# or the plot builder is unavailable.                                           #
#------------------------------------------------------------------------------#

  peak_plot_available <- !is.null(data.for.evaluation) &&
    is.data.frame(data.for.evaluation) &&
    exists("make_peak_phase_plot")

  plot_block <- if(peak_plot_available){
    htmltools::tags$details(
      class = "accordion",
      htmltools::tags$summary(htmltools::tags$strong("Forecasting the Peak, Visualized")),
      htmltools::div(
        class = "accordion-body",
        htmltools::HTML(paste0('
  <div class="section-intro" style="margin: 0 auto;">
    <div style="background: #f7f4fc; border-left: 4px solid #522D80; border-radius: 4px;
                padding: 10px 15px; margin: 0.25rem 0 2.5rem;">
      <span style="font-size: 13px; font-weight: 700; color: #522D80; display: block;
                   margin-bottom: 4px; text-transform: uppercase; letter-spacing: 0.5px;">
        To Navigate
      </span>
      <p style="font-size: 15px; color: #555; line-height: 1.6; margin: 0;">
        Three linked views of how each forecast horizon captured the peak. Use the
        <strong>arrows or dots</strong> to page between <strong>Peak Timing &amp; Peak-to-Peak
        Magnitude</strong>, <strong>Magnitude at Predicted Peak</strong>, and
        <strong>Magnitude at Observed Peak</strong>. The solid black line is the observed
        outcome; each coloured line is one <strong>forecast horizon</strong> (lighter = longer
        lead). <strong>Hover</strong> a forecast peak to preview its gap, or <strong>click</strong>
        it for the full metric breakdown in a draggable box; use the <strong>legend</strong> to
        toggle any line on or off. The plots follow the geography and season selected above.
      </p>
    </div>
  </div>')),
        make_peak_phase_plot(
          data                = data.for.evaluation,
          loc                 = loc_codes,
          training.data.label = training.data.label,
          outcome             = outcome,
          peakTrough.data     = peakPhase.data)
      )
    )
  }else{
    NULL
  }

#------------------------------------------------------------------------------#
# Detailed Methods accordion (the four peak measures) --------------------------
#------------------------------------------------------------------------------#
# About: A reference for the four peak measures. Formulas use readable word     #
# labels (KaTeX), matching the Percent Agreement section, so any reader can see  #
# exactly what is compared.                                                     #
#------------------------------------------------------------------------------#

  methods_html <- htmltools::HTML(paste0('
  <div style="font-family: sans-serif; padding: 0.5rem 0;">

    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      Every measure below compares the forecast with what was actually observed. For each
      location and season there is one observed peak, and each forecast horizon has its own
      predicted peak. Timing is measured in weeks; the three magnitude measures are reported
      both as a raw count difference and as a percent accuracy.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">The peaks being compared</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      The <strong>observed peak</strong> is the single highest observed value in the season,
      and the week it happened. The <strong>predicted peak</strong> is the highest forecast
      value a horizon produced across the season, and the week it was forecast for. Two more
      values are read straight off the curves: the observed value on the predicted-peak week,
      and the forecast value on the observed-peak week.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Peak Timing</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      How far off the timing was &mdash; the gap in weeks between when the forecast placed the
      peak and when it actually happened. A negative number means early, a positive number means
      late, and a value near zero is reported as On Target.
    </p>
    <div id="eq-timing" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> a predicted peak on Jan 25 against an observed peak on Feb 01
      gives a timing of &minus;1 week (one week early).
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Peak-to-Peak Magnitude</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      How close the two peak heights were, ignoring timing. A positive number means the forecast
      overshot the true peak.
    </p>
    <div id="eq-p2p" style="text-align: center; margin: 0.75rem 0 1.5rem;"></div>

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Magnitude at Predicted Peak</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      How the predicted peak compared with what was actually observed in that same week &mdash;
      the week the forecast placed its peak.
    </p>
    <div id="eq-samewk" style="text-align: center; margin: 0.75rem 0 1.5rem;"></div>

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Magnitude at Observed Peak</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      What the forecast said for the true peak week, compared with the actual peak. This is
      defined only when a forecast targeted that week.
    </p>
    <div id="eq-peakwk" style="text-align: center; margin: 0.75rem 0 1.5rem;"></div>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Percent Accuracy</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1rem;">
      Each magnitude is also reported as a percent accuracy: the ratio of the smaller value to
      the larger value between the forecast and observed counts, as a percentage. It is bounded
      between 0% and 100%, where 100% is a perfect match, and it does not depend on the direction
      of the miss.
    </p>
    <div id="eq-acc" style="text-align: center; margin: 0.75rem 0 1rem;"></div>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      <strong>Example:</strong> if the predicted peak is 450 and the observed peak is 500,
      the Peak-to-Peak percent accuracy is (450 / 500) &times; 100 = 90%.
    </p>

    <hr style="border: none; border-top: 1px solid #e0e0e0; margin: 0 0 1.5rem;">

    <p style="font-size: 14px; font-weight: 700; margin: 0 0 0.5rem;">Putting it together across horizons and seasons</p>
    <p style="font-size: 14px; line-height: 1.6; margin: 0 0 1.5rem;">
      Every measure is calculated separately for each horizon. When results are combined across
      horizons or seasons (as in the location comparison), <strong>timing</strong> is averaged as
      the mean number of weeks off <em>regardless of direction</em>, so being early one time and
      late another cannot cancel out; the magnitude accuracies are averaged directly. A
      smallest-to-largest range is shown next to each average.
    </p>

  </div>

  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.css">
  <script src="https://cdn.jsdelivr.net/npm/katex@0.16.9/dist/katex.min.js"></script>
  <script>
    katex.render(
      "\\\\text{Peak Timing} = \\\\text{Predicted Peak Week} - \\\\text{Observed Peak Week}",
      document.getElementById("eq-timing"), { throwOnError: false, displayMode: true });
    katex.render(
      "\\\\text{Peak-to-Peak Magnitude} = \\\\text{Predicted Peak Value} - \\\\text{Observed Peak Value}",
      document.getElementById("eq-p2p"), { throwOnError: false, displayMode: true });
    katex.render(
      "\\\\text{Magnitude at Predicted Peak} = \\\\text{Predicted Peak Value} - \\\\text{Observed Value That Week}",
      document.getElementById("eq-samewk"), { throwOnError: false, displayMode: true });
    katex.render(
      "\\\\text{Magnitude at Observed Peak} = \\\\text{Forecast at True Peak Week} - \\\\text{Observed Peak Value}",
      document.getElementById("eq-peakwk"), { throwOnError: false, displayMode: true });
    katex.render(
      "\\\\text{Percent Accuracy} = \\\\frac{\\\\min(\\\\text{Forecasted},\\\\, \\\\text{Observed})}{\\\\max(\\\\text{Forecasted},\\\\, \\\\text{Observed})} \\\\times 100",
      document.getElementById("eq-acc"), { throwOnError: false, displayMode: true });
  </script>
  '))

  methods_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Detailed Methods (Peak Timing & Magnitude)")),
    htmltools::div(class = "accordion-body", methods_html)
  )


#------------------------------------------------------------------------------#
# Assembling the full drop-down ------------------------------------------------
#------------------------------------------------------------------------------#

  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Forecasted Peak Performance (Timing & Magnitude)")),
    htmltools::div(
      class = "accordion-body",
      intro_html,
      dropdown_html,
      table_html,
      table_note_html,
      plot_block,
      compare_accordion,
      methods_accordion
    )
  )

}
