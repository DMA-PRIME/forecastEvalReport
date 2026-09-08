#------------------------------------------------------------------------------#
#                                                                              #
#              Creating the trend capture summary plot                         #
#                                                                              #
#------------------------------------------------------------------------------#
# About: This function builds the main figure for the Trend/Phase-Specific     #
# Performance section: three side-by-side panels (Ascension, Peak, Decline),   #
# each holding one 100% stacked bar per forecast horizon. Every eligible       #
# forecast-target week is classified by how far the forecasted five-level      #
# trend label landed from the observed one: captured (same label), a near      #
# miss (one level off), or a strong miss (two or more levels off). The         #
# capture percent is printed at the end of every bar, each panel header        #
# carries the phase-level capture rate, and a one-line takeaway under the      #
# figure names the strongest and weakest phases so the key result reads in    #
# seconds. All seasons in the data are combined.                               #
#                                                                              #
# Interactivity: hovering any bar segment dims the rest of the figure and     #
# opens a card with the full outcome breakdown for that phase and horizon;     #
# hovering a legend entry isolates that outcome across every panel so, for    #
# example, all strong misses light up at once.                                 #
#------------------------------------------------------------------------------#

#' Build the trend capture summary figure
#'
#' @param phase_data Data frame of row-level trend/phase results carrying at
#'   least location, phase, horizon, observed_trend, forecast_trend, and
#'   is_transmission.
#' @param location Location code to display.
#' @param id_suffix Suffix appended to element ids so several locations can
#'   render their own copy of the figure on one page.
#' @param time_step Days per forecast step (7 for weekly data); used to
#'   translate horizon numbers into plain-language row labels.
#'
#' @return An HTML string containing the figure, or a status message when no
#'   eligible rows exist.
#' @noRd
make_trend_capture_summary_plot <- function(phase_data,
                                            location,
                                            id_suffix = "",
                                            time_step = 7){

#------------------------------------------------------------------------------#
# Guarding the inputs -----------------------------------------------------------
#------------------------------------------------------------------------------#

  needed <- c("location", "phase", "horizon",
              "observed_trend", "forecast_trend")

  if(is.null(phase_data) || !is.data.frame(phase_data) ||
     nrow(phase_data) == 0L || !all(needed %in% names(phase_data))){
    return(paste0(
      '<p style="font-size:13px;color:#8a6d3b;background:#fcf8e3;',
      'border:1px solid #faebcc;border-radius:4px;padding:8px 12px;">',
      'No eligible trend/phase rows were available to draw the trend ',
      'capture figure.</p>'
    ))
  }

#------------------------------------------------------------------------------#
# Filtering to eligible rows ----------------------------------------------------
#------------------------------------------------------------------------------#

  ladder <- c("large decrease", "decrease", "stable",
              "increase", "large increase")

  rows <- phase_data[phase_data$location == location, , drop = FALSE]

  # Transmission-season weeks only, when the flag is present
  if("is_transmission" %in% names(rows)){
    rows <- rows[rows$is_transmission %in% TRUE, , drop = FALSE]
  }

  # Matching the trend labels onto the five-level ladder
  obs_idx <- match(tolower(trimws(as.character(rows$observed_trend))), ladder)
  fc_idx  <- match(tolower(trimws(as.character(rows$forecast_trend))), ladder)

  keep <- !is.na(obs_idx) & !is.na(fc_idx) &
    rows$phase %in% c("Ascension", "Peak", "Decline") &
    !is.na(rows$horizon)

  rows    <- rows[keep, , drop = FALSE]
  obs_idx <- obs_idx[keep]
  fc_idx  <- fc_idx[keep]

  if(nrow(rows) == 0L){
    return(paste0(
      '<p style="font-size:13px;color:#8a6d3b;background:#fcf8e3;',
      'border:1px solid #faebcc;border-radius:4px;padding:8px 12px;">',
      'No eligible trend/phase rows were available for this location.</p>'
    ))
  }

#------------------------------------------------------------------------------#
# Classifying each week ---------------------------------------------------------
#------------------------------------------------------------------------------#
# About: A week is captured when the forecast called the same five-level      #
# label as observed, a near miss when the label was one level off, and a      #
# strong miss when it was two or more levels off.                             #
#------------------------------------------------------------------------------#

  offset   <- abs(fc_idx - obs_idx)
  category <- ifelse(offset == 0L, "captured",
                     ifelse(offset == 1L, "near", "strong"))

  phases   <- c("Ascension", "Peak", "Decline")
  horizons <- sort(unique(rows$horizon))
  n_h      <- length(horizons)

  # Plain-language horizon labels: "2 wk ahead" reads faster than "H2"
  if(is.null(time_step) || !is.finite(time_step)) time_step <- 7
  hz_lab <- vapply(horizons, function(h){
    hn <- suppressWarnings(as.numeric(h))
    if(!is.finite(hn)) return(paste0("H", h))
    if(time_step == 7){
      if(hn == 0) "Same wk" else paste0(hn, " wk ahead")
    }else{
      d <- hn * time_step
      if(d == 0) "Same day" else
        paste0(d, if(d == 1) " day ahead" else " days ahead")
    }
  }, character(1))

  # Counts per phase x horizon x category
  cell <- function(ph, hz){
    sel <- rows$phase == ph & rows$horizon == hz
    c(captured = sum(category[sel] == "captured"),
      near     = sum(category[sel] == "near"),
      strong   = sum(category[sel] == "strong"))
  }

#------------------------------------------------------------------------------#
# Geometry and palette ----------------------------------------------------------
#------------------------------------------------------------------------------#

  col_cap    <- "#1baf7a"   # captured
  col_near   <- "#f2a33c"   # near miss (one level off)
  col_strong <- "#e34948"   # strong miss (two or more levels off)
  col_empty  <- "#f0efe9"

  phase_fill <- c("Ascension" = "#E6F1FB",
                  "Peak"      = "#FAEEDA",
                  "Decline"   = "#E1F5EE")
  phase_text <- c("Ascension" = "#2c5d8f",
                  "Peak"      = "#8a6d3b",
                  "Decline"   = "#2c7a5e")

  cat_label <- c(captured = "Captured",
                 near     = "Near miss (1 level off)",
                 strong   = "Strong miss (2+ levels off)")
  cat_color <- c(captured = col_cap, near = col_near, strong = col_strong)

  svg_w    <- 760
  gutter   <- 92                      # horizon labels
  gap      <- 14
  panel_w  <- floor((svg_w - gutter - 2 * gap - 8) / 3)
  panel_x  <- gutter + (seq_len(3) - 1) * (panel_w + gap)

  legend_y <- 16
  header_y <- 34
  header_h <- 46
  bars_y0  <- header_y + header_h + 12
  bar_h    <- 18
  pitch    <- 30
  pct_w    <- 40                      # room for the bold percent
  bar_w    <- panel_w - pct_w

  # The observed-trend strip below the panels: labels present in the data,
  # kept in ladder order, pooled across phases and horizons
  label_display <- c("Large Decrease", "Decrease", "Stable",
                     "Increase", "Large Increase")
  obs_present <- which(vapply(seq_along(ladder),
                              function(i) any(obs_idx == i), logical(1)))
  n_obs   <- length(obs_present)
  strip_y <- bars_y0 + n_h * pitch + 16      # strip heading baseline
  strip_rows_y0 <- strip_y + 14
  svg_h   <- strip_rows_y0 + n_obs * pitch + 6

  box_id <- paste0("tcsBox", id_suffix)

  esc <- function(x) htmltools::htmlEscape(x)
  pct <- function(k, n) if(n > 0) round(100 * k / n) else NA_real_

#------------------------------------------------------------------------------#
# Hover card content ------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Every segment carries the full breakdown for its bar so the card      #
# always answers "of the weeks in this phase at this horizon, how many were    #
# captured, near misses, and strong misses". The hovered outcome is bolded.    #
#------------------------------------------------------------------------------#

  card_html <- function(header, k, focus, extra = ""){

    n <- sum(k)

    line <- function(key){
      strong_open  <- if(key == focus) "<strong>" else ""
      strong_close <- if(key == focus) "</strong>" else ""
      paste0(
        '<span style="display:block;margin:1px 0;">',
        '<span style="display:inline-block;width:9px;height:9px;',
        'border-radius:2px;background:', cat_color[[key]],
        ';margin-right:6px;"></span>',
        strong_open, cat_label[[key]], ": ", k[[key]], " of ", n,
        " (", pct(k[[key]], n), "%)", strong_close, '</span>'
      )
    }

    paste0(
      '<span style="display:block;font-weight:700;color:#522D80;',
      'margin:0 0 3px;">', header, '</span>',
      line("captured"), line("near"), line("strong"), extra
    )
  }

#------------------------------------------------------------------------------#
# Legend ------------------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Legend entries are interactive: hovering one isolates that outcome    #
# across all three panels.                                                     #
#------------------------------------------------------------------------------#

  legend_item <- function(x, key, label){
    paste0(
      '<g class="tcs-leg" data-cat="', key, '" style="cursor:pointer;">',
      '<rect x="', x, '" y="', legend_y - 10, '" width="12" height="12" rx="2"',
      ' fill="', cat_color[[key]], '"/>',
      '<text x="', x + 17, '" y="', legend_y,
      '" font-size="11.5" fill="#52514e">', label, '</text>',
      '</g>'
    )
  }

  legend_svg <- paste0(
    legend_item(gutter, "captured", "Captured (same trend)"),
    legend_item(gutter + 168, "near", "Near miss (1 level off)"),
    legend_item(gutter + 340, "strong", "Strong miss (2+ levels off)")
  )

#------------------------------------------------------------------------------#
# Panels ------------------------------------------------------------------------
#------------------------------------------------------------------------------#

  panels_svg <- ""
  phase_capture <- setNames(rep(NA_real_, 3), phases)

  for(p in seq_along(phases)){

    ph <- phases[p]
    px <- panel_x[p]

    #####################################################
    # Panel header with the phase-level capture rate    #
    #####################################################
    ph_sel <- rows$phase == ph
    ph_n   <- sum(ph_sel)
    ph_cap <- sum(category[ph_sel] == "captured")
    phase_capture[ph] <- pct(ph_cap, ph_n)

    header_sub <- if(ph_n > 0){
      paste0("Captured ", phase_capture[ph], "% \u00b7 ", ph_n, " weeks")
    }else{
      "No eligible weeks"
    }

    # Observed date range of the phase, printed between the phase title and
    # the capture line. The formatter emits an HTML entity for the dash;
    # inside SVG text a literal en dash is used instead
    phase_dates <- format_trend_phase_date_ranges(phase_data, location, ph)
    phase_dates <- gsub("&ndash;", "\u2013", phase_dates, fixed = TRUE)
    phase_dates <- gsub("&mdash;", "\u2014", phase_dates, fixed = TRUE)

    panels_svg <- paste0(
      panels_svg,
      '<text x="', px + bar_w / 2, '" y="', header_y + 11,
      '" text-anchor="middle" font-size="13" font-weight="700" fill="',
      phase_text[ph], '">', ph, '</text>',
      if(nzchar(phase_dates)) paste0(
        '<text x="', px + bar_w / 2, '" y="', header_y + 25,
        '" text-anchor="middle" font-size="10.5" fill="#8a89a0">',
        htmltools::htmlEscape(phase_dates), '</text>'
      ) else "",
      '<text x="', px + bar_w / 2, '" y="', header_y + 39,
      '" text-anchor="middle" font-size="11" fill="#898781">',
      header_sub, '</text>',
      '<rect x="', px, '" y="', header_y + 44, '" width="', bar_w,
      '" height="3" rx="1.5" fill="', phase_text[ph],
      '" fill-opacity="0.35"/>'
    )

    #####################################################
    # One stacked bar per horizon                       #
    #####################################################
    for(h in seq_len(n_h)){

      hz     <- horizons[h]
      by     <- bars_y0 + (h - 1) * pitch
      k      <- cell(ph, hz)
      n      <- sum(k)
      bar_id <- paste0("p", p, "h", h)

      # Empty cell: a light bar with a note, no percent
      if(n == 0L){
        panels_svg <- paste0(
          panels_svg,
          '<rect x="', px, '" y="', by, '" width="', bar_w,
          '" height="', bar_h, '" rx="3" fill="', col_empty, '"/>',
          '<text x="', px + bar_w / 2, '" y="', by + 13,
          '" text-anchor="middle" font-size="10" fill="#898781">',
          'no eligible weeks</text>'
        )
        next
      }

      w_cap    <- bar_w * k[["captured"]] / n
      w_near   <- bar_w * k[["near"]] / n
      w_strong <- bar_w * k[["strong"]] / n

      seg <- function(x0, w, key){
        if(w <= 0) return("")
        paste0(
          '<rect class="tcs-seg" data-bar="', bar_id,
          '" data-cat="', key,
          '" data-info="',
          htmltools::htmlEscape(
            card_html(paste0(ph, ' &#183; ', esc(hz_lab[h])), k, key),
            attribute = TRUE),
          '" x="', sprintf("%.1f", x0), '" y="', by,
          '" width="', sprintf("%.1f", w), '" height="', bar_h,
          '" fill="', cat_color[[key]], '" style="cursor:pointer;"/>'
        )
      }

      panels_svg <- paste0(
        panels_svg,

        # Rounded background so the stacked bar keeps soft corners
        '<rect x="', px, '" y="', by, '" width="', bar_w, '" height="',
        bar_h, '" rx="3" fill="', col_empty, '"/>',

        seg(px, w_cap, "captured"),
        seg(px + w_cap, w_near, "near"),
        seg(px + w_cap + w_near, w_strong, "strong"),

        # The headline number for the row: percent of weeks captured
        '<text class="tcs-pct" x="', px + bar_w + 6, '" y="', by + 13.5,
        '" font-size="12" font-weight="700" fill="#0f6e56">',
        pct(k[["captured"]], n), '%</text>'
      )
    }
  }

#------------------------------------------------------------------------------#
# Observed-trend strip -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: One full-width bar per observed five-level trend, pooled across       #
# phases and horizons, showing which observed trends the forecast tends to    #
# miss. The hover card adds whether the misses leaned upward or downward.     #
#------------------------------------------------------------------------------#

  strip_bar_w <- panel_x[3] + bar_w - gutter
  strip_svg <- paste0(
    '<text x="', gutter, '" y="', strip_y,
    '" font-size="12" font-weight="700" fill="#52514e">',
    'Which Observed Trends Get Missed</text>',
    '<text x="', gutter + strip_bar_w, '" y="', strip_y,
    '" text-anchor="end" font-size="10.5" fill="#898781">',
    'All phases and horizons combined</text>'
  )

  for(r in seq_len(n_obs)){

    li  <- obs_present[r]
    sel <- obs_idx == li
    k   <- c(captured = sum(category[sel] == "captured"),
             near     = sum(category[sel] == "near"),
             strong   = sum(category[sel] == "strong"))
    n   <- sum(k)
    by  <- strip_rows_y0 + (r - 1) * pitch
    bar_id <- paste0("obs", r)

    # Direction of the misses for this observed label
    up   <- sum(sel & fc_idx > obs_idx)
    down <- sum(sel & fc_idx < obs_idx)
    extra <- if(up + down > 0){
      paste0(
        '<span style="display:block;margin:3px 0 0;color:#898781;">',
        'Of the misses, ', up, ' leaned upward and ', down,
        ' leaned downward.</span>'
      )
    }else{""}

    w_cap    <- strip_bar_w * k[["captured"]] / n
    w_near   <- strip_bar_w * k[["near"]] / n
    w_strong <- strip_bar_w * k[["strong"]] / n

    seg <- function(x0, w, key){
      if(w <= 0) return("")
      paste0(
        '<rect class="tcs-seg" data-bar="', bar_id,
        '" data-cat="', key,
        '" data-info="',
        htmltools::htmlEscape(
          card_html(paste0('Observed trend: ', label_display[li]),
                    k, key, extra),
          attribute = TRUE),
        '" x="', sprintf("%.1f", x0), '" y="', by,
        '" width="', sprintf("%.1f", w), '" height="', bar_h,
        '" fill="', cat_color[[key]], '" style="cursor:pointer;"/>'
      )
    }

    strip_svg <- paste0(
      strip_svg,
      '<text x="', gutter - 8, '" y="', by + 13.5,
      '" text-anchor="end" font-size="11.5" font-weight="700"',
      ' fill="#52514e">', label_display[li], '</text>',
      '<rect x="', gutter, '" y="', by, '" width="', strip_bar_w,
      '" height="', bar_h, '" rx="3" fill="', col_empty, '"/>',
      seg(gutter, w_cap, "captured"),
      seg(gutter + w_cap, w_near, "near"),
      seg(gutter + w_cap + w_near, w_strong, "strong"),
      '<text x="', gutter + strip_bar_w + 6, '" y="', by + 13.5,
      '" font-size="12" font-weight="700" fill="#0f6e56">',
      pct(k[["captured"]], n), '%</text>'
    )
  }

#------------------------------------------------------------------------------#
# Horizon labels ----------------------------------------------------------------
#------------------------------------------------------------------------------#

  labels_svg <- paste0(vapply(seq_len(n_h), function(h){
    paste0(
      '<text x="', gutter - 8, '" y="',
      bars_y0 + (h - 1) * pitch + 13.5,
      '" text-anchor="end" font-size="11.5" font-weight="700"',
      ' fill="#52514e">', esc(hz_lab[h]), '</text>'
    )
  }, character(1)), collapse = "")

#------------------------------------------------------------------------------#
# Hover interactivity -----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: Hovering a segment dims all other segments (its own bar stays        #
# readable) and opens a card that follows the pointer. Hovering a legend       #
# entry isolates its outcome across every panel. All state is restored on      #
# leave, and each location's figure runs its own copy of the script.           #
#------------------------------------------------------------------------------#

  script <- paste0(
    '<script>(function(){',
    'var box=document.getElementById("', box_id, '");',
    'if(!box)return;',
    'var tip=box.querySelector(".tcs-tip");',
    'var segs=box.querySelectorAll(".tcs-seg");',
    'function reset(){',
    'segs.forEach(function(s){s.style.opacity=1;});',
    'if(tip)tip.style.display="none";',
    '}',
    'segs.forEach(function(s){',
    's.addEventListener("mouseenter",function(){',
    'var bar=s.getAttribute("data-bar");',
    'segs.forEach(function(o){',
    'o.style.opacity=o===s?1:(o.getAttribute("data-bar")===bar?0.75:0.22);',
    '});',
    'if(tip){tip.innerHTML=s.getAttribute("data-info");',
    'tip.style.display="block";}',
    '});',
    's.addEventListener("mousemove",function(e){',
    'if(!tip)return;',
    'var r=box.getBoundingClientRect();',
    'var x=e.clientX-r.left+14,y=e.clientY-r.top+14;',
    'if(x+tip.offsetWidth>r.width-4)x=e.clientX-r.left-tip.offsetWidth-14;',
    'if(y+tip.offsetHeight>r.height-4)y=e.clientY-r.top-tip.offsetHeight-14;',
    'tip.style.left=x+"px";tip.style.top=y+"px";',
    '});',
    's.addEventListener("mouseleave",reset);',
    '});',
    'box.querySelectorAll(".tcs-leg").forEach(function(l){',
    'l.addEventListener("mouseenter",function(){',
    'var cat=l.getAttribute("data-cat");',
    'segs.forEach(function(o){',
    'o.style.opacity=o.getAttribute("data-cat")===cat?1:0.15;',
    '});',
    '});',
    'l.addEventListener("mouseleave",reset);',
    '});',
    '})();</script>'
  )

#------------------------------------------------------------------------------#
# Assembling the figure ---------------------------------------------------------
#------------------------------------------------------------------------------#

  paste0(
    '<div id="', box_id, '" style="position:relative;margin:0 0 0.25rem;">',
    '<svg viewBox="0 0 ', svg_w, ' ', svg_h,
    '" width="100%" role="img" xmlns="http://www.w3.org/2000/svg"',
    ' font-family="sans-serif" style="display:block;">',
    legend_svg,
    labels_svg,
    panels_svg,
    strip_svg,
    '</svg>',
    '<div class="tcs-tip" style="position:absolute;display:none;left:0;top:0;',
    'background:#fcfcfb;border:1px solid #c3c2b7;border-radius:6px;',
    'box-shadow:0 2px 8px rgba(0,0,0,0.12);padding:8px 11px;',
    'font-size:12px;line-height:1.5;color:#333;z-index:30;',
    'pointer-events:none;max-width:280px;"></div>',
    '</div>',
    script
  )
}
