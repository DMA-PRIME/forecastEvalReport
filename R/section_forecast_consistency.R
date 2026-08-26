#' Render the forecast consistency section
#'
#' Builds an interactive Plotly-based "forecast consistency" visualization for
#' each location, overlaying the most recent operational forecasts against the
#' observed target data. The newest forecast is solid with blue prediction
#' intervals; older forecasts are dashed with lighter bands, and forecasts
#' beyond the two most recent default to hidden. A floating legend lets the
#' reader toggle individual forecasts, the target data, and auxiliary variables.
#'
#' The section is only rendered when an implementation model is present and at
#' least one archived forecast can be located in the `Forecasts/` directory
#' written by `extract_implementation_data()`.
#'
#' @param impl_meta Metadata list from `extract_implementation_data()`.
#' @param eval_meta Metadata list from `extract_evaluation_data()`, or `NULL`.
#' @param config Validated config list from `validate_report_params()`.
#' @param implementation_model Validated implementation model data frame,
#'   or `NULL`.
#' @param master_data Assembled master data frame from `assemble_report_data()`.
#' @param variables_crosswalk Validated crosswalk data frame, or `NULL`.
#' @param plot_styles A named list of plot style settings produced by
#'   `create_plot_styles()`.
#' @param max_forecasts Integer. Maximum number of most-recent forecasts to
#'   overlay. Default 5.
#' @param forecast_every Optional positive integer. When set, select every Xth
#'   forecast (oldest-first) starting at the beginning of the view window
#'   (`view_start` if supplied, else the earliest forecast). Every qualifying
#'   forecast on the stride is shown -- the view window bounds the count and
#'   `max_forecasts` does not cap it. `NULL` (default) keeps the newest
#'   `max_forecasts` consecutive forecasts.
#' @param end_cushion Optional positive integer used only in stride mode
#'   (`forecast_every`). Drops the most-recent N forecasts from the window pool
#'   before striding, so the stride never reaches the very last forecast(s).
#'   `NULL` (default) keeps them.
#'
#' @param tick_size,forecast_size,observed_size,aux_size Optional single
#'   positive numbers: axis tick-label size (independent of `font_size`),
#'   forecast median line width, observed (truth) line width, and
#'   auxiliary-variable line width respectively. `NULL` uses the built-in value.
#' @param forecast_color,observed_color Optional single colors for the forecast
#'   median lines (overrides recency coloring; dash distinction kept) and the
#'   observed (truth) line. `NULL` keeps the built-in colors.
#'
#' @return An [htmltools::tagList()] for rendering, or `NULL` invisibly when no
#'   implementation model or archived forecasts are available.
#'
#' @keywords internal
#' @noRd
section_forecast_consistency <- function(impl_meta,
                                         eval_meta            = NULL,
                                         config               = NULL,
                                         implementation_model = NULL,
                                         master_data          = NULL,
                                         variables_crosswalk  = NULL,
                                         plot_styles          = NULL,
                                         max_forecasts        = 5L,
                                         forecast_every       = NULL,
                                         end_cushion          = NULL,
                                         for_export           = FALSE,
                                         location             = NULL,
                                         font_size            = NULL,
                                         view_start           = NULL,
                                         view_end             = NULL,
                                         tick_size            = NULL,
                                         forecast_size        = NULL,
                                         forecast_color       = NULL,
                                         observed_size        = NULL,
                                         observed_color       = NULL,
                                         aux_size             = NULL,
                                         aux_color            = NULL,
                                         title_gap            = NULL){

#------------------------------------------------------------------------------#
# Input guards -----------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section provides multiple guardrails against issues within the   #
# function inputs. This includes missing files, NA and NULL files. If an       #
# error occurs here, the remainder of the function will not run.               #
#------------------------------------------------------------------------------#

  ##########################################################
  # Checking if plot style is provided and in right format #
  ##########################################################
  if(is.null(plot_styles) || !is.list(plot_styles)){

    # Creating the empty plot styles file
    plot_styles <- forecastEvalReport::create_plot_styles()

  }

  ###############################################
  # Checking if implementation file is provided #
  ###############################################
  has_impl <- !is.null(implementation_model) &&
    is.data.frame(implementation_model) &&
    nrow(implementation_model) > 0

  #############################################
  # Error: No implementation File is Provided #
  #############################################
  if(!has_impl){

    # Message to show to users
    message("section_forecast_consistency: No implementation model. Skipping.")

    # Returning nothing
    return(invisible(NULL))

  }

  ##########################################
  # Error: No master data file is Provided #
  ##########################################
  if(is.null(master_data) || !is.data.frame(master_data) ||
     nrow(master_data) == 0){

    # Message to show to users
    message("section_forecast_consistency: No master data available. Skipping.")

    # Returning NULL
    return(invisible(NULL))
  }

#------------------------------------------------------------------------------#
# Pulling all forecasts --------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section pulls all old forecasts from the folder created by the   #
# implementation function.

  # build_forecast_archive() returns NULL for small max_forecasts caps, so load
  # generously with a value known to work, then keep the newest max_forecasts
  # below. (The report passes 10000, which is why it never hits the bug.)
  archive <- build_forecast_archive(impl_meta,
                                    max_forecasts = max(as.integer(max_forecasts),
                                                        10000L))

  if(is.null(archive) || nrow(archive) == 0){
    message("section_forecast_consistency: No archived forecasts found. Skipping.")
    return(invisible(NULL))
  }

  # Truncation flag for the recency emphasis. Computed here on the FULL archive
  # (before any window/stride filtering below) so the builder still knows the
  # last available forecast even after stride mode trims later forecasts out.
  # When view_end falls before that last forecast, the builder flattens the
  # emphasis (no single forecast singled out).
  flatten_emphasis <- !is.null(view_end) && nrow(archive) > 0 &&
    !is.na(as.Date(view_end)) &&
    as.Date(view_end) < max(archive$reference_date, na.rm = TRUE)

  # Select which forecasts to overlay. Two modes:
  #   * forecast_every = X : stride mode -- every Xth forecast, oldest-first,
  #     anchored at the start of the viewport window (view_start when supplied,
  #     else the earliest forecast). Every qualifying forecast on the stride is
  #     shown; the view window bounds the count (max_forecasts does not cap it).
  #   * otherwise          : the newest max_forecasts forecasts (consecutive).
  # Either way the result is left oldest-first so the newest draws on top.
  .stride <- !is.null(forecast_every) && is.numeric(forecast_every) &&
    length(forecast_every) == 1 && is.finite(forecast_every) &&
    forecast_every >= 1

  if(isTRUE(.stride)){

    # Oldest -> newest
    archive <- archive[order(archive$reference_date), , drop = FALSE]

    # Restrict to the viewport window when one was supplied (anchor = view_start)
    if(!is.null(view_start) && !is.null(view_end)){
      .vs    <- as.Date(view_start)
      .ve    <- as.Date(view_end)
      in_win <- !is.na(archive$reference_date) &
        archive$reference_date >= .vs & archive$reference_date <= .ve
      if(any(in_win)) archive <- archive[in_win, , drop = FALSE]
    }

    # Optional end cushion: drop the most-recent N forecasts from the (window-
    # restricted, oldest-first) pool before striding, so the stride never
    # reaches the very last forecast(s) in the window.
    if(!is.null(end_cushion) && is.numeric(end_cushion) &&
       length(end_cushion) == 1 && is.finite(end_cushion) &&
       end_cushion >= 1 && nrow(archive) > 0){
      .keep   <- max(0L, nrow(archive) - as.integer(end_cushion))
      archive <- archive[seq_len(.keep), , drop = FALSE]
    }

    # Every Xth forecast starting at the first (the beginning of the window).
    # No max_forecasts cap in stride mode: the view window bounds the count, so
    # every qualifying forecast that lands on the stride is shown.
    pick <- if(nrow(archive) > 0){
      seq(1L, nrow(archive), by = as.integer(forecast_every))
    }else{
      integer(0)
    }

    archive <- archive[pick, , drop = FALSE]

  }else if(is.numeric(max_forecasts) && is.finite(max_forecasts) &&
           max_forecasts >= 1 && nrow(archive) > max_forecasts){

    # Default: keep the newest max_forecasts, then restore oldest-first order
    newest  <- order(archive$reference_date,
                     decreasing = TRUE)[seq_len(max_forecasts)]
    archive <- archive[newest, , drop = FALSE]
    archive <- archive[order(archive$reference_date), , drop = FALSE]
  }

  #------------------------------------------------------------------------------#
  # Resolving metadata -----------------------------------------------------------
  #------------------------------------------------------------------------------#

  meta <- if(!is.null(impl_meta)) impl_meta else eval_meta

  #------------------------------------------------------------------------------#
  # Resolving the location crosswalk (raw code <-> display name) -----------------
  #------------------------------------------------------------------------------#
  # About: Forecast files on disk are keyed by raw location code (e.g. FIPS      #
  # "45"); truth rows in master_data are keyed by display name (e.g. "South      #
  # Carolina"). The consistency plot needs both -- the raw code to filter the    #
  # archived forecast CSVs, the display name to match truth. This builds a named #
  # vector raw-code -> display-name with the same hubverse/metrocast crosswalk   #
  # assemble_report_data() used: truth names first, raw value as final fallback. #
  #------------------------------------------------------------------------------#

  # Display names already present in the truth data (the match target)
  truth_locs <- unique(master_data$location[
    !is.na(master_data$variable_type) &
      master_data$variable_type == "outcome_data"])

  # Resolve one raw location code to the display name the truth data uses
  resolve_display <- function(code){

    # Coercing and trimming the raw code
    code <- trimws(as.character(code))

    # User-supplied crosswalk takes precedence over everything below, so the
    # display name matches the (cleaned) truth values already in master_data.
    loc_xwalk <- config$location_crosswalk
    if(!is.null(loc_xwalk) && !is.na(code) && code %in% names(loc_xwalk)){
      return(unname(loc_xwalk[[code]]))
    }

    # Already a truth display name -- keep as is
    if(code %in% truth_locs) return(code)

    # FIPS code -> display name (hubverse)
    idx <- match(code, forecastEvalReport::hubverse_locations$location)
    if(!is.na(idx)){
      return(forecastEvalReport::hubverse_locations$location_name[idx])
    }

    # Abbreviation -> display name (hubverse)
    idx <- match(toupper(code),
                 toupper(forecastEvalReport::hubverse_locations$abbreviation))
    if(!is.na(idx)){
      return(forecastEvalReport::hubverse_locations$location_name[idx])
    }

    # Metrocast code -> display name
    idx <- match(code, forecastEvalReport::metrocast_locations$location)
    if(!is.na(idx)){
      return(forecastEvalReport::metrocast_locations$location_name[idx])
    }

    # Nothing matched -- keep the raw value
    code
  }

  # Raw location codes: prefer the implementation model (the forecast-file
  # key), then the metadata map's names, then the truth names themselves
  raw_codes <- if(!is.null(implementation_model) &&
                  is.data.frame(implementation_model) &&
                  "location" %in% names(implementation_model)){
    unique(as.character(implementation_model$location))
  }else if(!is.null(meta$locations) && length(meta$locations) > 0){
    nm <- names(meta$locations)
    if(is.null(nm)) unname(meta$locations) else nm
  }else{
    truth_locs
  }

  # Named vector raw code -> display name (loc_display = value, raw_loc = name)
  locations <- stats::setNames(
    vapply(raw_codes, resolve_display, character(1)),
    raw_codes
  )

  #------------------------------------------------------------------------------#
  # Single-geography export filter -----------------------------------------------
  #------------------------------------------------------------------------------#
  # About: An export builds one geography. The request may arrive as a display   #
  # name or a raw code, so keep the entry whose value (display) or name (raw)    #
  # matches it.                                                                  #
  #------------------------------------------------------------------------------#

  if(!is.null(location)){
    keep <- vapply(seq_along(locations), function(i){
      identical(unname(locations[i]), as.character(location)) ||
        identical(names(locations)[i], as.character(location))
    }, logical(1))
    if(any(keep)) locations <- locations[keep]
  }

  outcome_display <- if(!is.null(variables_crosswalk)){

    out_rows <- variables_crosswalk[
      !is.na(variables_crosswalk$variable_type) &
        variables_crosswalk$variable_type == "outcome", ]

    if(nrow(out_rows) > 0){
      clean <- unique(out_rows$clean_name_full)
      clean <- clean[!is.na(clean) & nchar(trimws(clean)) > 0 &
                       clean != "USER: provide a definition"]
      if(length(clean) > 0) paste(clean, collapse = ", ") else
        paste(meta$outcome, collapse = ", ")
    }else{
      paste(meta$outcome, collapse = ", ")
    }

  }else{
    paste(meta$outcome, collapse = ", ")
  }

  spatial_scale   <- if(!is.null(meta$spatial_scale)) meta$spatial_scale else ""

  ######################################
  # Disease display label              #
  ######################################
  # Prefer the crosswalk's clean disease name (e.g. "COVID-19") from the
  # outcome row; fall back to the raw config$disease code only if absent.
  disease_display <- {
    dcl <- NULL
    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk) &&
       all(c("variable_type", "disease_name_clean") %in% names(variables_crosswalk))){
      drows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "outcome", ]
      if(nrow(drows) > 0){
        v <- unique(drows$disease_name_clean)
        v <- v[!is.na(v) & nzchar(v)]
        if(length(v) > 0) dcl <- paste(v, collapse = ", ")
      }
    }
    if(!is.null(dcl)) dcl else if(!is.null(config$disease)) config$disease else ""
  }

  #------------------------------------------------------------------------------#
  # Main plot loop ---------------------------------------------------------------
  #------------------------------------------------------------------------------#

  plotly_list     <- list()
  hover_text_list <- list()
  plot_id         <- "plot3"
  plot_count      <- 0L

  for(i in seq_along(locations)){

    loc_display <- locations[[i]]
    raw_loc     <- names(locations)[i]

    if(is.null(raw_loc) || is.na(raw_loc) || nchar(trimws(raw_loc)) == 0){
      raw_loc <- loc_display
    }

    res <- make_forecast_consistency_plot(
      loc_display          = loc_display,
      raw_loc              = raw_loc,
      archive              = archive,
      master_data          = master_data,
      implementation_model = implementation_model,
      impl_meta            = impl_meta,
      config               = config,
      variables_crosswalk  = variables_crosswalk,
      plot_styles          = plot_styles,
      outcome_display      = outcome_display,
      disease_display      = disease_display,
      spatial_scale        = spatial_scale,
      for_export           = for_export,
      font_size            = font_size,
      view_start           = view_start,
      view_end             = view_end,
      tick_size            = tick_size,
      forecast_size        = forecast_size,
      forecast_color       = forecast_color,
      observed_size        = observed_size,
      observed_color       = observed_color,
      aux_size             = aux_size,
      aux_color            = aux_color,
      title_gap            = title_gap,
      flatten              = flatten_emphasis
    )

    if(is.null(res)) next

    plot_count <- plot_count + 1L
    plotly_list[[plot_count]]      <- res$plot
    names(plotly_list)[plot_count] <- loc_display
    hover_text_list[[plot_count]]  <- res$hover_text
  }

  if(length(plotly_list) == 0){
    message("section_forecast_consistency: No plots built. Returning NULL.")
    return(invisible(NULL))
  }

  if(length(hover_text_list) != length(plotly_list)){
    hover_text_list <- hover_text_list[seq_along(plotly_list)]
  }

  #------------------------------------------------------------------------------#
  # Sorting plots alphabetically -------------------------------------------------
  #------------------------------------------------------------------------------#

  sort_order      <- order(names(plotly_list))
  plotly_list     <- plotly_list[sort_order]
  hover_text_list <- hover_text_list[sort_order]

  #------------------------------------------------------------------------------#
  # Legend data ------------------------------------------------------------------
  #------------------------------------------------------------------------------#

  all_param_data <- master_data[!is.na(master_data$variable_type) &
                                  master_data$variable_type == "aux_data", ]
  has_params <- !is.null(all_param_data) && nrow(all_param_data) > 0

  location_has_params <- setNames(
    vapply(names(plotly_list), function(loc){
      if(!has_params) return(FALSE)
      any(all_param_data$location == loc)
    }, logical(1)),
    names(plotly_list)
  )

  #------------------------------------------------------------------------------#
  # Forecast legend items (newest first; newest two checked by default) ----------
  #------------------------------------------------------------------------------#

  fc_rank <- rank(-as.numeric(archive$reference_date), ties.method = "first")
  ord     <- order(archive$reference_date, decreasing = TRUE)

  forecast_items <- lapply(ord, function(r){
    d_lbl <- format(archive$reference_date[r], "%Y-%m-%d")
    build_legend_item(
      label        = paste0("Forecast Date: ", d_lbl),
      trace_name   = paste0("Forecast ", d_lbl),
      swatch_class = "forecast-line",
      checked      = (fc_rank[r] <= 2),
      swatch_attrs = list(style = "display:none;")
    )
  })

  # Add All / Remove All controls + minimal button styling
  forecast_actions <- htmltools::tagList(
    htmltools::tags$style(htmltools::HTML(
      ".forecasts-actions{display:flex;gap:8px;justify-content:center;padding:2px 0 8px 0;}
       .forecasts-actions .legend-action-btn{display:inline-flex;align-items:center;
         justify-content:center;padding:5px 12px;border-radius:6px;font-size:0.86em;
         font-weight:600;cursor:pointer;border:1px solid #e6e9ef;background:#f3f4f6;
         color:#374151;min-width:88px;height:30px;white-space:nowrap;transition:none;}
       .forecasts-actions .legend-action-btn:hover,
       .forecasts-actions .legend-action-btn:active,
       .forecasts-actions .legend-action-btn:focus{background:#f3f4f6;color:#374151;
         box-shadow:none;outline:none;}
       .forecasts-actions .legend-action-btn:disabled{opacity:0.55;cursor:not-allowed;}"
    )),
    htmltools::div(
      class = "forecasts-actions",
      htmltools::tags$button(
        type = "button", class = "legend-action-btn add-all-forecasts",
        title = "Show all forecasts", "Add All"
      ),
      htmltools::tags$button(
        type = "button", class = "legend-action-btn remove-all-forecasts",
        title = "Hide all forecasts", "Remove All"
      )
    )
  )

  forecasts_section <- build_legend_section(
    title      = "Forecasts",
    section_id = "forecasts",
    collapsed  = FALSE,
    content    = htmltools::tagList(
      if(!isTRUE(for_export)) forecast_actions,
      htmltools::div(
        class = "forecasts-scroll",
        do.call(htmltools::tagList, forecast_items)
      )
    )
  )

  #------------------------------------------------------------------------------#
  # Checkbox sync JS -------------------------------------------------------------
  #------------------------------------------------------------------------------#

  # NOTE: this runs for every plot, export included (no for_export gate), to
  # match section_forecast_plots() exactly. The onRender pass is what finalizes
  # each plotly object as an embeddable widget; skipping it on the export path
  # left the bare object from plotly::config(), which htmltools::save_html()
  # then failed to serialize inline (the plot was hoisted to standalone and
  # dropped from the page). On the static-export page the global is absent, so
  # the call is guarded -- the wrap, not the call, is what fixes embedding.
  for(k in seq_along(plotly_list)){
    if(inherits(plotly_list[[k]], "htmlwidget")){
      plotly_list[[k]] <- htmlwidgets::onRender(
        plotly_list[[k]],
        htmlwidgets::JS(
          "function(el, x) { if (window.syncLegendCheckboxesOnRender) { window.syncLegendCheckboxesOnRender(el, x); } }"
        )
      )
    }
  }

  #------------------------------------------------------------------------------#
  # Add All / Remove All bulk-toggle handler (first panel only) -------------------
  #------------------------------------------------------------------------------#
  # About: Scoped to the nearest fs-wrap so it is independent of other plots.
  # Toggling sets each forecast checkbox and dispatches a 'change' event, which
  # the per-checkbox sync handler uses to show/hide the matching traces.
  #------------------------------------------------------------------------------#

  if(!isTRUE(for_export) && inherits(plotly_list[[1]], "htmlwidget")){
    plotly_list[[1]] <- htmlwidgets::onRender(
      plotly_list[[1]],
      htmlwidgets::JS(paste0("
        function(el, x){
          var guardKey = '__forecastBulkInit_", plot_id, "';
          if(window[guardKey]) return;
          window[guardKey] = true;

          var fsWrap = el.closest('.fs-wrap');
          if(!fsWrap) return;

          var addBtn    = fsWrap.querySelector('.add-all-forecasts');
          var removeBtn = fsWrap.querySelector('.remove-all-forecasts');

          function boxes(){
            return Array.prototype.slice.call(
              fsWrap.querySelectorAll('[data-section-content=forecasts] .legend-checkbox'));
          }

          function updateBtns(){
            var b = boxes();
            if(!b.length){
              if(addBtn) addBtn.disabled = true;
              if(removeBtn) removeBtn.disabled = true;
              return;
            }
            var c = b.filter(function(cb){ return cb.checked; }).length;
            if(addBtn) addBtn.disabled = (c === b.length);
            if(removeBtn) removeBtn.disabled = (c === 0);
          }

          function toggleAll(state){
            var b = boxes();
            if(!b.length) return;
            b.forEach(function(cb){ cb.checked = state; });
            window.requestAnimationFrame(function(){
              b.forEach(function(cb){
                cb.dispatchEvent(new Event('change', { bubbles: true }));
              });
              updateBtns();
            });
          }

          if(addBtn && !addBtn.__fcBulk){
            addBtn.addEventListener('click', function(){ toggleAll(true); addBtn.blur(); });
            addBtn.__fcBulk = true;
          }
          if(removeBtn && !removeBtn.__fcBulk){
            removeBtn.addEventListener('click', function(){ toggleAll(false); removeBtn.blur(); });
            removeBtn.__fcBulk = true;
          }

          boxes().forEach(function(cb){
            if(!cb.__fcChange){ cb.addEventListener('change', updateBtns); cb.__fcChange = true; }
          });
          updateBtns();
        }
      "))
    )
  }

  #------------------------------------------------------------------------------#
  # Floating legend --------------------------------------------------------------
  #------------------------------------------------------------------------------#

  wrap_id          <- paste0("wrap-",         plot_id)
  float_legend_id  <- paste0("float-legend-", plot_id)
  drag_id          <- paste0("legend-drag-",  plot_id)
  geo_select_id    <- paste0("geoSelect-",    plot_id)

  first_hover_text <- if(length(hover_text_list) > 0) hover_text_list[[1]] else ""

  legend_body <- htmltools::div(
    class = "legend-body",

    forecasts_section,

    build_legend_section(
      title      = "Target Data",
      section_id = "target-data",
      collapsed  = FALSE,
      content    = build_legend_item(
        label        = if(length(plotly_list) > 1){
          htmltools::span(
            id = paste0("target-data-label-", plot_id),
            first_hover_text
          )
        }else{
          first_hover_text
        },
        trace_name   = "Target Data",
        swatch_class = "target-data",
        checked      = TRUE
      )
    ),

    build_auxiliary_variables_legend(
      parameter_data      = all_param_data,
      variables_crosswalk = variables_crosswalk
    )

  )

  # A static export keeps this legend but renders it expanded (no "collapsed"
  # class, plus fe-export-legend) so the export CSS places it, opened, in the
  # reserved right margin -- matching the forecast-plot export.
  float_legend <- htmltools::div(
    id             = float_legend_id,
    class          = if(isTRUE(for_export)){
      "floating-legend float-legend fe-export-legend"
    }else{
      "floating-legend collapsed float-legend"
    },
    `data-plot-id` = plot_id,
    htmltools::div(
      class = "legend-header legend-drag",
      id    = drag_id,
      htmltools::span("Legend"),
      htmltools::tags$button(class = "legend-toggle", "\u25be")
    ),
    legend_body
  )

  #------------------------------------------------------------------------------#
  # Intro text -------------------------------------------------------------------
  #------------------------------------------------------------------------------#

  intro_text <- htmltools::div(
    style = "margin-bottom: 1em;",
    htmltools::tags$p(
      "The figure below overlays the most recent operational forecasts against",
      " the observed target data. The newest forecast is shown as a solid line",
      " with shaded prediction-interval bands; older forecasts are dashed with",
      " lighter bands. Use the floating legend to show or hide individual",
      " forecasts. Forecasts that track the observed line closely with narrow",
      " bands indicate consistent week-to-week performance, while persistent",
      " gaps or widening bands point to greater uncertainty."
    )
  )

  #------------------------------------------------------------------------------#
  # Rendering: single geography --------------------------------------------------
  #------------------------------------------------------------------------------#

  if(length(plotly_list) == 1){

    plot_block <- htmltools::div(
      htmltools::div(
        id           = wrap_id,
        `data-fs-id` = plot_id,
        class        = "fs-wrap no-eval-model",
        style        = "width:100%;display:flex;flex-direction:column;position:relative;",
        htmltools::div(
          class = "fs-body",
          style = "width:100%;flex:1 1 auto;min-height:0;position:relative;",
          float_legend,
          htmltools::div(
            style = "display:flex;justify-content:center;",
            htmltools::div(
              class = "plot-panel active-plot-panel",
              style = "display:block;",
              plotly_list[[1]]
            )
          )
        )
      )
    )

  }else{

    plot_block <- htmltools::tagList(

      htmltools::div(
        class = "geo-filter-row",
        htmltools::tags$select(
          id             = geo_select_id,
          `data-plot-id` = plot_id,
          lapply(seq_along(plotly_list), function(i){
            htmltools::tags$option(
              value             = i - 1,
              `data-has-params` = if(location_has_params[[i]]) "true" else "false",
              `data-hover-text` = hover_text_list[[i]],
              names(plotly_list)[i]
            )
          })
        )
      ),

      htmltools::div(
        id           = wrap_id,
        `data-fs-id` = plot_id,
        class        = "fs-wrap no-eval-model",
        style        = "width:100%;display:flex;flex-direction:column;position:relative;",
        htmltools::div(
          class = "fs-body",
          style = "width:100%;flex:1 1 auto;min-height:0;position:relative;",
          float_legend,
          htmltools::div(
            style = "display:flex;justify-content:center;",
            lapply(seq_along(plotly_list), function(i){
              htmltools::div(
                class = paste("plot-panel",
                              if(i == 1) "active-plot-panel" else NULL),
                style = if(i == 1) "display:block;" else "display:none;",
                plotly_list[[i]]
              )
            })
          )
        )
      )
    )

  }

  #------------------------------------------------------------------------------#
  # Return -----------------------------------------------------------------------
  #------------------------------------------------------------------------------#

  # An export (a single location was requested) returns only the plot block
  # (the fs-wrap), so write_forecast_html() / write_forecast_image() render it
  # like the forecast-plot section -- no heading, intro text, or dropdown. This
  # covers both HTML (interactive) and static exports; for_export only governs
  # the static layout adjustments in make_forecast_consistency_plot().
  #
  # NOTE: wrap in a tagList (with a spacer sibling) exactly as the forecast-plot
  # section does. Returning the bare plot_block div makes htmlwidgets treat the
  # plot as a top-level widget and render it standalone (as an iframe), so it
  # never serializes inline and the saved page loses the plot. The sibling
  # element forces normal inline embedding.
  if(!is.null(location)){
    return(htmltools::tagList(
      plot_block,
      htmltools::div(style = "margin-top: 0em;")
    ))
  }

  htmltools::tagList(
    htmltools::h1("Forecast Consistency"),
    intro_text,
    plot_block,
    htmltools::div(style = "margin-top: 0em;")
  )

}
