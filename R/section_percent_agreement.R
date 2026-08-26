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
#' filtered by the raw `location` code, which is what `make_percent_agreement_plot()`
#' expects.
#'
#' @param percentAgreement.data Output of `percentAgreementCalculation()` — the
#'   evaluation frame with row-level `per_agreement`, `is_transmission`,
#'   `horizon`, `location`, and (when available) `location_display`.
#' @param eval_meta Metadata list from `extract_evaluation_data()`. Used for the
#'   testing-data presence gate and as a fallback location label source.
#' @param outcome Character label for the outcome (right axis / hover). When
#'   `NULL`, the clean outcome name is taken from `variables_crosswalk`, then
#'   `eval_meta$outcome`, then a generic fallback.
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
section_percent_agreement <- function(percentAgreement.data,
                                      eval_meta,
                                      outcome             = NULL,
                                      variables_crosswalk = NULL,
                                      eval_config         = NULL) {

#------------------------------------------------------------------------------#
# Testing data must be present -------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section checks to make sure the testing data and percent         #
# agreement data is available prior to running the remainder of the script. If #
# the testing data or percent agreement metrics are not available the script   #
# does not run.                                                                #
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

  # Rendering nothing when no percent agreement metrics are
  if(is.null(percentAgreement.data) ||
     !is.data.frame(percentAgreement.data) ||
     nrow(percentAgreement.data) == 0) return(invisible(NULL))

#------------------------------------------------------------------------------#
# Resolving inputs -------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section extracts the information needed from the evaluation file #
# including the outcome label. It also prepares the other inputs such as the   #
# "non-transmission" months and text around the exclusion of those data        #
# from the calculations.                                                       #
#------------------------------------------------------------------------------#

  #####################################################################
  # Checking if the evaluation configuration file needs to be created #
  #####################################################################
  if(is.null(eval_config)) eval_config <- create_evaluation_config()

  ###############################################
  # Extracting the outcome: No outcome provided #
  ###############################################
  if(is.null(outcome)){

    # Setting the outcome to a NA
    outcome <- NA_character_

    #################################################
    # Pulling the clean outcome from the cross walk #
    #################################################
    if(!is.null(variables_crosswalk) && is.data.frame(variables_crosswalk) &&
       all(c("variable_type", "clean_name_full") %in% names(variables_crosswalk))){

      # Extracting the outcome rows
      outcome_rows <- variables_crosswalk[
        !is.na(variables_crosswalk$variable_type) &
          variables_crosswalk$variable_type == "outcome", ]

      ######################################
      # Triggered if outcome row available #
      ######################################
      if(nrow(outcome_rows) > 0){

        # Pulling the clean outcome names
        clean_names <- unique(outcome_rows$clean_name_full)

        # Validating the clean names
        clean_names <- clean_names[
          !is.na(clean_names) &
            nchar(trimws(clean_names)) > 0 &
            clean_names != "USER: provide a definition"]

        # Saving the clean name if all checks passed
        if(length(clean_names) > 0) outcome <- paste(clean_names, collapse = ", ")
      }
    }

    #############################
    # Handling missing outcomes #
    #############################
    if(is.na(outcome) || !nzchar(outcome)){

      # Using generic "Observed" legend entry
      outcome <- if(!is.null(eval_meta$outcome)) paste(eval_meta$outcome, collapse = ", ") else "Observed"

    }
  }

  ###############################################
  # Non-transmission month label for the prose  #
  ###############################################

  # Pulling the non-transmission months
  nt <- sort(unique(eval_config$non_transmission_months))

  # Creating the label: No Months Provided
  nt_label <- if(length(nt) == 0){"none"

  # Creating the label: One Month Provided
  }else if(identical(as.integer(nt), as.integer(min(nt):max(nt)))){

    # Creating the label
    paste0(month.name[min(nt)], " \u2013 ", month.name[max(nt)])

  # Creating the label: Multiple Months Provided
  }else{paste(month.name[nt], collapse = ", ")}

  ####################################################
  # Creating the footnote for 'No Evaluation Period' #
  ####################################################

  # Pulling months included in data
  data_months  <- unique(as.integer(format(as.Date(percentAgreement.data$target_end_date), "%m")))

  # Checking what no-transmission months included in data
  show_no_eval <- length(nt) > 0 && any(nt %in% data_months)

  # Creating the footnote: No-transmission months included
  no_eval_sentence <- if(show_no_eval){

    # Message to show
    paste0(' Periods marked <strong>No Evaluation</strong> (', nt_label,
           ') are excluded from all summaries.')

  # No footnote to show
  }else{''}

#------------------------------------------------------------------------------#
# Resolving location codes and display labels ----------------------------------
#------------------------------------------------------------------------------#
# About: This section filters the data by by raw `location`; the DOM (dropdown,#
# panels, table) is keyed by the display label so all three stay in sync.      #
# Labels come from location_display, then eval_meta$locations, then fall back  #
# to the raw code.                                                             #
#------------------------------------------------------------------------------#

  #####################################
  # Pulling the unique location codes #
  #####################################
  loc_codes <- sort(unique(percentAgreement.data$location))

  #######################################
  # Creating the location display lavel #
  #######################################
  resolve_label <- function(cd){

    # Starting with a NA label holder
    lab <- NA_character_

    ####################################
    # Display location names available #
    ####################################
    if("location_display" %in% names(percentAgreement.data)){

      # Unique location names
      cand <- unique(percentAgreement.data$location_display[percentAgreement.data$location == cd])

      # Cleaning up location name list
      cand <- cand[!is.na(cand) & nzchar(cand)]

      # Pulling only first location if more than one
      if(length(cand) >= 1) lab <- cand[1]

    }

    #####################################################
    # Display location names available: Evaluation file #
    #####################################################
    if(is.na(lab) && !is.null(eval_meta$locations) && cd %in% names(eval_meta$locations)){

      # Pulling location label
      lab <- unname(eval_meta$locations[[cd]])
    }

    ##################################################
    # Display location name not available: Using Raw #
    ##################################################
    if(is.na(lab) || !nzchar(lab)) lab <- cd

    ################################
    # Returning the location label #
    ################################
    lab

  }

  #####################################################
  # Applying location label function to all locations #
  #####################################################
  loc_labels  <- vapply(loc_codes, resolve_label, character(1))

  ######################################################
  # Pulling number of locations: Interactive component #
  ######################################################

  # Number of locations
  n_loc       <- length(loc_codes)

  # Keeping interactive if only more than one location
  interactive <- n_loc > 1

#------------------------------------------------------------------------------#
# Intro paragraph --------------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the introduction paragraph to show for the %     #
# agreement drop down. Essentially, its goal is to provide a brief into to     #
# what the user should expect to see in the percent agreement drop down.       #
#------------------------------------------------------------------------------#

  #######################################
  # Creating the introduction paragraph #
  #######################################
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
# About: This section creates the navigation call out. The goal of this        #
# section is to provide clear instructions for users as they navigate the      #
# percent agreement drop down.                                                 #
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
# About: This section creates the section header for above the figure. It is   #
# to make it clear that we show the percent agreement over time and by         #
# forecast horizon.                                                            #
#------------------------------------------------------------------------------#

  ##############################################
  # Creating the header percent agreement plot #
  ##############################################
  header_html <- htmltools::tagList(
    htmltools::div(style = "margin-top: 1.5em;"),
    htmltools::tags$h3(htmltools::tags$strong("Percent Agreement Over Time by Forecast Horizon")),
    htmltools::div(style = "margin-top: 2em;")
  )

#------------------------------------------------------------------------------#
# Geography dropdown (multi-location only) -------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the geography drop down if and only if more      #
# than one location is included. Therefore, users have the ability to shift    #
# through the individual figures for all locations included within the         #
# provided testing file.                                                       #
#------------------------------------------------------------------------------#

  ##############################################
  # Creating the drop down: Multiple Locations #
  ##############################################
  dropdown_html <- if(n_loc > 1){

    # Creating the drop down
    htmltools::tagList(
      build_geo_dropdown(loc_labels, select_id = "geoSelect_PA"),
      htmltools::div(style = "margin-top: 2em;")
    )

  ############################################
  # No drop down to be created: One location #
  ############################################
  }else{htmltools::div(style = "margin-top: 1.5em;")}

#------------------------------------------------------------------------------#
# Building the per-location plots ----------------------------------------------
#------------------------------------------------------------------------------#
# About: This section creates the per-location plots to show in the drop       #
# down. Essentially, they are controlled by a generic wrap id, and then a      #
# lapply is used to apply the make_percent_agreement_plot() to all locations   #
# included in the evaluation file.                                             #
#------------------------------------------------------------------------------#

  #################################
  # Generic wrap id for the plots #
  #################################
  wrap_id <- "wrap-pctAgreePlot"

  ###########################################
  # Building the list of per-location plots #
  ###########################################
  plotlyList <- setNames(

    # Applying the build function to all locations
    lapply(loc_codes, function(loc){
      make_percent_agreement_plot(
        data    = percentAgreement.data,
        loc     = loc,
        outcome = outcome
      )
    }),

    # Labels for the plot
    loc_labels
  )

#------------------------------------------------------------------------------#
# Rendering the figure (single vs multiple locations) --------------------------
#------------------------------------------------------------------------------#
# About: This section renders the figures either for the list of plots built   #
# in the above section or the first element of the above generated plot(s)     #
# given there will be only one plot in the list.                               #
#------------------------------------------------------------------------------#

  ##########################################
  # Building the single plot: One location #
  ##########################################
  if(n_loc == 1){

    ############################################
    # Rendering the plot for a single location #
    ############################################
    plot_block <- htmltools::div(

      # Style for plot container
      style = "display: flex; justify-content: center; width: 100%;",

      # Rendering the single plot
      htmltools::div(
        class = "plot-panel",
        style = "display: flex; justify-content: center;",
        plotlyList[[1]]
      )

    )

  #######################################################
  # Building the multiple plots: More than one location #
  #######################################################
  }else{

    ##################################
    # Creating the tag list of plots #
    ##################################
    plot_block <- htmltools::tagList(

      ##########################
      # One panel per location #
      ##########################
      htmltools::div(

        # ID to link plot and table
        id    = wrap_id,

        # Style for plot container
        style = "width: 100%; display: flex; justify-content: center;",

        # Rendering each per-location plot
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

      ###################################
      # Building the geography-selector #
      ###################################
      htmltools::tags$script(htmltools::HTML(sprintf('
        (function() {

          function applyPctAgreePlotLocation(locName) {
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
            if (typeof syncPctTableToPlot === "function") syncPctTableToPlot(locName);
          }

          var sel = document.getElementById("geoSelect_PA");
          if (sel) {
            sel.addEventListener("change", function() {
              var opt = this.options[this.selectedIndex];
              applyPctAgreePlotLocation(opt.getAttribute("data-location-name"));
            });
          }

          window.addEventListener("load", function() {
            var sel = document.getElementById("geoSelect_PA");
            if (sel && sel.options[0]) {
              applyPctAgreePlotLocation(sel.options[0].getAttribute("data-location-name"));
            }
          });

        })();
      ', wrap_id)))
    )
  }

#------------------------------------------------------------------------------#
# Building the location x horizon table (Median + Range) -----------------------
#------------------------------------------------------------------------------#
# About: This section builds the table that shows the median and range of      #
# row level percent agreement values. This is only computed for the rows/dates #
# that fall within the transmission season. Any empty location and horizon     #
# groups render a dash instead of crashing on an empty vector.                 #
#------------------------------------------------------------------------------#

  ################################
  # Pulling and sorting horizons #
  ################################
  horizons <- sort(unique(percentAgreement.data$horizon))

  ###########################################
  # Safe summary on a possibly-empty vector #
  ###########################################
  safe_round <- function(x, fn){

    # Filtering out NA rows
    x <- x[!is.na(x)]

    # Returning NA if all rows NA
    if(length(x) == 0) return(NA_real_)

    # Rounding to one digit if returns value
    round(fn(x), 1)

  }

  ################################
  # Cell builder: Median & Range #
  ################################
  metric_cell <- function(vals, is_overall = FALSE){

    ############################
    # Preparing the statistics #
    ############################

    # Pulling and rounding the median
    med <- safe_round(vals, stats::median)

    # Pulling and rounding the min value
    lo  <- safe_round(vals, min)

    # Pulling and rounding the max value
    hi  <- safe_round(vals, max)

    #############################
    # Preparing the table style #
    #############################

    # Background color
    bg        <- if(is_overall) "background: #f7f4fc;" else "border-right: 1px solid #e0e0e0;"

    # Header color
    med_color <- if(is_overall) "#522D80" else "#222"

    # Value color
    rng_color <- if(is_overall) "#9B85C8" else "#555"

    # Value text size
    med_txt <- if(is.na(med)) "\u2014" else paste0(med, "%")

    # Range text size
    rng_txt <- if(is.na(med)) "" else paste0("(", lo, "% \u2013 ", hi, "%)")

    # Handling other text
    dval    <- if(is.na(med)) "" else med

    ############################
    # Applying the table style #
    ############################
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

  ########################
  # One row per location #
  ########################
  all_rows <- paste0(

    # Applying style to single location rows
    sapply(seq_len(n_loc), function(i){

      # Pulling the location code
      code   <- loc_codes[i]

      # Pulling the location label
      label  <- loc_labels[i]

      # Pulling the border style
      border <- if(i < n_loc) "border-bottom: 1px solid #e0e0e0;" else ""

      # Filtering for in-transmission & single location
      loc_data <- percentAgreement.data %>%
        dplyr::filter(location == code, is_transmission == TRUE)

      ##############################
      # Creating the Horizon cells #
      ##############################
      horizon_cells <- paste0(

        # Cycling through horizons
        sapply(horizons, function(h){

          # Applying above function to create row
          metric_cell(loc_data$per_agreement[loc_data$horizon == h])
        }),

        collapse = ""

      )

      ################
      # Overall cell #
      ################
      overall_cell <- metric_cell(loc_data$per_agreement, is_overall = TRUE)

      #################
      # Location cell #
      #################
      loc_cell <- paste0('
        <td data-location="', label, '" style="padding: 14px 16px; font-size: 14px;
            font-weight: 700; color: #555; text-align: center; vertical-align: middle;
            width: 120px; border-right: 1px solid #e0e0e0; white-space: nowrap;">',
            label, '</td>')

      #########################
      # Creating the full row #
      #########################
      paste0('<tr style="', border, '">', loc_cell, horizon_cells, overall_cell, '</tr>')

    }),

    collapse = ""

  )

  ###########################
  # Creating Column headers #
  ###########################
  horizon_headers <- paste0(

    # Cycling through horizons
    sapply(horizons, function(h){

      # Creating the headers
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

  ###################################################
  # Table interactive indicator: Swithing locations #
  ###################################################
  show_search <- interactive

#------------------------------------------------------------------------------#
# Rendering the table ----------------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section renders the one row per location table. For              #
# multi-location reports a minimal script filters the visible row to match the #
# geography dropdown.                                                          #
#------------------------------------------------------------------------------#

  #########################################################################
  # Syncing table with user-selected location: Multiple locations present #
  #########################################################################
  sync_script <- if(interactive){
    '<script>
    (function() {
      function showPctRow(locName) {
        var tbody = document.querySelector("#pctAgreementTable tbody");
        if (!tbody) return;
        Array.from(tbody.querySelectorAll("tr")).forEach(function(row) {
          var lc = row.querySelector("td[data-location]");
          if (lc) row.style.display = (lc.getAttribute("data-location") === locName) ? "" : "none";
        });
      }
      // Exposed so the plot panel-switch JS can drive the table
      window.syncPctTableToPlot = function(locName) { showPctRow(locName); };
      function initPctTable() {
        var sel = document.getElementById("geoSelect_PA");
        if (!sel) return;
        sel.addEventListener("change", function() {
          var opt = this.options[this.selectedIndex];
          if (opt) showPctRow(opt.getAttribute("data-location-name"));
        });
        var opt0 = sel.options[sel.selectedIndex] || sel.options[0];
        if (opt0) showPctRow(opt0.getAttribute("data-location-name"));
      }
      if (document.readyState === "loading") {
        document.addEventListener("DOMContentLoaded", initPctTable);
      } else { initPctTable(); }
    })();
    </script>'

  ###########################################
  # No syncing needed: One location present #
  ###########################################
  }else{''}

  #############################
  # Rendering the final table #
  #############################
  table_html <- htmltools::HTML(paste0('
<div style="font-family:sans-serif;padding:1rem 0;overflow-x:auto;">
', sync_script, '
  <table id="pctAgreementTable" style="width:100%;border-collapse:collapse;
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
# About: This section creates the detailed metrics section that shows in the   #
# drop down for percent agreement. This is essentially to ensure that the user #
# knows exactly how the metrics they seeing are calculated.                    #
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

  #######################################
  # No text needed: No evaluation model #
  #######################################
  }else{''}

  ###############################################################
  # Creating the remainder of the methods for percent agreement #
  ###############################################################
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

  ###################################################
  # Rendering the detail methods internal accordian #
  ###################################################
  methods_accordion <- htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Detailed Methods (Percent Agreement)")),
    htmltools::div(class = "accordion-body", methods_html)
  )

#------------------------------------------------------------------------------#
# Location-to-Location Comparison accordion (multi-location only) --------------
#------------------------------------------------------------------------------#
# About: This section creates the table where all locations shown at once,     #
# sortable and searchable, starting sorted best-to-worst (highest overall      #
# median agreement first). It is only meaningful when more than one location   #
# is present, so it is omitted for single-location reports. It uses its own    #
# table id and sort function so it does not interfere with the one-location-at #
# -a-time table above.                                                         #
#------------------------------------------------------------------------------#

  #####################################################################
  # Creating the location-comparison accordion: Multi-location report #
  #####################################################################
  compare_accordion <- if(interactive){

    ###############################
    # Sortable comparison headers #
    ###############################
    compare_horizon_headers <- paste0(

      # Cycling through horizons
      sapply(seq_along(horizons), function(i){

        # Indexed header
        h <- horizons[i]

        # Table headers
        paste0('
          <th class="sum-th" onclick="sortPctCompare(', i, ', \'num\')"
              style="border-right: 1px solid #e0e0e0;">
            <div style="display:inline-flex;align-items:center;justify-content:center;gap:4px;line-height:1;">
              Horizon ', h, ' <span style="font-size:10px;">&#8597;</span>
            </div>
            <div class="sum-th-sub" style="color:#C9B8E8;">Median (Range)</div>
          </th>')
      }),
      collapse = ""
    )

    ########################################
    # Intro for location-compare drop down #
    ########################################
    compare_body <- htmltools::HTML(paste0('
    <p style="font-size: 14px; line-height: 1.6; color: #444; margin: 0 0 1rem 0;">
      Compare percent agreement across all locations at once. The table starts sorted from
      best to worst by overall median agreement; click any column to re-sort, or use the
      search box to find a specific location.
    </p>

    <div style="font-family:sans-serif;padding:0.5rem 0;overflow-x:auto;">
      <script>
        var pctCmpSortDir = {};
        function sortPctCompare(colIndex, type) {
          var tbody = document.querySelector("#pctCompareTable tbody");
          if (!tbody) return;
          var rows = Array.from(tbody.querySelectorAll("tr"));
          var asc  = pctCmpSortDir[colIndex] !== true;
          pctCmpSortDir[colIndex] = asc;
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
          pctCmpSortDir[overallCol] = true;   // flip so first sort is descending = best first
          sortPctCompare(overallCol, "num");
        });
      </script>

      <table id="pctCompareTable" style="width:100%;border-collapse:collapse;
                                          border-top:1px solid #333;border-bottom:1px solid #333;">
        <thead>
          <tr style="border-bottom:1px solid #333;">
            <th class="sum-th" onclick="sortPctCompare(0, \'text\')"
                style="border-right: 1px solid #e0e0e0; width: 120px;">
              Location
              <br/>
              <input type="text" id="pctCompareSearch" placeholder="Search..."
                onclick="event.stopPropagation();"
                onkeyup="
                  var val = this.value.toLowerCase();
                  var rows = document.querySelectorAll(\'#pctCompareTable tbody tr\');
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
            '<th class="sum-th" onclick="sortPctCompare(', length(horizons) + 1, ', \'num\')"
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

    ###############################################
    # Rendering the location-comparison accordian #
    ###############################################
    htmltools::tags$details(
      class = "accordion",
      htmltools::tags$summary(htmltools::tags$strong("Location-to-Location Comparison")),
      htmltools::div(class = "accordion-body", compare_body)
    )

  ##############################################################
  # No location comparison accordion needed: only one location #
  ##############################################################
  }else{NULL}

#------------------------------------------------------------------------------#
# Assembling the full drop-down ------------------------------------------------
#------------------------------------------------------------------------------#
# About: This section assembles the full drop down for percent agreement,      #
# including the text, headers, table, and figures. This is returned to the     #
# main report script.                                                          #
#------------------------------------------------------------------------------#

  #########################################
  # Assembling HTML for percent agreement #
  #########################################
  htmltools::tags$details(
    class = "accordion",
    htmltools::tags$summary(htmltools::tags$strong("Percent Agreement")),
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
