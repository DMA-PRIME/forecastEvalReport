#------------------------------------------------------------------------------#
#                                                                              #
#      Building introductory text for the peak phase evaluation section        #
#                                                                              #
#------------------------------------------------------------------------------#
# About:                                                                       #
#                                                                              #
# This function builds the introductory HTML for the peak evaluation section   #
# of the report. It defines the peak in plain language and frames the          #
# evaluation as three measures assessed at each forecast horizon.              #
#                                                                              #
# The peak is the single highest week of the season (the observed global       #
# maximum). The section reports three measures at each forecast horizon: peak  #
# timing, peak magnitude, and peak-week accuracy. The resolved outcome label is#
# woven into the wording when supplied.                                        #
#                                                                              #
# When multiple locations are present the navigation text speaks to several    #
# geographies; single-location reports use a simplified description.           #
#                                                                              #
# Centralizing this text keeps the explanatory language consistent and         #
# accurate across report types and geographies.                                #
#------------------------------------------------------------------------------#
#                       Author: Amanda Bleichrodt                              #
#------------------------------------------------------------------------------#
#                       Last Updated: 2026-06-30                               #
#------------------------------------------------------------------------------#
build_peak_phase_intro <- function(multi_location = TRUE,
                                   outcome        = NULL){

  ##################################
  # Outcome wording (data-driven)  #
  ##################################
  # Use the resolved outcome label only when it is meaningful
  has_outcome <- !is.null(outcome) && length(outcome) == 1 &&
    !is.na(outcome) && nzchar(outcome) && !identical(outcome, "Observed")

  # Outcome slot for the definition sentence
  week_phrase <- if(has_outcome){
    sprintf("the single highest week of %s in a season", outcome)
  } else {
    "the single highest week of a season"
  }

  ##############################################
  # Building the definition paragraph          #
  ##############################################
  definition_html <- sprintf(
    'The <strong>peak</strong> is %s: the week whose observed value reaches the
     season&rsquo;s maximum. This section measures how well the model forecast
     that peak at each <strong>forecast horizon</strong> &mdash; the number of
     weeks ahead a forecast was made &mdash; from the earliest lead times
     through the nowcast (the estimate for the current week). Four measures are
     reported: <strong>peak timing</strong> (how far the predicted peak week
     fell from the true peak week) and three height comparisons that differ only
     in which week anchors them &mdash; <strong>peak-to-peak magnitude</strong>
     (predicted peak height vs the observed peak height, dates aside),
     <strong>magnitude at the predicted peak</strong> (predicted peak vs what
     was actually observed that same week), and <strong>magnitude at the
     observed peak</strong> (the forecast for the true peak week vs the observed
     peak).',
    week_phrase)

  ##############################################
  # Navigation text: multi vs single location  #
  ##############################################
  navigate_text <- if(multi_location){

    'Use the <strong>dropdown</strong> below to select a geography. Results are
     organized by <strong>forecast horizon</strong>, with peak timing plus
     three magnitude comparisons (peak-to-peak, at the predicted peak, and at
     the observed peak) shown for each.'

  } else {

    'Results are organized by <strong>forecast horizon</strong>, with peak
     timing plus three magnitude comparisons (peak-to-peak, at the predicted
     peak, and at the observed peak) shown for each.'

  }

  ####################################
  # Generating the introduction text #
  ####################################
  htmltools::HTML(sprintf('
  <div class="section-intro">
    <p style="margin-bottom:2rem;">
      %s
    </p>

    <div style="background:#f4f7fc;border-left:4px solid #522D80;border-radius:4px;
                padding:10px 15px;margin-bottom:1.5rem;">
      <span style="font-size:13px;font-weight:700;color:#522D80;display:block;
                   margin-bottom:4px;text-transform:uppercase;letter-spacing:0.5px;">To Navigate</span>
      <p style="font-size:15px;color:#555;line-height:1.6;margin:0;">
        %s
      </p>
    </div>
  </div>
  ', definition_html, navigate_text))

}
