#' Format observed date ranges for a trend/phase table
#'
#' @keywords internal
#' @noRd
format_trend_phase_date_ranges <- function(phase_data, location, phase){

#------------------------------------------------------------------------------#
# Confirming phase dates are available ----------------------------------------
#------------------------------------------------------------------------------#
# About: Phase dates come from observed target periods. Duplicate dates created #
# by multiple horizons or forecast submissions are removed before display.     #
#------------------------------------------------------------------------------#

  required <- c("location", "phase", "target_end_date")
  if(is.null(phase_data) || !is.data.frame(phase_data) ||
     !all(required %in% names(phase_data))){
    return("")
  }

  dates <- phase_data[
    as.character(phase_data$location) == as.character(location) &
      as.character(phase_data$phase) == as.character(phase), , drop = FALSE
  ]

  if("is_transmission" %in% names(dates)){
    dates <- dates[dates$is_transmission %in% TRUE, , drop = FALSE]
  }

  if(inherits(dates$target_end_date, "Date")){
    dates$target_end_date <- as.Date(dates$target_end_date)
  }else if(inherits(dates$target_end_date, c("POSIXct", "POSIXlt"))){
    dates$target_end_date <- as.Date(dates$target_end_date)
  }else{
    dates$target_end_date <- suppressWarnings(
      as.Date(as.character(dates$target_end_date))
    )
  }
  dates <- dates[!is.na(dates$target_end_date), , drop = FALSE]
  if(nrow(dates) == 0L) return("")

#------------------------------------------------------------------------------#
# Formatting one range per season ---------------------------------------------
#------------------------------------------------------------------------------#
# About: Showing separate season ranges avoids implying that dates between two  #
# testing seasons were part of the same observed epidemic phase.                #
#------------------------------------------------------------------------------#

  if(!"season" %in% names(dates)) dates$season <- ""
  season_groups <- split(dates, as.character(dates$season))
  ranges <- lapply(season_groups, function(group){
    values <- sort(unique(group$target_end_date))
    start <- format(values[1], "%b %d, %Y")
    end <- format(values[length(values)], "%b %d, %Y")
    range <- if(length(values) == 1L) start else paste(start, end, sep = " &ndash; ")

    season <- unique(as.character(group$season))
    season <- season[!is.na(season) & nzchar(season)]
    if(length(season_groups) > 1L && length(season)){
      paste0(htmltools::htmlEscape(season[1]), ": ", range)
    }else{
      range
    }
  })

  paste0(unlist(ranges, use.names = FALSE), collapse = "<br>")
}
