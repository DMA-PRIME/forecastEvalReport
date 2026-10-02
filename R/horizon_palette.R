#' Named color palette for forecast horizons
#'
#' Uses the ColorBrewer "Set1" colors (hard-coded to avoid depending on
#' RColorBrewer). With more than nine horizons the palette is interpolated.
#'
#' @param horizons Vector of horizon values.
#' @return Character vector of colors named by `as.character(horizons)`.
#' @keywords internal
#' @noRd
horizon_palette <- function(horizons){

  set1 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
            "#FFFF33", "#A65628", "#F781BF", "#999999")
  n    <- length(horizons)

  cols <- if(n <= length(set1)){
    set1[seq_len(n)]
  }else{
    grDevices::colorRampPalette(set1)(n)
  }

  stats::setNames(cols, as.character(horizons))
}
