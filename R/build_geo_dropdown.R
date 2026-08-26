#' Build a geography selection dropdown for interactive reports
#'
#' Creates an HTML `<select>` menu for choosing among multiple locations in an
#' interactive report. Each geography becomes a zero-indexed `<option>` whose
#' value aligns with the panel-index convention used elsewhere, and whose
#' `data-location-name` attribute carries the display label so downstream
#' JavaScript (panel switching, table sync) can match selections by name. The
#' select is wrapped in a styled `.geo-filter-row` container for consistent
#' presentation across report components.
#'
#' @param geographies Character vector of geography display labels, one per
#'   panel, in panel order.
#' @param select_id Character. The `id` attribute applied to the `<select>`;
#'   referenced by the section's panel-switch and table-sync JavaScript.
#'   Default "geoSelect".
#' @param label Character. Optional label markup rendered before the select.
#'   Default "" (no label).
#'
#' @return Rendered HTML via [htmltools::HTML()] containing the dropdown.
#'
#' @keywords internal
#' @noRd
build_geo_dropdown <- function(geographies, select_id = "geoSelect", label = "") {

  ########################
  # Building the options #
  ########################
  options_html <- paste0(

    # Cycling through all geographies
    sapply(seq_along(geographies), function(i) {

      # Creating the options
      sprintf('<option value="%d" data-location-name="%s">%s</option>',
              i - 1,  # 0-indexed to match panel-index convention
              htmltools::htmlEscape(geographies[i]),
              htmltools::htmlEscape(geographies[i]))

    }), collapse = "\n")

  ########################################################
  # Building the HTML version of the geography drop down #
  ########################################################
  htmltools::HTML(sprintf('
    <div class="geo-filter-row">
      %s
      <select id="%s" class="geo-select">
        %s
      </select>
    </div>
  ', "", select_id, options_html))

}
