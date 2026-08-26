# Generate metrocast_locations package data from the raw CSV.
# Run this script whenever the MetroCast location crosswalk is updated.
# Source: https://github.com/reichlab/flu-metrocast/blob/main/auxiliary-data/locations.csv

metrocast_locations <- utils::read.csv(
  "data-raw/metrocast_locations.csv",
  colClasses = c(original_location_code = "character"),  # preserve "All" and codes
  stringsAsFactors = FALSE
)

usethis::use_data(metrocast_locations, overwrite = TRUE)
