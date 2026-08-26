# Generate hubverse_locations package data from the raw CSV.
# Run this script whenever the location crosswalk is updated.
# Used by all hubverse-format validators (FluSight, COVIDHub, RSVHub, etc.).

hubverse_locations <- utils::read.csv(
  "data-raw/flusight_locations.csv",
  colClasses = c(location = "character"),  # preserve leading zeros
  stringsAsFactors = FALSE
)

usethis::use_data(hubverse_locations, overwrite = TRUE)
