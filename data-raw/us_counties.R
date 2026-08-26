# Generate us_counties package data from the FIPS master file.
# Filters out state-level and national-level rows; keeps only counties
# and county-equivalents (boroughs, parishes, census areas, etc.).
# Source: Census Bureau FIPS codes master file.

raw <- utils::read.csv(
  "data-raw/us_counties.csv",
  colClasses     = c(fips = "character"),  # preserve leading zeros
  stringsAsFactors = FALSE,
  na.strings     = c("NA", "")
)

# Keep only county-level rows (drop national & state aggregates where state is NA)
us_counties <- raw[!is.na(raw$state), ]

# Re-sort by FIPS for predictability
us_counties <- us_counties[order(us_counties$fips), ]

# Reset row numbers
rownames(us_counties) <- NULL

usethis::use_data(us_counties, overwrite = TRUE)
