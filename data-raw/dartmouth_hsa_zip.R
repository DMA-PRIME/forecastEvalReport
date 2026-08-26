# Generate dartmouth_hsa_zip package data from the Dartmouth Atlas
# crosswalk file. Run this script whenever the crosswalk is updated.
# Source: Dartmouth Atlas of Health Care, ZipHsaHrr19.csv (2019 release).

dartmouth_hsa_zip <- utils::read.csv(
  "data-raw/dartmouth_hsa_zip.csv",
  colClasses = c(
    zipcode19 = "character",  # preserve leading zeros
    hsanum    = "character",  # preserve leading zeros if any
    hrrnum    = "character"
  ),
  stringsAsFactors = FALSE
)

usethis::use_data(dartmouth_hsa_zip, overwrite = TRUE)
