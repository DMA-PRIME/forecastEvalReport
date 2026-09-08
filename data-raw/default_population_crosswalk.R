# Build the package's default population crosswalk.
#
# Hubverse rows come from the package's FluSight 2024-2025 location reference.
# South Carolina county populations come from the U.S. Census Bureau Vintage
# 2023 county estimates. Public-health region membership follows the South
# Carolina Department of Public Health four-region map.

hubverse_locations <- utils::read.csv(
  "data-raw/flusight_locations.csv",
  colClasses = c(location = "character"),
  stringsAsFactors = FALSE
)

county_source <- utils::read.csv(
  "data-raw/co-est2023-alldata.csv",
  colClasses = c(STATE = "character", COUNTY = "character"),
  stringsAsFactors = FALSE
)

sc_counties <- county_source[
  county_source$STNAME == "South Carolina" & county_source$SUMLEV == 50,
  c("STATE", "COUNTY", "CTYNAME", "POPESTIMATE2023")
]

region_membership <- list(
  Lowcountry = c(
    "Allendale", "Bamberg", "Beaufort", "Berkeley", "Calhoun",
    "Charleston", "Colleton", "Dorchester", "Hampton", "Jasper",
    "Orangeburg"
  ),
  Midlands = c(
    "Aiken", "Barnwell", "Chester", "Edgefield", "Fairfield", "Kershaw",
    "Lancaster", "Lexington", "Newberry", "Richland", "Saluda", "York"
  ),
  `Pee Dee` = c(
    "Clarendon", "Chesterfield", "Darlington", "Dillon", "Florence",
    "Georgetown", "Horry", "Lee", "Marion", "Marlboro", "Sumter",
    "Williamsburg"
  ),
  Upstate = c(
    "Abbeville", "Anderson", "Cherokee", "Greenville", "Greenwood",
    "Laurens", "McCormick", "Oconee", "Pickens", "Spartanburg", "Union"
  )
)

sc_counties$county_name <- sub(" County$", "", sc_counties$CTYNAME)
sc_counties$region <- NA_character_

for(region_name in names(region_membership)){
  sc_counties$region[
    sc_counties$county_name %in% region_membership[[region_name]]
  ] <- region_name
}

if(anyNA(sc_counties$region)){
  stop("Every South Carolina county must be assigned to a DPH region.")
}

hub_rows <- data.frame(
  location = hubverse_locations$location_name,
  population = as.numeric(hubverse_locations$population),
  geography_type = ifelse(hubverse_locations$location == "US", "national", "state"),
  location_code = as.character(hubverse_locations$location),
  abbreviation = as.character(hubverse_locations$abbreviation),
  state = ifelse(hubverse_locations$location == "US", NA_character_,
                 as.character(hubverse_locations$abbreviation)),
  region = NA_character_,
  population_year = 2023L,
  source = "Hubverse FluSight 2024-2025 location reference",
  aliases = NA_character_,
  stringsAsFactors = FALSE
)

# Use the friendly country name while retaining the official Hubverse aliases.
hub_rows$aliases[hub_rows$location == "US"] <- "United States|United States of America"

county_rows <- data.frame(
  location = sc_counties$CTYNAME,
  population = as.numeric(sc_counties$POPESTIMATE2023),
  geography_type = "county",
  location_code = paste0(sc_counties$STATE, sc_counties$COUNTY),
  abbreviation = NA_character_,
  state = "SC",
  region = sc_counties$region,
  population_year = 2023L,
  source = "U.S. Census Bureau Vintage 2023 county population estimates",
  aliases = sc_counties$county_name,
  stringsAsFactors = FALSE
)

region_rows <- do.call(rbind, lapply(names(region_membership), function(region_name){
  members <- sc_counties$county_name %in% region_membership[[region_name]]
  data.frame(
    location = region_name,
    population = sum(as.numeric(sc_counties$POPESTIMATE2023[members])),
    geography_type = "region",
    location_code = NA_character_,
    abbreviation = NA_character_,
    state = "SC",
    region = region_name,
    population_year = 2023L,
    source = paste(
      "U.S. Census Bureau Vintage 2023 estimates aggregated to",
      "South Carolina DPH regions"
    ),
    aliases = paste0(region_name, " Region"),
    stringsAsFactors = FALSE
  )
}))

# Add the common unspaced spelling as an alias for Pee Dee.
region_rows$aliases[region_rows$location == "Pee Dee"] <-
  "Pee Dee Region|PeeDee"

default_population_crosswalk <- rbind(hub_rows, region_rows, county_rows)
row.names(default_population_crosswalk) <- NULL

# The county estimates should reproduce the Hubverse South Carolina total.
sc_total <- sum(county_rows$population)
hub_sc_total <- hub_rows$population[hub_rows$abbreviation == "SC"]
if(!identical(as.numeric(sc_total), as.numeric(hub_sc_total))){
  stop("South Carolina county populations do not match the Hubverse SC total.")
}

save(
  default_population_crosswalk,
  file = "data/default_population_crosswalk.rda",
  compress = "xz"
)
