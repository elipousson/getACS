acs_surveys <- c("acs1", "acs3", "acs5", "acsse")

decennial_datasets <- c(
  "sf1", "sf2", "sf3", "sf4", "pl", "ddhca",
  "as", "gu", "mp", "vi", "dhc", "dp",
  "dpas", "dpgu", "dpmp", "dpvi",
  "dhcvi", "dhcgu", "dhcvi", "dhcas",
  "sf2profile", "sf3profile",
  "sf4profile", "aian", "aianprofile",
  "cd110h", "cd110s", "cd110hprofile", "cd110sprofile", "sldh",
  "slds", "sldhprofile", "sldsprofile", "cqr",
  "cd113", "cd113profile", "cd115", "cd115profile", "cd116",
  "plnat", "cd118"
)

geographies_drop_state <- c(
  "us", "region", "division", "metropolitan/micropolitan statistical area",
  "metropolitan statistical area/micropolitan statistical area", "cbsa",
  "urban area", "zip code tabulation area", "zcta"
)

geographies_require_state <- c(
  "county subdivision", "tract", "block group", "cbg",
  "block", "school district (elementary)",
  "school district (secondary)", "school district (unified)",
  "state legislative district (upper chamber)",
  "state legislative district (upper chamber)",
  "state legislative district (lower chamber)",
  "voting district"
)

geographies_allow_county <- c(
  "county", "county subdivision", "tract",
  "block group", "cbg", "block"
)

geographies_require_county <- "block"
