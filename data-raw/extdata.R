## code to prepare `extdata` dataset goes here

table_metadata <- get_acs_metadata("acs5", 2021, metadata = "table")

readr::write_csv(
  table_metadata,
  file = file.path("inst/extdata", "2021_census_table_metadata.csv")
)

column_metadata <- get_acs_metadata("acs5", 2021, metadata = "column")

readr::write_csv(
  column_metadata,
  file = file.path("inst/extdata", "2021_census_column_metadata.csv")
)
