race_iteration <- tibble::tribble(
  ~code, ~group, ~label,
  "A", "White Alone", "White",
  "B", "Black or African American Alone", "Black",
  "C", "American Indian and Alaska Native Alone", "American Indian/Alaskan Native",
  "D", "Asian Alone", "Asian",
  "E", "Native Hawaiian and Other Pacific Islander Alone", "Native Hawaiian/Pacific Islander",
  "F", "Some Other Race Alone", "Other",
  "G", "Two or More Races", "Two or more",
  "H", "White Alone, Not Hispanic or Latino", "Non-Hispanic White",
  "I", "Hispanic or Latino", "Hispanic/Latino"
)

usethis::use_data(race_iteration, overwrite = TRUE)
