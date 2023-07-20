## code to prepare `tigerweb_geo_index` dataset goes here

tigerweb_geo_index <- esri2sf::esriIndex(
  "https://tigerweb.geo.census.gov/arcgis/rest/services",
  recurse = TRUE
)

usethis::use_data(tigerweb_geo_index, overwrite = TRUE)
