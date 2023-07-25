# bind_acs_geographies <- function(data,
#                                  geographies,
#                                  geography_col = "NAME",
#                                  ...) {
#
#   geography_data <- map(
#
#   )
#
#   for (i in geographies)
#
#
#
#     data <- data |>
#       select(any_of("name"), column_title, ends_with("estimate"), contains("moe"), -moe_denom)
#
#     county_name_suffix <- paste0("_", janitor::make_clean_names(select_counties_name))
#
#     county_combine_names <- paste0(names(data), county_name_suffix)
#
#     data_combined <- data |>
#       filter(name == select_area_name_plot) |>
#       bind_cols(
#         data |>
#           filter(name == select_counties_name) |>
#           set_names(county_combine_names) |>
#           select(contains("perc"))
#       )
#
#     data_combined |>
#       select(-name) |>
#       gt::gt() |>
#       gt_census_cols(
#         est_col_label = "Households",
#         perc_col_label = "% of total",
#         combined_spanner = "INSPIRE Area",
#         tables = tables
#       ) |>
#       gt_census_cols(
#         est_cols = NULL,
#         perc_cols = paste0(c("perc_estimate", "perc_moe"), county_name_suffix),
#         perc_col_label = "% of total",
#         combined_spanner = "Baltimore City",
#         source = NULL
#       )
#   }
# }
