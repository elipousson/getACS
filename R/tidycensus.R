#' Get multiple ACS tables
#'
#' @param tables A character vector of tables.
#' @inheritParams tidycensus::get_acs
#' @inheritParams label_acs_metadata
#' @keywords internal
#' @export
get_acs_tables <- function(geography,
                           tables = NULL,
                           cache_table = TRUE,
                           year = 2021,
                           survey = "acs5",
                           label = FALSE,
                           perc = TRUE,
                           geoid = "GEOID",
                           ...) {
  survey_label <- acs_survey_label(
    survey = survey,
    year = year
  )

  table_list <- purrr::map(
    cli::cli_progress_along(
      tables,
      format = "{col_blue(symbol$info)} Downloading tables from {col_blue(survey_label)} | {pb_bar} {pb_percent}"
    ),
    function(i) {
      suppressMessages(
        tidycensus::get_acs(
          geography = geography,
          table = tables[[i]],
          year = year,
          survey = survey,
          ...
        )
      )
    }
  )

  table_data <- purrr::list_rbind(
    table_list
  )

  if (!label) {
    return(table_data)
  }

  label_acs_metadata(
    table_data,
    survey = survey,
    year = year,
    perc = perc,
    geoid = geoid
  )
}

# get_acs_geographies  function(geographies,
#                               state,
#                               county = NULL,
#                               tables = NULL,
#                               cache_table = TRUE,
#                               year = 2021,
#                               label = FALSE,
#                               survey = "acs5",
#                               perc = TRUE,
#                               geoid = "GEOID",
#                               ...) {
#   survey_label <- acs_survey_label(
#     survey = survey,
#     year = year
#   )
#
#   table_list <- purrr::map(
#     cli::cli_progress_along(
#       tables,
#       format = "Downloading tables from {survey_label}",
#       type = "tasks"
#     ),
#     function(i) {
#       suppressMessages(
#         tidycensus::get_acs(
#           geography = geography,
#           table = tables[[i]],
#           year = year,
#           survey = survey,
#           ...
#         )
#       )
#     }
#   )
#
#   table_data <- purrr::list_rbind(
#     table_list
#   )
#
#   if (!label) {
#     return(table_data)
#   }
#
#   label_acs_metadata(
#     table_data,
#     survey = survey,
#     year = year,
#     perc = perc,
#     geoid = geoid
#   )
# }
