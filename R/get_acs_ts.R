#' Get multiple years of ACS data for time series analysis
#'
#' [get_acs_ts()] is a variant on [get_acs_geographies()] that supports
#' downloading data for multiple years in addition to multiple tables or
#' multiple geographies. The year is appended as an additional column in the
#' returned data frame. The intended use is to provide the latest year needed
#' and the function will download data for all non-overlapping survey periods.
#' For example, 2021 ACS data using the 5-year sample can be compared to 5-year
#' data from 2016 and 2011. Not all variables can be compared across different
#' years and caution is recommended when using ACS data for time series
#' analysis.
#'
#' @inheritParams get_acs_geographies
#' @param year A numeric vector of years. If length 1, the function uses
#'   [acs_survey_ts()] to get data for all comparable survey years back to the
#'   start of the ACS. This is the recommended approach for using
#'   [get_acs_ts()]. If length is greater than 1, return the selected years even
#'   if those years may not be valid to compare.
#' @inheritParams cli_quiet
#' @returns A data frame or sf object.
#' @export
#' @importFrom rlang try_fetch
#' @importFrom tidycensus get_acs
get_acs_ts <- function(geography,
                       variables = NULL,
                       table = NULL,
                       cache_table = TRUE,
                       year = 2022,
                       state = NULL,
                       county = NULL,
                       survey = "acs5",
                       ...,
                       quiet = FALSE) {
  cli_quiet(quiet)

  years <- year

  if (has_length(year, 1)) {
    years <- acs_survey_ts(survey, year)
  }

  acs_data <- map(
    seq_along(years),
    function(i) {
      rlang::try_fetch(
        list_cbind(
          list(
            get_acs_geographies(
              geography = geography,
              variables = variables,
              table = table,
              cache_table = cache_table,
              year = years[[i]],
              state = state,
              county = county,
              survey = survey,
              ...
            ),
            data.frame("year" = years[[i]])
          )
        ),
        error = function(cnd) {
          cli_warn(
            "Data unavailable for year {years[[i]]}."
          )
          return(NULL)
        }
      )
    }
  )

  acs_data <- list_rbind(
    acs_data
  )

  if (is_true(list2(...)[["geometry"]])) {
    check_installed("sf")
    acs_data <- sf::st_as_sf(acs_data)
  }

  acs_data
}
