#' Get a time series of ACS data
#'
#' @inheritParams tidycensus::get_acs
#' @param year description
#' @inheritDotParams tidycensus::get_acs
#' @export
#' @importFrom purrr map list_cbind list_rbind
#' @importFrom rlang try_fetch
#' @importFrom tidycensus get_acs
get_acs_ts <- function(geography,
                       state = NULL,
                       survey = "acs5",
                       year = 2021,
                       cache_table = TRUE,
                       ...) {
  years <- acs_survey_ts(survey, year)

  print(years)

  acs_list <- purrr::map(
    years,
    function(yr) {
      rlang::try_fetch(
        purrr::list_cbind(
          list(
            tidycensus::get_acs(
              geography = geography,
              state = state,
              survey = survey,
              year = yr,
              cache_table = cache_table,
              ...
            ),
            data.frame("year" = yr)
          )
        ),
        error = function(cnd) {
          cli_warn(
            "Data unavailable for year {yr}."
          )
        }
      )
    }
  )

  purrr::list_rbind(
    acs_list
  )
}
