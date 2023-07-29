#' Get a time series of ACS data
#'
#' @inheritParams get_acs_geographies
#' @param year If length 1, the function uses [acs_survey_ts()] to get data for
#'   all comparable survey years back to the start of the ACS. If length is
#'   greater than 1, return the selected years even if those years may not be
#'   valid to compare.
#' @export
#' @importFrom purrr map list_cbind list_rbind
#' @importFrom rlang try_fetch
#' @importFrom tidycensus get_acs
get_acs_ts <- function(geography,
                       variables = NULL,
                       table = NULL,
                       cache_table = TRUE,
                       year = 2021,
                       state = NULL,
                       county = NULL,
                       survey = "acs5",
                       ...) {
  years <- year

  if (has_length(year, 1)) {
    years <- acs_survey_ts(survey, year)
  }

  acs_list <- purrr::map(
    seq_along(years),
    function(i) {
      rlang::try_fetch(
        purrr::list_cbind(
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

  purrr::list_rbind(
    acs_list
  )
}
