#' Assorted helpers for ACS survey types and labels
#'
#' @rdname acs_survey_match
#' @name acs_survey_match
#' @keywords internal
#' @export
#' @importFrom rlang arg_match0
acs_survey_match <- function(survey = "acs5",
                             error_call = caller_env()) {
  arg_match0(survey, c("acs5", "acs3", "acs1"), error_call = error_call)
}

#' @rdname acs_survey_match
#' @name acs_survey_sample
#' @keywords internal
#' @export
#' @importFrom stringr str_extract
acs_survey_sample <- function(survey = "acs5") {
  stringr::str_extract(survey, "[0-9]$")
}

#' @rdname acs_survey_match
#' @name acs_survey_ts
#' @param year Based on the year and survey, [acs_survey_ts()] returns a vector
#'   of years for non-overlapping ACS samples to allow comparison.
#' @keywords internal
#' @export
#' @importFrom glue glue
acs_survey_ts <- function(survey,
                          year) {
  stopifnot(
    year > 2005
  )

  sample <- acs_survey_sample(survey)

  comparison_url <- paste0(
    "https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data/",
    year, ".html"
  )

  cli::cli_inform(
    "Learn more about comparing {year} American Community Survey Data:
    {.url {comparison_url}}"
  )

  min_year <- switch (sample,
                      "1" = 2005,
                      "3" = 2007,
                      "5" = 2009
  )

  sample <- as.integer(sample)
  years <- year

  while (min(years) %% min_year >= sample) {
    years <- c(years, min(years) - sample)
  }

  years
}

#' @rdname acs_survey_match
#' @name acs_survey_label
#' @keywords internal
#' @export
#' @importFrom glue glue
acs_survey_label <- function(survey = "acs5",
                             year = 2021,
                             pattern = "{year_start}-{year} ACS {sample}-year Estimates",
                             prefix = "") {
  sample <- acs_survey_sample(survey)

  year_start <- year - (as.integer(sample) - 1)

  glue::glue(prefix, pattern, .envir = current_env())
}

#' @rdname acs_survey_match
#' @name acs_survey_label_tables
#' @keywords internal
#' @export
#' @importFrom glue glue
#' @importFrom knitr combine_words
acs_survey_label_tables <- function(survey = "acs5",
                                    year = 2021,
                                    prefix = "",
                                    tables = NULL,
                                    table_label = "Table",
                                    sep = ", ",
                                    and = " and ",
                                    before = "",
                                    after = ".",
                                    oxford_comma = TRUE,
                                    ...) {
  label <- acs_survey_label(survey, year, prefix = prefix)

  if (is_null(tables) || tables == "") {
    return(paste0(label, after))
  }


  if (length(tables) > 1) {
    table_label <- paste0(table_label, "s")
  }

  tables <- knitr::combine_words(
    tables,
    sep = sep,
    and = and,
    before = before,
    after = after,
    oxford_comma = oxford_comma
  )

  glue::glue("{label}, {table_label} {tables}")
}
