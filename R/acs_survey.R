#' Assorted helpers for ACS survey types and labels
#'
#' These simple functions allow validating ACS survey options, getting
#' comparable years for time series analysis, and creating standard labels.
#'
#' @examples
#' acs_survey_match("acs1")
#'
#' acs_survey_sample("acs3")
#'
#' acs_survey_ts("acs5", 2020)
#'
#' acs_survey_label()
#'
#' acs_survey_label_table(table = c("B19013", "B01003"))
#' @name acs_survey
NULL



#' @rdname acs_survey
#' @name acs_survey_match
#' @param survey ACS survey, "acs5", "acs3", or "acs1".
#' @inheritParams rlang::args_error_context
#' @export
#' @importFrom rlang arg_match0
acs_survey_match <- function(survey = "acs5",
                             error_call = caller_env()) {
  arg_match0(survey, c("acs5", "acs3", "acs1"), error_call = error_call)
}

#' @rdname acs_survey
#' @name acs_survey_sample
#' @export
#' @importFrom stringr str_extract
acs_survey_sample <- function(survey = "acs5") {
  stringr::str_extract(survey, "[0-9]$")
}

#' @rdname acs_survey
#' @param year Based on the year and survey, [acs_survey_ts()] returns a vector
#'   of years for non-overlapping ACS samples to allow comparison.
#' @export
#' @importFrom glue glue
acs_survey_ts <- function(survey = "acs5",
                          year = 2021,
                          call = caller_env()) {
  sample <- acs_survey_sample(survey)
  check_number_whole(year, call = call)

  min_year <- switch(sample,
    "1" = 2005,
    "3" = 2007,
    "5" = 2009
  )

  if (year < min_year) {
    cli_abort(
      "{.arg year} must be equal to or greater than the minimum release year
      {min_year} for {.arg survey} {survey}",
      call = call
    )
  }

  if (identical(year, 2005)) {
    return(year)
  }

  comparison_url <- paste0(
    "https://www.census.gov/programs-surveys/acs/guidance/comparing-acs-data/",
    year, ".html"
  )

  cli_bullets(
    c("i" = "Learn more about comparing {year} American Community Survey Data:
    {.url {comparison_url}}")
  )

  years <- year
  sample <- as.integer(sample)

  while (min(years) %% min_year >= sample) {
    years <- c(years, min(years) - sample)
  }

  years
}

#' @rdname acs_survey
#' @name acs_survey_label
#' @param prefix Text to insert before ACS survey label.
#' @param pattern Pattern passed to [glue::glue()]. Allows use of the
#'   `year_start` variable which is the earliest year for a survey sample
#'   specified by the survey parameter.
#' @export
acs_survey_label <- function(survey = "acs5",
                             year = 2021,
                             pattern = "{year_start}-{year} ACS {sample}-year Estimates",
                             prefix = "") {
  sample <- acs_survey_sample(survey)

  year_start <- year - (as.integer(sample) - 1)

  glue(prefix, pattern, .envir = current_env())
}

#' @rdname acs_survey
#' @name acs_survey_label_table
#' @param table One or more table IDs to include in label or source note.
#' @param table_label Label to use when referring to table or tables. A "s" is
#'   appended to the end of the table_label if tables is more than length 1.
#' @param before,after A character string to be added before/after each word.
#' @param end A character string appended to the end of the full label. Defaults
#'   to ".".
#' @inheritParams knitr::combine_words
#' @export
#' @importFrom knitr combine_words
acs_survey_label_table <- function(survey = "acs5",
                                   year = 2021,
                                   prefix = "",
                                   table = NULL,
                                   table_label = "Table",
                                   sep = ", ",
                                   and = " and ",
                                   before = "",
                                   after = before,
                                   end = ".",
                                   oxford_comma = TRUE) {
  label <- acs_survey_label(survey, year, prefix = prefix)
  table <- unique(table)

  if (is_empty(table) || identical(table, "")) {
    return(paste0(label, end))
  }

  if (length(table) > 1) {
    table_label <- paste0(table_label, "s")
  }

  table <- knitr::combine_words(
    table,
    sep = sep,
    and = and,
    before = before,
    after = after,
    oxford_comma = oxford_comma
  )

  glue("{label}, {table_label} {table}{end}")
}
