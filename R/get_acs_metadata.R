#' Get table or column metadata from Census Reporter project
#'
#' Read precomputed U.S. Census table or column metadata files from the  [Census
#' Reporter GitHub
#' repository](https://github.com/censusreporter/census-table-metadata).
#'
#' @param survey Survey, "acs5", "acs3", or "acs1".
#' @param year Sample year (between 2006 and 2021).
#' @param metadata Type of metadata to return, "table" or "column"
#' @inheritDotParams readr::read_csv
#' @keywords internal
#' @export
#' @importFrom rappdirs user_cache_dir
get_acs_metadata <- function(survey = "acs5",
                             year = 2021,
                             metadata = "table",
                             ...,
                             table = NULL,
                             cache_data = TRUE,
                             progress = FALSE,
                             show_col_types = FALSE,
                             error_call = caller_env()) {
  stopifnot(
    (year >= 2006) && (year <= 2021)
  )

  survey <- tolower(survey)
  survey <- acs_survey_match(survey, error_call = error_call)
  sample <- acs_survey_sample(survey)

  metadata <- arg_match0(metadata, c("table", "column"))
  filename <- glue("census_{metadata}_metadata.csv")
  folder <- glue("acs{year}_{sample}yr")
  cache_dir <- rappdirs::user_cache_dir("getACS")

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }

  cache_path <- file.path(cache_dir, glue("{folder}_{filename}"))

  if (file.exists(cache_path)) {
    msg <- "Reading cached {metadata} metadata for {acs_survey_label(survey, year)}"
    file <- cache_path
    cache_data <- FALSE
  } else {
    msg <- "Downloading {metadata} metadata for {acs_survey_label(survey, year)}"
    base_url <- "https://raw.githubusercontent.com/censusreporter/census-table-metadata/master/precomputed"
    file <- paste0(c(base_url, folder, filename), collapse = "/")
  }

  check_installed("readr", call = error_call)

  cli::cli_progress_step(msg)

  data <- readr::read_csv(
    file = file,
    ...,
    progress = progress,
    show_col_types = show_col_types
  )

  if (cache_data) {
    cli::cli_progress_step("Caching {metadata} metadata")

    readr::write_csv(
      x = data,
      file = cache_path,
      progress = progress
    )
  }

  if (is_null(table)) {
    return(data)
  }

  cli::cli_progress_step(
    "Filtering {metadata} metadata to table ID {.val {table}}"
  )

  vctrs::vec_slice(
    data,
    vctrs::vec_in(
      data[["table_id"]],
      table
    )
  )
}

#' Label American Community Survey data using table and column metadata from
#' Census Reporter
#'
#' @param data A data frame downloaded with [tidycensus::get_acs()].
#' @param perc If `TRUE` (default), use the denominator column ID to calculate
#'   each estimate as a percent share of the denominator value and use
#'   [tidycensus::moe_prop()] to calculate a new margin of error for the percent
#'   estimate.
#' @inheritParams join_acs_percent
#' @inheritParams get_acs_metadata
#' @seealso [join_acs_percent()]
#' @keywords internal
#' @export
#' @importFrom rlang has_name
label_acs_metadata <- function(data,
                               survey = "acs5",
                               year = 2021,
                               perc = TRUE,
                               geoid_col = "GEOID") {
  data <- label_acs_table_metadata(data, survey, year)

  data <- label_acs_column_metadata(data, survey, year)

  if (perc && all(has_name(data, geoid_col))) {
    data <- join_acs_percent(data, geoid_col = geoid_col)
  }

  data
}

#' @rdname label_acs_metadata
#' @name label_acs_table_metadata
#' @inheritParams get_acs_metadata
#' @export
#' @importFrom dplyr mutate left_join all_of
#' @importFrom stringr str_detect
label_acs_table_metadata <- function(data,
                                     survey = "acs5",
                                     year = 2021) {
  stopifnot(
    has_name(data, "variable")
  )

  table_metadata <- get_acs_metadata(survey, year, metadata = "table")

  data <- dplyr::mutate(
    data,
    table_id = str_table_id(variable),
    .after = dplyr::all_of("variable")
  )

  data <- dplyr::left_join(data, table_metadata, by = dplyr::join_by(table_id))

  if (any(stringr::str_detect(data[["table_id"]], "[:alpha:]$"))) {
    data <- join_acs_race_iteration(data)
  }

  data
}

#' @noRd
#' @importFrom stringr str_extract
str_table_id <- function(variable) {
  stringr::str_extract(variable, ".+(?=_)")
}

#' @noRd
#' @importFrom stringr str_extract str_replace_all
join_acs_race_iteration <- function(data) {
  stopifnot(
    has_name(data, "table_id")
  )

  data[["race_iteration_code"]] <- stringr::str_extract(
    data[["table_id"]], "[:alpha:]$"
  )

  data[["race_iteration_group"]] <- stringr::str_replace_all(
    data[["race_iteration_code"]],
    pattern = set_names(
      race_iteration[["group"]],
      race_iteration[["code"]]
    )
  )

  data
}

#' @rdname label_acs_metadata
#' @name label_acs_column_metadata
#' @export
#' @importFrom dplyr mutate left_join all_of
#' @importFrom stringr str_extract str_remove
label_acs_column_metadata <- function(data,
                                      survey = "acs5",
                                      year = 2021) {
  column_metadata <- get_acs_metadata(survey, year, metadata = "column")

  stopifnot(
    has_name(data, "variable")
  )

  data <- dplyr::mutate(
    data,
    table_id = stringr::str_extract(variable, ".+(?=_)"),
    column_id = stringr::str_remove(variable, "_"),
    .after = dplyr::all_of("variable")
  )

  data <- dplyr::left_join(
    data,
    column_metadata,
    by = dplyr::all_of(c("table_id", "column_id"))
  )

  # Strip trailing ":" from "Total:"
  dplyr::mutate(
    data,
    column_title = stringr::str_remove(column_title, ":$")
  )
}
