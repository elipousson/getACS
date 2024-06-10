#' @noRd
acs_cache_dir <- function(pkg = "getACS") {
  cache_dir <- rappdirs::user_cache_dir(pkg)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }

  cache_dir
}

#' Get table or column metadata from Census Reporter project
#'
#' Read precomputed U.S. Census table or column metadata files from the  [Census
#' Reporter GitHub
#' repository](https://github.com/censusreporter/census-table-metadata).
#'
#' @param survey Survey, "acs5", "acs3", or "acs1".
#' @param year Sample year (between 2006 and 2022).
#' @param metadata Type of metadata to return, "table" or "column"
#' @inheritDotParams readr::read_csv
#' @keywords internal
#' @export
#' @importFrom rappdirs user_cache_dir
get_acs_metadata <- function(survey = "acs5",
                             year = 2022,
                             metadata = "table",
                             ...,
                             table = NULL,
                             cache_data = TRUE,
                             progress = FALSE,
                             show_col_types = FALSE,
                             quiet = FALSE,
                             error_call = caller_env()) {
  sample <- acs_survey_sample(survey)

  check_acs_survey(survey, year, sample, call = error_call)

  metadata <- arg_match0(
    metadata,
    values = c("table", "column"),
    error_call = error_call
  )

  filename <- glue("census_{metadata}_metadata.csv")
  folder <- glue("acs{year}_{sample}yr")
  cache_path <- file.path(acs_cache_dir(), glue("{folder}_{filename}"))
  acs_label <- acs_survey_label(survey, year)

  msg <- "Downloading {metadata} metadata for {acs_label}"
  base_url <- "https://raw.githubusercontent.com/censusreporter/census-table-metadata/master/precomputed"
  file <- paste0(c(base_url, folder, filename), collapse = "/")

  if (file.exists(cache_path)) {
    msg <- "Reading cached {metadata} metadata for {acs_label}"
    file <- cache_path
    cache_data <- FALSE
  }

  check_installed("readr", call = error_call)

  cli_quiet(quiet)

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

  table <- arg_match(
    table,
    values = data[["table_id"]],
    multiple = TRUE,
    error_call = error_call
  )

  vctrs::vec_slice(
    data,
    i = vctrs::vec_in(
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
#' @param variable_col Variable column name. Defaults to "variable"
#' @inheritParams join_acs_percent
#' @inheritParams get_acs_metadata
#' @inheritParams assign_acs_reliability
#' @seealso [join_acs_percent()]
#' @keywords internal
#' @export
#' @importFrom rlang has_name
label_acs_metadata <- function(data,
                               survey = "acs5",
                               year = 2022,
                               perc = TRUE,
                               reliability = FALSE,
                               moe_level = 90,
                               geoid_col = "GEOID",
                               variable_col = "variable") {
  data <- label_acs_table_metadata(
    data = data,
    survey = survey,
    year = year,
    variable_col = variable_col
  )

  data <- label_acs_column_metadata(
    data = data,
    survey = survey,
    year = year,
    variable_col = variable_col
  )

  if (perc && all(has_name(data, geoid_col))) {
    data <- join_acs_percent(data, geoid_col = geoid_col)
  }

  if (reliability) {
    data <- assign_acs_reliability(data, moe_level = moe_level)
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
                                     year = 2022,
                                     variable_col = "variable",
                                     table_id_col = "table_id",
                                     call = caller_env()) {
  stopifnot(
    has_name(data, variable_col)
  )

  table_metadata <- get_acs_metadata(survey, year, metadata = "table")

  data <- dplyr::mutate(
    data,
    "{table_id_col}" := str_table_id(.data[[variable_col]]),
    .after = all_of(variable_col)
  )

  data <- dplyr::left_join(
    data,
    table_metadata,
    by = dplyr::join_by({{ table_id_col }})
  )

  if (is_character(data[[table_id_col]]) && !all(is.na(data[[table_id_col]]))) {
    has_race_iteration <- any(
      stringr::str_detect(data[[table_id_col]], "[:alpha:]$")
    )

    if (has_race_iteration) {
      data <- join_acs_race_iteration(
        data,
        table_id_col = table_id_col,
        call = call
      )
    }
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
join_acs_race_iteration <- function(data,
                                    table_id_col = "table_id",
                                    call = caller_env()) {
  check_has_name(data, nm = table_id_col, call = call)

  data[["race_iteration_code"]] <- stringr::str_extract(
    data[[table_id_col]], "[:alpha:]$"
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
                                      year = 2022,
                                      variable_col = "variable",
                                      column_id_col = "column_id",
                                      column_title_col = "column_title",
                                      table_id_col = "table_id",
                                      call = caller_env()) {
  column_metadata <- get_acs_metadata(survey, year, metadata = "column")

  check_has_name(data, nm = variable_col, call = call)

  data <- dplyr::mutate(
    data,
    "{table_id_col}" := str_table_id(.data[[variable_col]]),
    "{column_id_col}" := stringr::str_remove(.data[[variable_col]], "_"),
    .after = all_of(variable_col)
  )

  data <- dplyr::left_join(
    data,
    column_metadata,
    by = c(table_id_col, column_id_col)
  )

  # Strip trailing ":" from "Total:"
  dplyr::mutate(
    data,
    "{column_title_col}" := stringr::str_remove(.data[[column_title_col]], ":$")
  )
}
