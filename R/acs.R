#' Assorted helpers for ACS survey types and labels
#'
#' @rdname acs_survey
#' @name acs_survey_match
#' @keywords internal
#' @export
#' @importFrom rlang arg_match0
acs_survey_match <- function(survey = "acs5",
                             error_call = caller_env()) {
  arg_match0(survey, c("acs5", "acs3", "acs1"), error_call = error_call)
}

#' @rdname acs_survey
#' @name acs_survey_sample
#' @keywords internal
#' @export
#' @importFrom stringr str_extract
acs_survey_sample <- function(survey = "acs5") {
  stringr::str_extract(survey, "[0-9]$")
}

#' @rdname acs_survey
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

#' @rdname acs_survey
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
#' @importFrom glue glue
#' @importFrom readr read_csv
get_acs_metadata <- function(survey = "acs5",
                             year = 2021,
                             metadata = "table",
                             ...,
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
  filename <- glue::glue("census_{metadata}_metadata.csv")

  file <- system.file(
    "extdata", paste0(year, "_", filename),
    package = "tidycensusExtras"
  )

  if (!file.exists(file)) {
    folder <- glue::glue("acs{year}_{sample}yr")
    base_url <- "https://raw.githubusercontent.com/censusreporter/census-table-metadata/master/precomputed"
    file <- paste0(c(base_url, folder, filename), collapse = "/")
  }

  readr::read_csv(
    file = file,
    ...,
    progress = progress,
    show_col_types = show_col_types
  )
}

#' Label American Community Survey data using table and column metadata from
#' Census Reporter
#'
#' @param data A data frame downloaded with [tidycensus::get_acs()].
#' @inheritParams get_acs_metadata
#' @seealso [join_acs_percent()]
#' @keywords internal
#' @export
#' @importFrom rlang has_name
label_acs_metadata <- function(data,
                               survey = "acs5",
                               year = 2021,
                               perc = TRUE,
                               geoid = "GEOID") {
  data <- label_acs_table_metadata(data, survey, year)

  data <- label_acs_column_metadata(data, survey, year)

  if (perc && all(has_name(data, geoid))) {
    data <- join_acs_percent(data, geoid = geoid)
  }

  data
}

#' @rdname label_acs_metadata
#' @name label_acs_table_metadata
#' @keywords internal
#' @export
#' @importFrom dplyr mutate left_join join_by
#' @importFrom stringr str_extract str_remove
label_acs_table_metadata <- function(data,
                                     survey = "acs5",
                                     year = 2021) {
  table_metadata <- get_acs_metadata(survey, year, metadata = "table")

  stopifnot(
    has_name(data, "variable")
  )

  data <- dplyr::mutate(
    data,
    table_id = stringr::str_extract(variable, ".+(?=_)"),
    .after = dplyr::all_of("variable")
  )

  data <- dplyr::left_join(data, table_metadata, by = dplyr::join_by(table_id))

  if (any(stringr::str_detect(data[["table_id"]], "[:alpha:]"))) {
    stopifnot(
      has_name(data, "table_title")
    )

    data <- dplyr::mutate(
      data,
      race_category = stringr::str_extract(table_title, "(?<=\\().+(?=\\))")
    )
  }

  data
}

#' @rdname label_acs_metadata
#' @name label_acs_column_metadata
#' @keywords internal
#' @export
#' @importFrom dplyr mutate left_join rename
#' @importFrom stringr str_remove
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
    by = join_by(table_id, column_id)
  )
}


#' Calculate estimates as a percent of the denominator estimates
#'
#' Use the denominator_column_id value from the column metadata added with
#' [label_acs_metadata()] to calculate the estimate as a percent share of the
#' denominator value. [tidycensus::moe_prop()] is used to calculate the margin
#' of error for the percentage.
#'
#' @keywords internal
#' @inheritParams base::round
#' @inheritParams dplyr::left_join
#' @seealso [tidycensus::moe_prop()]
#' @export
#' @importFrom cli cli_alert_warning
#' @importFrom dplyr filter select left_join mutate
#' @importFrom tidycensus moe_prop
join_acs_percent <- function(data,
                             geoid = "GEOID",
                             na_matches = "never",
                             digits = 2) {
  stopifnot(
    all(has_name(data, c(
      geoid, "column_id", "column_title",
      "denominator_column_id", "estimate", "moe"
    )))
  )

  if (!all(data[["denominator_column_id"]] %in% data[["column_id"]])) {
    cli::cli_alert_warning(
      "{.arg data} does not contain all of the denominator values needed
      to calculate the percent estimates for each variables.",
      wrap = TRUE
    )
  }

  denominator_data <- data |>
    dplyr::filter(column_id %in% data[["denominator_column_id"]]) |>
    dplyr::select(
      {{ geoid }},
      denominator_estimate = estimate,
      denominator_moe = moe,
      denominator_column_title = column_title,
      denominator_column_id = column_id
    )

  data |>
    dplyr::left_join(
      denominator_data,
      by = dplyr::join_by({{ geoid }}, "denominator_column_id"),
      na_matches = na_matches
    ) |>
    dplyr::mutate(
      perc_estimate = round(estimate / denominator_estimate, digits = digits),
      perc_moe = round(
        tidycensus::moe_prop(
          estimate, denominator_estimate,
          moe, denominator_moe
        ),
        digits = digits
      ),
      .after = all_of("moe")
    )
}

#' Filter American Community Survey data by table, variables, or other attributes
#'
#' A data frame of American Community Survey data enriched with table and column
#' metadata using the [label_acs_metadata()] function.
#'
#' @param data A data frame with a "table_id", "variable", and "column_title"
#'   columns.
#' @param ... Parameters passed to [dplyr::filter()]
#' @param table,column Table ID and column title values to return.
#' @param vars,drop_vars Variables to keep and drop.
#' @keywords internal
#' @seealso
#'  [dplyr::filter()]
#' @rdname filter_acs
#' @export
#' @importFrom dplyr filter
filter_acs <- function(data,
                       ...,
                       table = NULL,
                       column = NULL,
                       vars = NULL,
                       drop_vars = NULL) {
  stopifnot(
    all(has_name(data, c("table_id", "variable", "column_title")))
  )

  if (!is_null(table)) {
    data <- dplyr::filter(
      data,
      table_id %in% table
    )
  }

  if (!is_null(drop_vars)) {
    data <- dplyr::filter(
      data,
      !(variable %in% drop_vars)
    )
  }

  if (!is_null(vars)) {
    data <- dplyr::filter(
      data,
      variable %in% vars
    )
  }

  if (!is_null(column)) {
    data <- dplyr::filter(
      data,
      column_title %in% column
    )
  }

  dplyr::filter(data, ...)
}
