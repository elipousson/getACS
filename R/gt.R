#' Add a source note to a table
#'
#' A wrapper for [gt::tab_source_note()] and [acs_survey_label_tables()]
#'
#' @inheritParams gt::tab_source_note
#' @inheritParams acs_survey_label_tables
#' @param append_note If `TRUE`, add source_note to the end of the generated ACS
#'   data label. If `FALSE`, any supplied source_note will be used instead of an
#'   ACS label.
#' @param use_md If `TRUE`, pass source_note to gt::md() first.
#' @keywords internal
#' @export
#' @importFrom gt tab_source_note md
#' @importFrom glue glue
tab_acs_source <- function(gt_object,
                           source_note = NULL,
                           survey = "acs5",
                           year = 2021,
                           prefix = "Source: ",
                           after = ".",
                           tables = NULL,
                           table_label = "Table",
                           append_note = FALSE,
                           use_md = FALSE,
                           ...) {
  if (append_note) {
    after <- after %||% ""
    after <- paste(after, source_note)
    source_note <- NULL
  }

  source_note <- source_note %||% acs_survey_label_tables(
    survey = survey,
    year = year,
    prefix = prefix,
    tables = tables,
    table_label = table_label,
    after = after,
    ...
  )

  if (use_md) {
    source_note <- gt::md(source_note)
  }

  gt::tab_source_note(
    gt_object,
    source_note = source_note
  )
}

#' A wrapper for [gt::cols_merge_uncert()] with extra features
#'
#' Wrap [gt::cols_merge_uncert()] but allow col_val and col_uncert to be set
#' based on a length 2 cols parameter and optionally apply a prefix or postfix
#' value.
#'
#' @name cols_merge_uncert_ext
#' @inheritParams gt::cols_merge_uncert
#' @param columns description
#' @param prefix,postfix Optional strings to insert before and/or after both
#'   col_val and col_uncert. Use a length 2 string `c("", "uncert_prefix")` if
#'   you want to apply a prefix to only one or the other column specification.
#' @inheritParams stringr::str_c
#' @keywords internal
#' @export
#' @importFrom stringr str_c
cols_merge_uncert_ext <- function(gt_object,
                                  columns = NULL,
                                  col_val = NULL,
                                  col_uncert = NULL,
                                  prefix = "",
                                  postfix = "",
                                  sep = "",
                                  ...) {
  columns <- columns %||% c(col_val, col_uncert)

  stopifnot(all(is.character(columns)))

  columns <- stringr::str_c(prefix, columns, postfix, sep = sep)

  if (!has_length(columns, 2)) {
    cli::cli_abort(
      "{.fn cols_merge_uncert_ext} requires {.arg columns} be
      a length 2 character vector."
    )
  }

  if (!all(has_name(gt_object[["_data"]], columns))) {
    return(gt_object)
  }

  gt::cols_merge_uncert(
    gt_object,
    col_val = columns[[1]],
    col_uncert = columns[[2]],
    ...
  )
}


#' Format estimate and MOE columns in a gt table
#'
#' @keywords internal
#' @export
fmt_acs_estimate <- function(gt_object,
                             col_est = "estimate",
                             col_moe = "moe",
                             columns = NULL,
                             col_labels = "Est.",
                             spanner = NULL,
                             decimals = 0,
                             use_seps = TRUE) {
  if (is.null(columns)) {
    return(gt_object)
  }

  columns <- columns %||% c(col_est, col_moe)

  gt_object <- acs_cols_label(gt_object, columns, col_labels)

  gt_object <- gt::fmt_number(
    gt_object,
    columns = any_of(columns),
    decimals = decimals,
    use_seps = use_seps
  )

  if (is.null(spanner)) {
    return(cols_merge_uncert_ext(gt_object, columns = columns))
  }

  gt::tab_spanner(gt_object, spanner, columns = columns)
}

#' @noRd
acs_cols_label <- function(gt_object, columns, col_labels = NULL) {
  if (is.null(col_labels)) {
    return(gt_object)
  }

  stopifnot(
    all(is.character(columns))
  )

  if (has_length(col_labels, 1)) {
    col_labels <- set_names(col_labels, columns[[1]])
  } else {
    col_labels <- set_names(col_labels, columns)
  }

  gt::cols_label(
    gt_object,
    .list = col_labels
  )
}

#' Format % estimate and % MOE columns in a gt table
#'
#' @keywords internal
#' @export
fmt_acs_percent <- function(gt_object,
                            col_est = "perc_estimate",
                            col_moe = "perc_moe",
                            columns = NULL,
                            col_labels = "% share",
                            spanner = NULL,
                            decimals = 0,
                            use_seps = TRUE,
                            ...) {
  columns <- columns %||% c(col_est, col_moe)

  gt_object <- acs_cols_label(gt_object, columns, col_labels)

  gt_object <- gt::fmt_percent(
    gt_object,
    columns = any_of(columns),
    decimals = decimals,
    use_seps = use_seps,
    ...
  )

  if (is.null(spanner)) {
    return(cols_merge_uncert_ext(gt_object, columns))
  }

  stopifnot(
    is.character(spanner)
  )

  gt::tab_spanner(gt_object, spanner, columns = columns)
}

#' Format Census estimate and percent estimate columns
#'
#' @keywords internal
#' @export
#' @importFrom gt tab_spanner
gt_census_cols <- function(gt_object,
                           est_cols = c("estimate", "moe"),
                           est_col_label = "Est.",
                           perc_cols = c("perc_estimate", "perc_moe"),
                           perc_col_label = "% share",
                           est_spanner = NULL,
                           perc_spanner = NULL,
                           combined_spanner = NULL,
                           column_title = NULL,
                           source_note = NULL,
                           survey = "acs5",
                           year = 2021,
                           prefix = "Source: ",
                           after = ".",
                           tables = NULL,
                           decimals = 0,
                           ...) {
  if (is.data.frame(gt_object)) {
    gt_object <- gt::gt(gt_object, ...)
  }

  stopifnot(
    has_name(gt_object[["_data"]], "column_title")
  )

  gt_object <- gt_object |>
    fmt_acs_estimate(
      columns = est_cols,
      col_labels = est_col_label,
      decimals = decimals,
      spanner = est_spanner
    ) |>
    fmt_acs_percent(
      columns = perc_cols,
      col_labels = perc_col_label,
      decimals = decimals,
      spanner = perc_spanner
    )

  if (!is_null(combined_spanner)) {
    gt_object <- gt::tab_spanner(
      gt_object,
      label = combined_spanner,
      columns = any_of(c(est_cols, perc_cols))
    )
  }

  tab_acs_source(
    gt_object = gt_object,
    source_note = source_note,
    survey = survey,
    year = year,
    prefix = prefix,
    after = after,
    tables = tables
  )
}
