#' Apply formatting to value and percent value columns using `gt::fmt_number()`
#' and `gt::fmt_percent()`
#'
#' [acs_fmt_values()] is a wrapper for [gt::fmt_number()], [gt::fmt_percent()],
#' and [gt::cols_merge_uncert()].
#'
#' @inheritParams acs_perc_cols
#' @inheritParams gt::fmt_number
#' @inheritParams gt::fmt_percent
#' @param merge_moe If `TRUE`, use [gt::cols_merge_uncert()] to merge the
#'   value_col and moe_col and the percent value and margin of error columns.
#' @param .cols_fn tidyselect function used to select columns to format and
#'   merge. Defaults to [tidyselect::starts_with]
#' @keywords internal
acs_fmt_values <- function(data,
                           value_col = "estimate",
                           moe_col = "moe",
                           perc_prefix = "perc",
                           perc_sep = "_",
                           decimals = 0,
                           use_seps = TRUE,
                           accounting = FALSE,
                           scale_by = 1,
                           merge_moe = TRUE,
                           .cols_fn = starts_with) {
  perc_cols <- acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep)

  data <- data |>
    gt::fmt_number(
      columns = .cols_fn(c(value_col, moe_col)),
      use_seps = use_seps,
      accounting = accounting,
      scale_by = scale_by,
      decimals = decimals
    ) |>
    gt::fmt_percent(
      columns = .cols_fn(perc_cols),
      decimals = decimals
    )

    if (!merge_moe) {
      return(data)
    }

  data |>
    gt::cols_merge_uncert(
      col_val = .cols_fn(value_col),
      col_uncert = .cols_fn(moe_col)
    ) |>
    gt::cols_merge_uncert(
      col_val = .cols_fn(perc_cols[[1]]),
      col_uncert = .cols_fn(perc_cols[[2]])
    )
}

#' Create new spanners using `gt::tab_spanner_delim()`
#'
#' @inheritParams gt::tab_spanner_delim
#' @keywords internal
#' @export
acs_tab_spanner_delim <- function(data,
                                  column_title_col = "column_title",
                                  value_col = "estimate",
                                  moe_col = "moe",
                                  perc_prefix = "perc",
                                  columns = starts_with(
                                    c(value_col, moe_col, perc_prefix)
                                  ),
                                  delim = "_",
                                  split = "last",
                                  limit = 1,
                                  reverse = TRUE,
                                  ...) {
  gt::tab_spanner_delim(
    data,
    delim = delim,
    columns = columns,
    split = split,
    limit = limit,
    reverse = reverse,
    ...
  )
}

#' gt table function parameter definitions
#'
#' @param gt_object A gt object.
#' @name gt_params
#' @keywords internal
NULL

#' Add a Census data source note to a gt table
#'
#' [tab_acs_source_note()] adds a source note to a gt table using
#' [acs_survey_label_table()] and [gt::tab_source_note()].
#'
#' @inheritParams gt_params
#' @inheritParams gt::tab_source_note
#' @inheritParams acs_survey_label_table
#' @param append_note If `TRUE`, add source_note to the end of the generated ACS
#'   data label. If `FALSE`, any supplied source_note will be used instead of an
#'   ACS label.
#' @param use_md If `TRUE`, pass source_note to [gt::md()] first.
#' @param ... For [tab_acs_source_note()], additional parameters passed to
#'   [acs_survey_label_table()]. For [cols_merge_uncert_ext()], additional
#'   parameters passed to [gt::cols_merge_uncert()]. For [fmt_acs_percent()],
#'   additional parameters passed to [gt::fmt_percent()].
#' @family gt table
#' @keywords gt
#' @export
#' @importFrom glue glue
#' @importFrom gt md tab_source_note
tab_acs_source_note <- function(gt_object,
                                source_note = NULL,
                                append_note = FALSE,
                                survey = "acs5",
                                year = 2021,
                                table = NULL,
                                table_label = "Table",
                                prefix = "Source: ",
                                end = ".",
                                use_md = FALSE,
                                ...) {
  if (append_note) {
    end <- paste(end, source_note)
    source_note <- NULL
  }

  source_note <- source_note %||% acs_survey_label_table(
    survey = survey,
    year = year,
    prefix = prefix,
    table = table,
    table_label = table_label,
    end = end,
    ...
  )

  if (use_md) {
    source_note <- gt::md(source_note)
  }

  if (identical(source_note, "")) {
    return(gt_object)
  }

  gt::tab_source_note(
    gt_object,
    source_note = source_note
  )
}

#' Merge columns to a value-with-uncertainty column
#'
#' [cols_merge_uncert_ext()] is a variant of [gt::cols_merge_uncert()] to
#' support col_val and col_uncert to be set based on a length 2 cols parameter
#' and optionally apply a prefix or postfix value. These options are primarily
#' for internal use by [gt_acs()], [fmt_acs_estimate()], and
#' [fmt_acs_percent()].
#'
#' @name cols_merge_uncert_ext
#' @inheritParams gt_params
#' @inheritParams gt::cols_merge_uncert
#' @param columns description
#' @param prefix,postfix Optional strings to insert before and/or after both
#'   col_val and col_uncert. Use a length 2 string `c("", "uncert_prefix")` if
#'   you want to apply a prefix to only one or the other column specification.
#' @inheritParams stringr::str_c
#' @inheritParams rlang::args_error_context
#' @keywords internal
#' @export
#' @importFrom stringr str_c
#' @importFrom gt cols_merge_uncert
#' @importFrom cli cli_alert_warning
cols_merge_uncert_ext <- function(gt_object,
                                  columns = NULL,
                                  col_val = NULL,
                                  col_uncert = NULL,
                                  prefix = "",
                                  postfix = "",
                                  sep = "",
                                  ...,
                                  call = caller_env()) {
  columns <- columns %||% c(col_val, col_uncert)

  check_character(columns, call = call)

  columns <- stringr::str_c(prefix, columns, postfix, sep = sep)

  if (has_length(columns, 1)) {
    return(gt_object)
  }

  if (!all(has_name(gt_object[["_data"]], columns))) {
    cli::cli_alert_warning(
      "Missing one or more {.arg columns}: {.val {columns}}"
    )

    return(gt_object)
  }

  gt::cols_merge_uncert(
    gt_object,
    col_val = columns[[1]],
    col_uncert = columns[[2]],
    ...
  )
}

#' Format estimate and margin of error columns in a gt table
#'
#' [fmt_acs_estimate()] formats estimate and margin of error columns for a gt
#' table created with ACS data. [fmt_acs_percent()] does the same for the
#' perc_estimate and perc_moe columns calculated by [join_acs_percent()].  Both
#' functions are used internally by [gt_acs()].
#'
#' @inheritParams gt_params
#' @param col_est,col_moe Column names for the estimate and margin of error
#'   values in the table data.
#' @param col_labels Column name used for one or more columns passed to
#'   [cols_label_ext()]
#' @param columns If `NULL` (default), columns is set to `c(col_est, col_moe)`.
#'   If spanner is `NULL`, columns is passed to [cols_merge_uncert_ext()] and
#'   must be a length 2 character vector.
#' @param spanner If `NULL`, gt table is passed to [cols_merge_uncert_ext()]. If
#'   not `NULL`, spanner is passed to the label parameter of
#'   [gt::tab_spanner()].
#' @inheritParams gt::fmt_number
#' @param ... Additional parameters passed to [gt::fmt_number()] by
#'   [fmt_acs_estimate()] or to [gt::fmt_percent()] by [fmt_acs_percent()].
#' @inheritParams rlang::args_error_context
#' @family gt table
#' @keywords gt
#' @export
#' @importFrom gt fmt_number tab_spanner
fmt_acs_estimate <- function(gt_object,
                             col_est = "estimate",
                             col_moe = "moe",
                             columns = NULL,
                             col_labels = "Est.",
                             spanner = NULL,
                             decimals = 0,
                             use_seps = TRUE,
                             ...,
                             call = caller_env()) {
  columns <- columns %||% c(col_est, col_moe)

  gt_object <- cols_label_ext(
    gt_object,
    columns = columns,
    col_labels = NULL
  )

  gt_object <- gt::fmt_number(
    gt_object,
    columns = any_of(columns),
    decimals = decimals,
    use_seps = use_seps,
    ...
  )

  if (is_null(spanner)) {
    return(cols_merge_uncert_ext(gt_object, columns = columns))
  }

  gt::tab_spanner(gt_object, spanner, columns = columns)
}

#' @rdname fmt_acs_estimate
#' @name fmt_acs_percent
#' @export
#' @importFrom gt fmt_percent tab_spanner
fmt_acs_percent <- function(gt_object,
                            col_est = "perc_estimate",
                            col_moe = "perc_moe",
                            columns = NULL,
                            col_labels = "% share",
                            spanner = NULL,
                            decimals = 0,
                            use_seps = TRUE,
                            ...,
                            call = caller_env()) {
  columns <- columns %||% c(col_est, col_moe)

  gt_object <- cols_label_ext(
    gt_object,
    columns,
    col_labels = NULL,
    call = call
  )

  gt_object <- gt::fmt_percent(
    gt_object,
    columns = any_of(columns),
    decimals = decimals,
    use_seps = use_seps,
    ...
  )

  if (is_null(spanner)) {
    return(cols_merge_uncert_ext(gt_object, columns))
  }

  check_character(spanner, call = call)

  gt::tab_spanner(
    gt_object,
    label = spanner,
    columns = columns
  )
}

#' @rdname fmt_acs_estimate
#' @name cols_label_ext
#' @details Using cols_label_ext
#' [cols_label_ext()] is a variant on [gt::cols_label()] used by
#' [fmt_acs_estimate()] and [fmt_acs_percent()].
#' @export
#' @importFrom gt cols_label
cols_label_ext <- function(gt_object,
                           columns = NULL,
                           col_labels = NULL,
                           call = caller_env()) {
  if (is_null(col_labels)) {
    return(gt_object)
  }

  check_character(columns, call = call)

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
