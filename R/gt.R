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
#' @family gt table
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

#' Format estimate and margin of error columns (or % estimate and % margin of
#' error columns) in a gt table
#'
#' [fmt_acs_estimate()] formats estimate and margin of error columns for a gt
#' table. Used by [gt_acs()]
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
#'   not `NULL`, spanner is passed to the label parameter of [gt::tab_spanner()].
#' @inheritParams gt::fmt_number
#' @param ... Additional parameters passed to [gt::fmt_number()] by
#'   [fmt_acs_estimate()] or to [gt::fmt_percent()] by [fmt_acs_percent()].
#' @inheritParams rlang::args_error_context
#' @family gt table
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
    col_labels = col_labels
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

  gt_object <- cols_label_ext(gt_object, columns, col_labels, call = call)

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

#' Format American Community Survey estimate and percent estimate columns for a
#' gt table
#'
#' Create or format a gt table with an estimate and margin of error and
#' (optionally) percent estimate and margin of error value. Use in combination
#' with the [select_acs_cols()] helper function to prep data before creating a
#' table.
#'
#' @inheritParams gt::gt
#' @param est_cols,est_col_label,est_spanner Passed to columns, col_labels, and
#'   spanner parameter for [fmt_acs_estimate()]
#' @param perc_cols,perc_col_label,perc_spanner Passed to columns, col_labels,
#'   and spanner parameter for [fmt_acs_percent()]
#' @param combined_spanner If not `NULL`, combined_spanner is passed to label
#'   parameter of [gt::tab_spanner()] using both est_cols and perc_cols as the
#'   columns parameter.
#' @inheritParams fmt_acs_estimate
#' @param column_title_col,column_title_label Column title and label. If
#'   column_title_label is a string, column_title_col is required.
#'   column_title_label can also be a named vector in the format of `c("label" =
#'   "column")`. column_title_col defaults to "column_title"
#' @param name_col,name_col_label Place name column and label. name_col_label
#'   can be a string or a named vector (similar to column_title_label). name_col
#'   defaults to "NAME"
#' @inheritParams tab_acs_source_note
#' @family gt table
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   data <- get_acs_tables(
#'     geography = "county",
#'     county = "Baltimore city",
#'     state = "MD",
#'     table = "B08134"
#'   )
#'
#'   tbl_data <- filter_acs(data, indent == 1, line_number <= 10)
#'   tbl_data <- select_acs_cols(tbl_data)
#'
#'   gt_acs(
#'     tbl_data,
#'     column_title_label = "Commute time",
#'     table = "B08134"
#'   )
#' }
#' }
#' @export
#' @importFrom gt gt tab_spanner cols_label
gt_acs <- function(data,
                   ...,
                   est_cols = c("estimate", "moe"),
                   est_col_label = "Est.",
                   perc_cols = c("perc_estimate", "perc_moe"),
                   perc_col_label = "% share",
                   est_spanner = NULL,
                   perc_spanner = NULL,
                   combined_spanner = NULL,
                   decimals = 0,
                   column_title_col = "column_title",
                   column_title_label = NULL,
                   name_col = "NAME",
                   name_col_label = NULL,
                   source_note = NULL,
                   append_note = FALSE,
                   survey = "acs5",
                   year = 2021,
                   table = NULL,
                   prefix = "Source: ",
                   end = ".") {
  gt_object <- data

  if (!inherits(data, "gt_tbl")) {
    gt_object <- gt::gt(data, ...)
  }

  if (identical(column_title_label, "from_table")) {
    metadata <- get_acs_metadata(survey, year, table = table)

    if (has_length(unique(metadata[["simple_table_title"]]), 1)) {
      column_title_label <- metadata[["simple_table_title"]]
    }
  }

  if (!is_null(est_cols)) {
    if (all(is.na(gt_object[["_data"]][[est_cols[[1]]]]))) {
      gt_object <- gt::cols_hide(
        gt_object,
        dplyr::any_of(est_cols)
      )
    } else {
      gt_object <- fmt_acs_estimate(
        gt_object,
        columns = est_cols,
        col_labels = est_col_label,
        decimals = decimals,
        spanner = est_spanner
      )
    }
  }

  if (!is_null(perc_cols)) {
    if (all(is.na(gt_object[["_data"]][[perc_cols[[1]]]]))) {
      gt_object <- gt::cols_hide(
        gt_object,
        dplyr::any_of(perc_cols)
      )
    } else {
      gt_object <- fmt_acs_percent(
        gt_object,
        columns = perc_cols,
        col_labels = perc_col_label,
        decimals = decimals,
        spanner = perc_spanner
      )
    }
  }

  if (!is_null(combined_spanner)) {
    gt_object <- gt::tab_spanner(
      gt_object,
      label = combined_spanner,
      columns = any_of(c(est_cols, perc_cols))
    )
  }

  if (!is_null(column_title_label)) {
    if (!is_named(column_title_label)) {
      stopifnot(
        has_name(gt_object[["_data"]], column_title_col)
      )

      column_title_label <- set_names(column_title_label, column_title_col)
    }

    gt_object <- gt::cols_label(
      gt_object,
      .list = column_title_label
    )
  }

  if (!is_null(name_col_label)) {
    if (!is_named(name_col_label)) {
      stopifnot(
        has_name(gt_object[["_data"]], name_col)
      )

      name_col_label <- set_names(name_col_label, name_col)
    }

    gt_object <- gt::cols_label(
      gt_object,
      .list = name_col_label
    )
  }

  tab_acs_source_note(
    gt_object = gt_object,
    source_note = source_note,
    append_note = append_note,
    survey = survey,
    year = year,
    table = table,
    prefix = prefix,
    end = end
  )
}


#' @noRd
gt_acs_bind_geographies <- function(data,
                                    names = NULL,
                                    name_labels = NULL,
                                    table = NULL,
                                    name_col = "name",
                                    column_title_col = "column_title",
                                    column_title_label = NULL,
                                    est_col_label = "Households",
                                    perc_col_label = "% of total",
                                    source_note = NULL) {
  stopifnot(
    has_name(data, name_col)
  )

  names <- names %||% unique(data[[name_col]])

  stopifnot(
    length(names) > 1
  )

  names_df <- data.frame()
  col_names <- list()

  for (i in seq_along(names)) {
    bind_names_df <- filter_acs(
      data,
      table = table,
      .data[[name_col]] == names[[i]]
    )

    if (i > 1) {
      column_title_col <- NULL
    }

    bind_names_df <- select_acs_cols(
      data = data,
      name_col = NULL,
      column_title_col = column_title_col
    )

    col_names[[i]] <- paste0(names[[i]], "_", names(bind_names_df))

    names_df <- purrr::list_rbind(
      list(
        names_df,
        set_names(bind_names_df, col_names[[i]])
      )
    )
  }

  # for (i in seq_along(names)) {
  #
  #   paste0(names[[i]], names(bind_names_df))))
  #
  #   tbls[[i]] <- getACS::gt_acs(
  #     nm_data,
  #     est_col_label = est_col_label,
  #     perc_col_label = perc_col_label,
  #     combined_spanner = nm,
  #     table = table,
  #     source_note = source_note,
  #     column_title_label = column_title_label
  #   )
  # }
  #

  names_df
}
