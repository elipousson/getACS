#' Variant of `gt::cols_label()` for ACS
#'
#' @param value_col Value column name. Defaults to "estimate"
#' @param value_label Value column label. Defaults to "Est.".
#' @param moe_col Margin of error column name. Defaults to "moe"
#' @param moe_label Margin of error column label. Defaults to "MOE".
#' @inheritParams acs_perc_cols
#' @param perc_value_label Percent value column label.
#' @param perc_moe_label Percent margin of error column label.
#' @param variable_col Variable column name. Defaults to "variable". Typically,
#'   dropped from table by default.
#' @param column_title_col,column_title_label Column title column name and
#'   label. Defaults to  "column_title" and `NULL`.
#' @param .col_fn tidyselect function to use with column names. Defaults to
#'   [tidyselect::starts_with],
#' @inheritParams withr::with_environment
#' @keywords internal
#' @export
#' @importFrom tidyselect starts_with
cols_acs_label <- function(data,
                           value_col = "estimate",
                           value_label = "Est.",
                           moe_col = "moe",
                           moe_label = "MOE",
                           perc_prefix = "perc",
                           perc_sep = "_",
                           perc_value_label = "% share",
                           perc_moe_label = "% MOE",
                           perc = TRUE,
                           name_col = "NAME",
                           name_label = NULL,
                           column_title_col = "column_title",
                           column_title_label = NULL,
                           .col_fn = starts_with,
                           env = NULL) {
  perc_cols <- acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep, perc)

  perc_value_col <- NULL
  perc_moe_col <- NULL
  if (!is.null(perc_cols)) {
    perc_value_col <- perc_cols[[1]]
    perc_moe_col <- perc_cols[[2]]
  }

  data <- .cols_label_ext(
    data,
    columns = value_col,
    label = value_label,
    .col_fn = .col_fn,
    env = env
  )

  data <- .cols_label_ext(
    data,
    columns = moe_col,
    label = moe_label,
    .col_fn = .col_fn,
    env = env
  )

  data <- .cols_label_ext(
    data,
    columns = perc_value_col,
    label = perc_value_label,
    .col_fn = .col_fn,
    env = env
  )

  data <- .cols_label_ext(
    data,
    columns = perc_moe_col,
    label = perc_moe_label,
    .col_fn = .col_fn,
    env = env
  )

  data <- .cols_label_ext(
    data,
    columns = name_col,
    label = name_label,
    .col_fn = .col_fn,
    env = env
  )

  .cols_label_ext(
    data,
    columns = column_title_col,
    label = column_title_label,
    .col_fn = .col_fn,
    env = env
  )
}

#' Hide columns where all values are NA
#'
#' @noRd
.cols_hide_na <- function(data, columns = NULL) {
  all_is_na_data <- dplyr::select(
    data[["_data"]],
    where(
      function(x) {
        all(is.na(x))
      }
    )
  )

  if (ncol(all_is_na_data) == 0) {
    return(data)
  }

  gt::cols_hide(
    data = data,
    columns = columns %||% any_of(names(all_is_na_data))
  )
}

#' Extended version of `gt::cols_label()`
#'
#' @noRd
.cols_label_ext <- function(data,
                            columns = NULL,
                            label = NULL,
                            .col_fn = starts_with,
                            env = NULL,
                            ...) {
  if (is_null(label) || is_null(columns)) {
    return(data)
  }

  # if (!is_named(label) && is_string(label) && is_string(columns)) {
  #   label <- set_names(columns, nm = label)
  # }

  if (is_named(label)) {
    data <- gt::cols_label(
      data,
      .list = label,
      ...
    )

    return(data)
  }


  withr::with_environment(
    env = env %||% current_env(),
    {
      gt::cols_label(
        data,
        .list = list2(
          .col_fn(columns) ~ label
        ),
        ...
      )
    }
  )
}

#' Create a table with `gt::gt()`
#'
#' @inheritParams gt::gt
#' @param drop_geometry If `TRUE` (default) and data is an sf object, drop
#'   geometry before turning the data frame into a table.
#' @keywords internal
#' @importFrom sf st_drop_geometry
#' @importFrom gt gt
.gt_ext <- function(data,
                    rownames_to_stub = FALSE,
                    row_group_as_column = FALSE,
                    ...,
                    drop_geometry = TRUE,
                    hide_na_cols = TRUE) {
  if (inherits(data, "gt_tbl")) {
    return(gt)
  }

  if (drop_geometry && inherits(data, "sf")) {
    check_installed("sf")
    data <- sf::st_drop_geometry(data)
  }

  gt_object <- gt::gt(
    data,
    rownames_to_stub = rownames_to_stub,
    row_group_as_column = row_group_as_column,
    ...
  )


  if (hide_na_cols) {
    gt_object <- .cols_hide_na(gt_object)
  }

  gt_object
}
