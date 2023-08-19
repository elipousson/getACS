#' Create a gt table with formatted ACS estimate and percent estimate columns
#'
#' Create or format a gt table with an estimate and margin of error and
#' (optionally) percent estimate and margin of error value. Use in combination
#' with the [select_acs()] helper function to prep data before creating a
#' table.
#'
#' @inheritParams .gt_ext
#' @inheritParams acs_perc_cols
#' @param combined_spanner If not `NULL`, combined_spanner is passed to label
#'   parameter of [gt::tab_spanner()] using the value columns and percent
#'   columns as the columns parameter.
#' @inheritParams fmt_acs_estimate
#' @param column_title_col,column_title_label Column title and label. If
#'   column_title_label is a string, column_title_col is required.
#'   column_title_label can also be a named vector in the format of `c("label" =
#'   "column")`. column_title_col defaults to "column_title"
#' @param name_col,name_label Place name column and label. name_label
#'   can be a string or a named vector (similar to column_title_label). name_col
#'   defaults to "NAME"
#' @inheritParams fmt_acs_estimate
#' @inheritParams tab_acs_source_note
#' @param hide_na_cols If `TRUE` (default), hide columns where all values are
#'   `NA`.
#' @family gt table
#' @keywords gt
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
#'   tbl_data <- select_acs(tbl_data)
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
#' @importFrom sf st_drop_geometry
gt_acs <- function(data,
                   rownames_to_stub = FALSE,
                   row_group_as_column = FALSE,
                   ...,
                   value_col = "estimate",
                   moe_col = "moe",
                   perc_prefix = "perc",
                   perc_sep = "_",
                   column_title_col = "column_title",
                   name_col = "NAME",
                   perc_value_label = "% share",
                   value_label = "Est.",
                   column_title_label = NULL,
                   name_label = NULL,
                   est_spanner = NULL,
                   perc_spanner = NULL,
                   combined_spanner = NULL,
                   decimals = 0,
                   source_note = NULL,
                   append_note = FALSE,
                   drop_geometry = TRUE,
                   hide_na_cols = TRUE,
                   survey = "acs5",
                   year = 2021,
                   table = NULL,
                   prefix = "Source: ",
                   end = ".",
                   est_cols = NULL,
                   perc_cols = NULL) {
  gt_object <- .gt_ext(
    data,
    rownames_to_stub = rownames_to_stub,
    row_group_as_column = row_group_as_column,
    ...
  )

  est_cols <- est_cols %||% c(value_col, moe_col)
  perc_cols <- perc_cols %||% acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep)

  if (identical(column_title_label, "from_table")) {
    metadata <- get_acs_metadata(survey, year, table = table)

    if (has_length(unique(metadata[["simple_table_title"]]), 1)) {
      column_title_label <- unique(metadata[["simple_table_title"]])
    }
  }

  if (!is_null(est_cols)) {
    gt_object <- fmt_acs_estimate(
      gt_object,
      columns = est_cols,
      col_labels = value_label,
      decimals = decimals,
      spanner = est_spanner
    )
  }

  if (!is_null(perc_cols)) {
    gt_object <- fmt_acs_percent(
      gt_object,
      columns = perc_cols,
      col_labels = perc_col_label,
      decimals = decimals,
      spanner = perc_spanner
    )
  }

  gt_object <- acs_cols_label(
    gt_object,
    value_col = value_col,
    value_label = value_label,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep,
    perc_value_label = perc_value_label,
    column_title_col = column_title_col,
    column_title_label = column_title_label
  )

  if (!is_null(combined_spanner)) {
    gt_object <- gt::tab_spanner(
      gt_object,
      label = combined_spanner,
      columns = any_of(c(est_cols, perc_cols))
    )
  }

  # gt_object <- .cols_label_ext(
  #   gt_object,
  #   columns = column_title_col,
  #   label = column_title_label
  # )
  #
  #   gt_object <- .cols_label_ext(
  #     gt_object,
  #     columns = any_of(name_col),
  #     label = name_label
  #   )

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

#' Create a gt table with values compared by name, geography, or variable
#'
#' [gt_acs_compare()] is a variant of [gt_acs()] that uses [acs_pivot_wider()]
#' to support comparisons of multiple named areas or multiple geographies
#' side-by-side in a combined gt table.
#'
#' @inheritParams acs_pivot_wider
#' @inheritParams acs_cols_label
#' @inheritParams select_acs
#' @inheritParams acs_cols_label
#' @inheritParams acs_fmt_values
#' @inheritParams acs_tab_spanner_delim
#' @inheritParams tab_acs_source_note
#' @name gt_acs_compare
#' @export
#' @importFrom withr with_environment
#' @importFrom rlang current_env
#' @importFrom gt cols_label fmt_number fmt_percent cols_merge_uncert
#'   tab_spanner_delim
gt_acs_compare <- function(data,
                           name_col = "NAME",
                           value_col = "estimate",
                           moe_col = "moe",
                           perc_prefix = "perc",
                           perc_sep = "_",
                           variable_col = "variable",
                           column_title_col = "column_title",
                           value_label = "Est.",
                           moe_label = "MOE",
                           perc_value_label = "% share",
                           perc_moe_label = "% MOE",
                           column_title_label = NULL,
                           names_from = name_col,
                           values_from = any_of(
                             c(
                               value_col, moe_col,
                               paste0(perc_prefix, perc_sep, c(value_col, moe_col))
                             )
                           ),
                           names_vary = "slowest",
                           names_glue = NULL,
                           split = "last",
                           limit = 1,
                           names_sep = "_",
                           reverse = TRUE,
                           decimals = 0,
                           merge_moe = TRUE,
                           source_note = NULL,
                           append_note = FALSE,
                           hide_na_cols = TRUE,
                           survey = "acs5",
                           year = 2021,
                           table = NULL,
                           prefix = "Source: ",
                           end = ".",
                           ...) {
  data <- acs_pivot_wider(
    data,
    name_col = name_col,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    names_from = names_from,
    values_from = values_from,
    names_sep = names_sep,
    names_vary = names_vary,
    names_glue = names_glue
  )

  gt_object <- .gt_ext(data, ...)

  gt_object <- acs_cols_label(
    gt_object,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep,
    column_title_col = column_title_col,
    value_label = value_label,
    moe_label = moe_label,
    perc_value_label = perc_value_label,
    perc_moe_label = perc_moe_label,
    column_title_label = column_title_label
  )

  gt_object <- acs_fmt_values(
    gt_object,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    decimals = decimals,
    perc_sep = perc_sep,
    merge_moe = merge_moe
  )

  gt_object <- .cols_hide_na(gt_object)

  gt_object <- acs_tab_spanner_delim(
    gt_object,
    column_title_col = column_title_col,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    delim = names_sep
  )

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
