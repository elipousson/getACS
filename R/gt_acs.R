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
#'   `column_title_label` is a string, `column_title_col` is required.
#'   `column_title_label` can also be a named vector in the format of `c("label"
#'   = "column")`. `column_title_col` defaults to "column_title". If
#'   `column_title_label` is "from_table", the label is set based on the
#'   simple_table_title column in the table metadata.
#' @param name_col,name_label Place name column and label. `name_label`
#'   can be a string or a named vector (similar to `column_title_label`).
#'   `name_col` defaults to "NAME"
#' @param est_spanner,perc_spanner Spanner labels for estimate and percent
#'   estimate columns.
#' @param est_cols,perc_cols Deprecated. Estimate and percent estimate columns.
#' @inheritParams fmt_acs_values
#' @inheritParams tab_acs_source_note
#' @inheritParams cols_acs_label
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
                   currency_value = FALSE,
                   survey = "acs5",
                   year = 2022,
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
  perc_cols <- perc_cols %||%
    acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep)

  if (identical(column_title_label, "from_table")) {
    metadata <- get_acs_metadata(survey, year, table = table)

    if (has_length(unique(metadata[["simple_table_title"]]), 1)) {
      column_title_label <- unique(metadata[["simple_table_title"]])
    }
  }

  gt_object <- fmt_acs_values(
    gt_object,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep,
    currency_value = currency_value
  )

  gt_object <- cols_acs_label(
    gt_object,
    value_col = value_col,
    value_label = value_label,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep,
    perc_value_label = perc_value_label,
    column_title_col = column_title_col,
    column_title_label = column_title_label,
    name_col = name_col,
    name_label = name_label
  )

  if (!is_null(combined_spanner)) {
    gt_object <- gt::tab_spanner(
      gt_object,
      label = combined_spanner,
      columns = any_of(c(est_cols, perc_cols))
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
