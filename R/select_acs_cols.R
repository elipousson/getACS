#' Select columns from an ACS data frame using `dplyr::select()`
#'
#' [select_acs_cols()] is a wrapper for [dplyr::select()] designed to select the
#' appropriate columns for a gt table created with [gt_acs()]. Set any named
#' parameter to `NULL` to drop the respective column or use the additional `...`
#' parameter to modify the selection.
#'
#' @inheritParams gt_acs
#' @param ... Additional parameters passed to [dplyr::select()]
#' @param name_col,column_title_col,value_cols,perc_value_cols ACS data column names
#'   to select using [tidyselect::any_of()]. Set any parameter to `NULL` to
#'   avoid selecting columns.
#' @param denominator_start Passed to [starts_with()] to drop denominator
#'   columns. Defaults to "denominator"
#' @param keep_denominator If `FALSE` (default), drop all columns that start
#'   with the text from denominator_start
#' @export
#' @importFrom dplyr select
#' @importFrom tidyselect any_of starts_with
select_acs_cols <- function(data,
                            ...,
                            name_col = "NAME",
                            column_title_col = "column_title",
                            value_cols = c("estimate", "moe"),
                            perc_value_cols = c("perc_estimate", "perc_moe"),
                            denominator_start = "denominator",
                            keep_denominator = FALSE) {
  data <- dplyr::select(
    data,
    any_of(c(name_col, column_title_col, value_cols, perc_value_cols)),
    ...
  )

  if (keep_denominator) {
    return(data)
  }

  dplyr::select(data, -starts_with(denominator_start))
}
