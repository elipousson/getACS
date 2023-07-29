utils::globalVariables(
  c(
    "cols", "column_id", "column_title", "denominator_estimate",
    "denominator_moe", "estimate", "moe", "table_id", "table_title", "variable"
  )
)

#' @noRd
check_sf <- function(x, allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (inherits(x, "sf")) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    what = "sf",
    allow_null = allow_null,
    call = call
  )
}

#' Helper function to select columns typically used in an ACS table
#'
#' @inheritParams gt_acs
#' @param ... Additional parameters passed to [dplyr::select()]
#' @param name_col,column_title_col,est_cols,perc_est_cols ACS data column names
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
                            est_cols = c("estimate", "moe"),
                            perc_est_cols = c("perc_estimate", "perc_moe"),
                            denominator_start = "denominator",
                            keep_denominator = FALSE) {
  data <- dplyr::select(
    data,
    any_of(c(name_col, column_title_col, est_cols, perc_est_cols)),
    ...
  )

  if (keep_denominator) {
    return(data)
  }

  dplyr::select(data, -starts_with(denominator_start))
}

#' Helper for recoding based on a named list
#'
#' @keywords internal
#' @export
fct_recode_with_list <- function(x, list = NULL, in_order = TRUE, ordered = NA) {
  check_installed("forcats")

  x <- forcats::fct_recode(x, !!!list)

  if (!in_order) {
    return(x)
  }

  forcats::fct_inorder(x, ordered = ordered)
}
