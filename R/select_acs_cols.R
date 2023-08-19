#' Check if x is a data frame with the expected ACS column names
#'
#' @noRd
check_acs_cols <- function(x,
                           value_col = "estimate",
                           moe_col = "moe",
                           perc_prefix = "perc",
                           perc_sep = "_",
                           check_perc = TRUE,
                           ...,
                           arg = caller_arg(x),
                           call = caller_env()) {
  check_data_frame(x, arg = arg, call = call)

  nm <- c(value_col, moe_col)

  if (check_perc) {
    nm <- c(nm, acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep))
  }

  if (all(has_name(x, nm))) {
    return(invisible(NULL))
  }

  cli_abort(
    "{.arg {arg}} is missing one or more required column names: {.val {nm}}",
    call = call
  )
}

#' Select columns from an ACS data frame using `dplyr::select()`
#'
#' `r lifecycle::badge("deprecated")`
#'
#' [select_acs_cols()] is a wrapper for [dplyr::select()] designed to select the
#' appropriate columns for a gt table created with [gt_acs()]. Set any named
#' parameter to `NULL` to drop the respective column or use the additional `...`
#' parameter to modify the selection.
#'
#' @inheritParams gt_acs
#' @param ... Additional parameters passed to [dplyr::select()]
#' @param name_col,column_title_col,value_cols,perc_value_cols ACS data column
#'   names to select using [tidyselect::any_of()]. Set any parameter to `NULL`
#'   to avoid selecting columns.
#' @param denominator_start Passed to [starts_with()] to drop denominator
#'   columns. Defaults to "denominator"
#' @param keep_denominator If `FALSE` (default), drop all columns that start
#'   with the text from denominator_start
#' @keywords internal
#' @export
#' @importFrom dplyr select
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

#' Select columns from an ACS data frame using `dplyr::select()`
#'
#' `r lifecycle::badge("experimental")`
#'
#' [select_acs()] is a wrapper for [dplyr::select()] designed to select the
#' appropriate columns for a gt table created with [gt_acs()]. Set any named
#' parameter to `NULL` to drop the respective column or use the additional `...`
#' parameter to modify the selection. This function replaces the deprecated
#' [select_acs_cols()] function.
#'
#' @name select_acs
#' @param ... Additional parameters passed to [dplyr::select()]
#' @param .name_col,.column_title_col,.value_col,.moe_col ACS data column
#'   names to select using [tidyselect::any_of()]. Set any parameter to `NULL`
#'   to avoid selecting columns.
#' @param .perc_prefix,.perc_sep Percent value prefix and separator. Set
#'   .perc_prefix to `NULL` to drop the percent value and percent margin of
#'   error columns.
#' @export
#' @importFrom dplyr select
select_acs <- function(.data,
                       ...,
                       .name_col = "NAME",
                       .column_title_col = "column_title",
                       .value_col = "estimate",
                       .moe_col = "moe",
                       .perc_prefix = "perc",
                       .perc_sep = "_",
                       .fn = any_of) {
  nm <- c(.name_col, .column_title_col, .value_col, .moe_col)

  if (!is_null(.perc_prefix)) {
    nm <- c(nm, acs_perc_cols(.value_col, .moe_col, .perc_prefix, .perc_sep))
  }

  dplyr::select(.data, .fn(nm), ...)
}
