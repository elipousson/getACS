#' Keep or drop columns from an ACS data frame using `dplyr::select()`
#'
#' `r lifecycle::badge("experimental")`
#'
#' [select_acs()] is a wrapper for [dplyr::select()] designed to select the
#' appropriate columns for a gt table created with [gt_acs()]. Set any named
#' parameter to `NULL` to drop the respective column or use the additional `...`
#' parameter to modify the selection.
#'
#' @name select_acs
#' @inheritParams dplyr::select
#' @param .name_col,.column_title_col,.value_col,.moe_col ACS data column names
#'   to select using the Tidyverse selection helper in `.fn`. Set any parameter
#'   to `NULL` to avoid selecting columns.
#' @param .perc_prefix,.perc_sep Percent value prefix and separator. Set
#'   .perc_prefix to `NULL` or `.perc = FALSE` to drop the percent value and percent margin of
#'   error columns.
#' @param .perc If `TRUE`, select the percent value and percent margin of
#'   error columns along with the supplied column values.
#' @param .fn Tidyverse selection helper to use with named ACS columns. Defaults
#'   to [tidyselect::any_of]. See [dplyr::select()] for an overview of selection
#'   features.
#' @export
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   edu_data <- get_acs_tables(
#'     "county",
#'     table = "B15003",
#'     state = "MD",
#'     county = "Baltimore city"
#'   )
#'
#'   select_acs(edu_data)
#' }
#' }
#' @importFrom dplyr select
select_acs <- function(.data,
                       ...,
                       .name_col = "NAME",
                       .column_title_col = "column_title",
                       .value_col = "estimate",
                       .moe_col = "moe",
                       .perc_prefix = "perc",
                       .perc_sep = "_",
                       .perc = TRUE,
                       .fn = any_of) {
  nm <- c(
    .name_col, .column_title_col, .value_col, .moe_col
  )

  if (!is.null(.perc_sep) && .perc) {
    nm <- c(
      nm,
      acs_perc_cols(
        value_col = .value_col,
        moe_col = .moe_col,
        perc_prefix = .perc_prefix,
        perc_sep = .perc_sep,
        perc = .perc
      )
    )
  }

  nm <- as.character(nm)

  dplyr::select(.data, .fn(nm), ...)
}
