#' @noRd
acs_perc_cols <- function(value_col = "estimate",
                          moe_col = "moe",
                          perc_prefix = "perc",
                          perc_sep = "_") {
  paste0(perc_prefix, perc_sep, c(value_col, moe_col))
}

#' Pivot a ACS data frame into a wider format by name or other columns
#'
#' [acs_pivot_wider()] wraps [tidyr::pivot_wider()] and makes it easy to convert
#' an ACS data frame into a wide format by changing the value of the
#' `names_from` parameter.
#'
#' @inheritParams select_acs_cols
#' @inheritParams tidyr::pivot_wider
#' @inheritDotParams tidyr::pivot_wider
#' @export
#' @importFrom tidyr pivot_wider
acs_pivot_wider <- function(data,
                            name_col = "NAME",
                            value_col = "estimate",
                            moe_col = "moe",
                            perc_prefix = "perc",
                            perc_sep = "_",
                            names_from = name_col,
                            values_from = any_of(
                              c(
                                value_col, moe_col,
                                acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep)
                              )
                            ),
                            names_sep = "_",
                            names_vary = "slowest",
                            names_glue = NULL,
                            ...) {
  tidyr::pivot_wider(
    data,
    names_from = names_from,
    values_from = values_from,
    names_vary = names_vary,
    names_glue = names_glue,
    ...
  )
}
