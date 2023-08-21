#' Create `perc` value and moe column names with prefix and separator
#'
#' @param perc_prefix Prefix string for percent value columns.
#' @param perc_sep Separator string between `perc_prefix` and the `value_col`
#'   and `moe_col` strings.
#' @keywords internal
acs_perc_cols <- function(value_col = "estimate",
                          moe_col = "moe",
                          perc_prefix = "perc",
                          perc_sep = "_") {
  # FIXME: I'm not so sure about this as a design pattern
  if (is.null(perc_prefix)) {
    return(NULL)
  }

  paste0(perc_prefix, perc_sep, c(value_col, moe_col))
}

#' Pivot a ACS data frame into a wider format by name or other columns
#'
#' [pivot_acs_wider()] wraps [tidyr::pivot_wider()] and makes it easy to convert
#' an ACS data frame into a wide format by changing the value of the
#' `names_from` parameter.
#'
#' @param name_col Name column. Defaults to "NAME". Ignored if names_from is not
#'   set to name_col.
#' @inheritParams acs_perc_cols
#' @inheritParams cols_acs_label
#' @inheritParams tidyr::pivot_wider
#' @inheritDotParams tidyr::pivot_wider
#' @export
#' @importFrom tidyr pivot_wider
pivot_acs_wider <- function(data,
                            name_col = "NAME",
                            value_col = "estimate",
                            moe_col = "moe",
                            perc_prefix = "perc",
                            perc_sep = "_",
                            names_from = name_col,
                            values_from = NULL,
                            names_sep = "_",
                            names_vary = "slowest",
                            names_glue = NULL,
                            names_repair = "check_unique",
                            .col_fn = any_of,
                            ...) {
  tidyr::pivot_wider(
    data,
    names_from = names_from,
    values_from = values_from %||%
      .col_fn(
        c(
          value_col, moe_col,
          acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep)
        )
      ),
    names_vary = names_vary,
    names_glue = names_glue,
    names_repair = names_repair,
    ...
  )
}
