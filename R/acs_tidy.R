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
#' `names_from` parameter. The default parameter value vary from the tidyr
#' version with `names_vary = "slowest"` and `values_from = NULL` (replaced by
#' using the `.col_fn` `{tidyselect}` function on the named value and percent
#' value columns). You may need to retain the variable column and set `id_cols =
#' "variable"` if the column_title does not uniquely identify rows after
#' widening the input data.
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
                            .col_fn = any_of,
                            ...,
                            id_cols = NULL,
                            id_expand = FALSE,
                            names_from = name_col,
                            names_sep = "_",
                            names_glue = NULL,
                            names_vary = "slowest",
                            names_repair = "check_unique",
                            values_from = NULL) {
  tidyr::pivot_wider(
    data,
    id_cols = id_cols,
    id_expand = id_expand,
    names_from = names_from,
    names_sep = names_sep,
    names_glue = names_glue,
    names_vary = names_vary,
    names_repair = names_repair,
    values_from = values_from %||%
      .col_fn(
        c(
          value_col, moe_col,
          acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep)
        )
      ),
    ...
  )
}
