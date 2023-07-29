#' Assorted formatting functions for ACS tables
#'
#' @rdname fmt_acs
#' @name fmt_acs_county_col
#' @export
#' @importFrom dplyr all_of
#' @importFrom gt fmt
#' @importFrom stringr str_remove
fmt_acs_county <- function(data,
                           state,
                           pattern = ", {state}",
                           name_col = "NAME",
                           columns = dplyr::all_of(name_col),
                           ...) {

  if (inherits(data, "gt_tbl")) {
    gt::fmt(
      data,
      columns = columns,
      ...,
      fns = function(x){
        stringr::str_remove(x, glue(pattern))
      }
    )
  } else if (is.data.frame(data)) {
    dplyr::mutate(
      data,
      "{name_col}" := stringr::str_remove(.data[[name_col]], glue(pattern))
    )
  }
}
