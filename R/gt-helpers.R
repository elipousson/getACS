#' Hide columns where all values are NA
#'
#' @noRd
cols_hide_na <- function(data, columns) {
  all_is_na_data <- dplyr::select(
    data[["_data"]],
    where(
      function(x) {
        all(is.na(x))
      }
    )
  )

  if (ncol(all_is_na_data) == 0) {
    return(data)
  }

  gt::cols_hide(
    data = data,
    columns = dplyr::any_of(names(all_is_na_data))
  )
}
