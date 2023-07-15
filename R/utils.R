utils::globalVariables(
  c(
    "cols", "column_id", "column_title", "denominator_estimate",
    "denominator_moe", "estimate", "moe", "table_id", "table_title", "variable"
  )
)

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
