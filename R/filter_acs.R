#' Filter American Community Survey data by table, variables, or other attributes
#'
#' A data frame of American Community Survey data enriched with table and column
#' metadata using the [label_acs_metadata()] function.
#'
#' @param data A data frame with a "table_id", "variable", and "column_title"
#'   columns.
#' @param ... Parameters passed to [dplyr::filter()]
#' @param table,column Table ID and column title values to return.
#' @param vars,drop_vars Variable IDs to keep or to drop. If table is supplied
#'   (or if data only contains data for a single table), numeric values are
#'   allowed for vars and drop_vars (e.g. if table is "B14001" and vars is 2
#'   data is filtered to variable "B14001_002").
#' @keywords internal
#' @seealso
#'  [dplyr::filter()]
#' @rdname filter_acs
#' @export
#' @importFrom dplyr filter
filter_acs <- function(data,
                       ...,
                       table = NULL,
                       column = NULL,
                       vars = NULL,
                       drop_vars = NULL,
                       geography = NULL) {
  if (!is_null(geography)) {
    stopifnot(
      has_name(data, "geography")
    )

    data <- data[data[["geography"]] %in% geography, ]
  }

  if (!is_null(table)) {
    stopifnot(
      has_name(data, "table_id")
    )

    data <- dplyr::filter(
      data,
      table_id %in% table
    )
  }

  if (!is_null(drop_vars)) {
    stopifnot(
      has_name(data, "variable")
    )

    if (!is.character(drop_vars)) {
      drop_vars <- acs_table_variables(
        table = table,
        variables = drop_vars,
        data = data
        )
    }

    data <- dplyr::filter(
      data,
      !(.data[["variable"]] %in% drop_vars)
    )
  }

  if (!is_null(vars)) {
    stopifnot(
      has_name(data, "variable")
    )

    if (!is.character(vars)) {
      vars <- acs_table_variables(
        table = table,
        variables = vars,
        data = data
        )
    }

    data <- dplyr::filter(
      data,
      .data[["variable"]] %in% vars
    )
  }

  if (!is_null(column)) {
    stopifnot(
      has_name(data, "column_title")
    )

    data <- dplyr::filter(
      data,
      column_title %in% column
    )
  }

  dplyr::filter(data, ...)
}
