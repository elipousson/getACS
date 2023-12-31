#' Filter ACS data by table ID, variables, or other attributes using
#' `dplyr::filter()`
#'
#' [filter_acs()] helps to filter a data frame of American Community Survey data
#' enriched with table and column metadata using the [label_acs_metadata()]
#' function using [dplyr::filter()].
#'
#' @param data A data frame with a "table_id", "variable", and "column_title"
#'   columns.
#' @param ... Parameters passed to [dplyr::filter()]
#' @param table,column Table ID and column title values to return.
#' @param vars,drop_vars Variable IDs to keep or to drop. If table is supplied
#'   (or if data only contains data for a single table), numeric values are
#'   allowed for vars and drop_vars (e.g. if table is "B14001" and vars is 2
#'   data is filtered to variable "B14001_002").
#' @param geography Geography values to filter by.
#' @param variable_col Variable column name. Defaults to "variable".
#' @keywords internal
#' @seealso
#'  [dplyr::filter()]
#' @rdname filter_acs
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   edu_data <- get_acs_geographies(
#'     c("county", "state"),
#'     table = "B15003",
#'     state = "MD",
#'     county = "Baltimore city"
#'   )
#'
#'   edu_data |>
#'     filter_acs(vars = "B15003_017")
#'
#'   edu_data |>
#'     filter_acs(vars = 17)
#'
#'   edu_data |>
#'     filter_acs(drop_vars = 1)
#'
#'   edu_data |>
#'     filter_acs(geography = "county")
#'
#'   edu_data |>
#'     filter_acs(column = "Master's degree")
#' }
#' }
#' @export
#' @importFrom dplyr filter
filter_acs <- function(data,
                       ...,
                       table = NULL,
                       column = NULL,
                       vars = NULL,
                       drop_vars = NULL,
                       geography = NULL,
                       variable_col = "variable") {
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
      has_name(data, variable_col)
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
      !(.data[[variable_col]] %in% drop_vars)
    )
  }

  if (!is_null(vars)) {
    stopifnot(
      has_name(data, variable_col)
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
      .data[[variable_col]] %in% vars
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
