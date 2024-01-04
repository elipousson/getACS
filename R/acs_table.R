#' Append a set of race iteration codes to an ACS table ID
#'
#' [acs_table_race_iteration()] uses the `race_iteration` reference data to
#' create or validate race iteration codes and create race iteration table IDs.
#'
#' @name acs_table_race_iteration
#' @param table An ACS table ID string.
#' @param codes Character vector of race iteration codes to return. If `NULL`
#'   (default), codes is set to `c("", race_iteration[["code"]])`.
#' @inheritParams rlang::args_error_context
#' @returns A character vector of variable ID values for a single table.
#' @seealso [acs_table_variables()]
#' @examples
#' acs_table_race_iteration("B25003")
#' @export
acs_table_race_iteration <- function(table,
                                     codes = NULL,
                                     error_call = caller_env()) {
  check_string(table, call = error_call)

  code_options <- c("", race_iteration[["code"]])

  if (is.null(codes)) {
    codes <- code_options
  } else {
    codes <- arg_match(codes, code_options, error_call = error_call)
  }

  check_character(codes, call = error_call)

  paste0(table, codes)
}

#' Convert an ACS table ID to a set of variable ID values
#'
#' [acs_table_variables()] helps to make a vector of variable ID values based on
#' a table ID string. The returned variable IDs use the format returned by
#' [tidycensus::get_acs()], e.g. "{table_id}_{line_number}" where the
#' line_number is a width 3 string prefixed by "0". If variables is `NULL`, the
#' function calls [get_acs_metadata()] with `metadata = "column"` and returns
#' all available variables for the table for the supplied year and survey. Note
#' that the `sep` and `width` parameters should *not* be changed if you are
#' working with data from the `{tidycensus}` package.
#'
#' @name acs_table_variables
#' @param table An ACS table ID string.
#' @param data If data is provided and table is `NULL`, table is set based on
#'   the unique values in the "table_id" column of data. If data contains more
#'   than one table_id value, the function will error
#' @param variables A numeric vector corresponding to the line number of the
#'   variables.
#' @param sep A separator character between the table ID string and variable ID
#'   values.
#' @param width Variable ID suffix width.
#' @inheritParams get_acs_metadata
#' @inheritParams rlang::args_error_context
#' @inheritParams rlang::args_error_context
#' @returns A character vector of variable ID values for a single table.
#' @seealso [acs_table_race_iteration()]
#' @examples
#' acs_table_variables(table = "B15003")
#'
#' acs_table_variables(table = "B15003", variables = c(1:5))
#' @export
#' @importFrom stringr str_pad
acs_table_variables <- function(table = NULL,
                                variables = NULL,
                                data = NULL,
                                survey = "acs5",
                                year = 2022,
                                sep = "_",
                                width = 3,
                                error_call = caller_env()) {
  if (is.data.frame(data) && is.null(table)) {
    stopifnot(has_name(data, "table_id"))
    table <- unique(data[["table_id"]])
  }

  check_string(table, call = error_call)

  if (is.null(variables)) {
    table_metadata <- get_acs_metadata(
      survey = survey,
      year = year,
      metadata = "column",
      table = table,
      error_call = error_call,
      quiet = TRUE
    )

    variables <- table_metadata[["line_number"]]
  }

  paste0(table, sep, stringr::str_pad(variables, width = width, pad = "0"))
}
