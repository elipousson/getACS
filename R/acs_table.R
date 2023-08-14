
#' Append race iteration codes to a table ID
#'
#' @param table Table ID.
#' @name acs_table_race_iteration
#' @keywords internal
#' @export
acs_table_race_iteration <- function(table,
                                     codes = NULL,
                                     error_call = caller_env()) {
  check_string(table, call = error_call)
  codes <- codes %||% c("", race_iteration[["code"]])
  check_character(codes, call = error_call)

  paste0(table, codes)
}

#' Convert a table ID into a set of variable ID using a numeric vector for
#' variables
#'
#' [acs_table_variables()] supports the creation of variable ID values based on
#' a table ID string. The returned variable IDs use the format returned by
#' [tidycensus::get_acs()], e.g. "{table_id}_{line_number}" where the
#' line_number is a width 3 string prefixed by "0". If variables is `NULL`, the
#' function calls [get_acs_metadata()] with `metadata = "column"` and returns
#' all available variables for the table for the supplied year and survey.
#'
#' @param table A table ID string.
#' @param data If data is provided and table is `NULL`, table is set based on
#'   the unique values in the "table_id" column of data. If data contains more
#'   than one table_id value, the function will error
#' @param variables A numeric vector corresponding to the line number of the
#'   variables.
#' @inheritParams get_acs_metadata
#' @inheritParams stringr::str_pad
#' @inheritParams rlang::args_error_context
#' @name acs_table_variables
#' @returns A character vector of variable ID values.
#' @export
#' @importFrom stringr str_pad
acs_table_variables <- function(table = NULL,
                                variables = NULL,
                                data = NULL,
                                survey = "acs5",
                                year = 2021,
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

  paste0(table, "_", stringr::str_pad(variables, width = width, pad = "0"))
}
