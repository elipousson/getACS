#' Format place names or column titles in a gt table or data frame with ACS data
#'
#' [fmt_acs_county()] is helpful for stripping the state name from county-level
#' ACS data and [fmt_acs_minutes()] does the same for a column with a duration
#' (e.g. commute times). If data is not a `gt_tbl` object, both function can use
#' [dplyr::mutate()] to transform a standard data frame.
#'
#' @rdname fmt_acs
#' @param name_col Name for column with place name values. Defaults to "NAME"
#' @param state State name. Required if state is included in pattern.
#' @param pattern Passed to [glue::glue()] and [stringr::str_replace()] for
#'   [fmt_acs_county()] or just to [stringr::str_replace()] by
#'   [fmt_acs_minutes()]. Defaults to `", {state}"` which strips the state name
#'   from a column of county-level name values or `"[:space:]minutes$"` which
#'   strips the trailing text for minutes.
#' @param replacement Passed to [stringr::str_replace()]. Defaults to `""`.
#' @inheritParams gt::fmt
#' @inheritDotParams gt::fmt
#' @name fmt_acs_county
#' @keywords gt
#' @export
#' @importFrom dplyr all_of
#' @importFrom gt fmt
#' @importFrom stringr str_remove
fmt_acs_county <- function(data,
                           state = NULL,
                           pattern = ", {state}",
                           replacement = "",
                           name_col = "NAME",
                           columns = all_of(name_col),
                           ...) {
  if (is.null(state) && any(data[["geography"]] == "state")) {
    state <- unique(
      data[data[["geography"]] == "state", name_col]
    )
  }

  pattern <- glue(pattern)

  fmt_str_replace(
    data = data,
    pattern = pattern,
    replacement = replacement,
    col = name_col,
    columns = columns,
    ...
  )
}

#' @rdname fmt_acs
#' @name fmt_acs_minutes
#' @param column_title_col Column title column.
#' @export
fmt_acs_minutes <- function(data,
                            pattern = "[:space:]minutes$",
                            replacement = "",
                            column_title_col = "column_title",
                            columns = all_of(column_title_col),
                            ...) {
  fmt_str_replace(
    data = data,
    pattern = pattern,
    replacement = replacement,
    col = column_title_col,
    columns = columns,
    ...
  )
}

#' @noRd
fmt_str_replace <- function(data,
                            pattern,
                            replacement,
                            col = "NAME",
                            columns = all_of(col),
                            ...) {
  if (inherits(data, "gt_tbl")) {
    gt::fmt(
      data,
      columns = columns,
      ...,
      fns = function(x) {
        stringr::str_replace(x, pattern, replacement)
      }
    )
  } else if (is.data.frame(data)) {
    dplyr::mutate(
      data,
      "{col}" := stringr::str_replace(
        .data[[col]],
        pattern, replacement
      )
    )
  }
}

#' Format jam values in an estimate column of a gt table or ACS data frame
#'
#' Currently only supports variable B25035_001 from the Median Year Structure
#' Built table.
#'
#' @param data Data frame with ACS data
#' @name fmt_acs_jam_values
#' @seealso
#' - [fmt_acs_county()]
#' - [fmt_acs_minutes()]
#' @keywords gt
#' @export
#' @importFrom dplyr filter mutate if_else
#' @importFrom cli cli_bullets
fmt_acs_jam_values <- function(data) {
  stopifnot(
    all(has_name(data, c("estimate", "variable")))
  )

  if (nrow(
    dplyr::filter(
      data,
      (variable == "B25035_001"), (estimate == 0)
    )
  ) > 0) {
    cli::cli_bullets(
      c(
        "i" = "For the {.val Median Year Structure Built} table,
        {.val 0} is a placeholder for an estimate of 1939 or earlier.",
        "!" = "Replacing {.val 0} values with {.val 1939}"
      )
    )
  }

  dplyr::mutate(
    data,
    estimate = dplyr::case_when(
      (variable == "B25035_001") & (estimate == 0) ~ 1939,
      .default = estimate
    )
  )
}
