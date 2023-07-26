#' Replace placeholder jam values in an ACS data frame
#'
#' Currently only supports variable B25035_001 from the Median Year Structure
#' Built table.
#'
#' @param data Data frame with ACS data
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

#' Format ACS columns using different units
#'
#' @keywords internal
#' @export
fmt_acs_minutes <- function(data,
                            column_title = "column_title") {

  data[[column_title]] <- stringr::str_remove(data[[column_title]], "[:space:]minutes$")

  data
}
