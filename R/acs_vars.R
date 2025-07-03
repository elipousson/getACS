#' Load ACS variables with [tidycensus::load_variables()]
#'
#' [load_acs_vars()] calls [tidycensus::load_variables()] and then combines the
#' returned data frame with the Census Reporter metadata from
#' [label_acs_table_metadata()]. The function can optionally filter the variable
#' definitions to a set of tables and variables or drop variables from the
#' results.
#'
#' @inheritParams label_acs_table_metadata
#' @inheritParams tidycensus::load_variables
#' @param geography_levels Ordered vector of geography levels used to convert
#'   the geography column returned by [tidycensus::load_variables()] into a
#'   factor. Default: c("block", "block group", "tract", "county", "state",
#'   "us")
#' @param table Table ID to return.
#' @inheritParams filter_acs
#' @returns A data frame with ACS variables definitions.
#' @seealso
#'  [tidycensus::load_variables()]
#' @rdname load_acs_vars
#' @export
#' @importFrom tidycensus load_variables
#' @importFrom dplyr rename mutate
load_acs_vars <- function(
  year = 2022,
  survey = "acs5",
  cache = TRUE,
  variable_col = "variable",
  geography_levels = c(
    "block",
    "block group",
    "tract",
    "county",
    "state",
    "us"
  ),
  table = NULL,
  vars = NULL,
  drop_vars = NULL
) {
  survey <- acs_survey_match(survey)

  acs_var_data <- tidycensus::load_variables(
    year = year,
    dataset = survey,
    cache = cache
  )

  acs_var_data <- dplyr::rename(
    acs_var_data,
    "{variable_col}" := name
  )

  if (!is.null(geography_levels)) {
    acs_var_data <- dplyr::mutate(
      acs_var_data,
      geography = factor(
        .data[["geography"]],
        levels = geography_levels,
        ordered = TRUE
      )
    )
  }

  acs_var_data <- label_acs_table_metadata(
    acs_var_data,
    survey = survey,
    year = year,
    variable_col = variable_col
  )

  filter_acs(
    acs_var_data,
    table = table,
    vars = vars,
    drop_vars = drop_vars
  )
}
