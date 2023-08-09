#' Collapse variables into a new label column using `forcats::fct_collapse()`
#'
#' [collapse_acs_variables()] uses [forcats::fct_collapse()] to aggregated
#' variables while creating a new label column. Other variables are retained in
#' list columns of unique values. The aggregated values for `perc_moe` may not
#' be accurate after transformation with this function.
#'
#' @param data ACS data frame input.
#' @inheritParams forcats::fct_collapse
#' @param name_col Name column name, Default: 'NAME'
#' @param variable_col Variable column name, Default: 'variable'
#' @param column_title_col Column title column name, Default: 'column_title'
#' @param label_col Label column name, Default: 'label'. Label is a factor
#'   column added to the returned data frame.
#' @param na.rm Passed to [sum()], Default: `TRUE`
#' @param digits Passed to [digits()], Default: 2
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   edu_data <- get_acs_tables(
#'     "county",
#'     table = "B15003",
#'     state = "MD",
#'     county = "Baltimore city"
#'   )
#'
#'   table_vars <- acs_table_variables("B15003")
#'
#'   collapse_acs_variables(edu_data,
#'     "total" = table_vars[1],
#'     "second" = table_vars[2],
#'     "third and fourth" = table_vars[3:4],
#'     "next 5" = table_vars[5:10],
#'     other_level = "Other"
#'   )
#' }
#' }
#' @seealso
#'  [forcats::fct_collapse()], [camiller::add_grps()]
#' @rdname collapse_acs_variables
#' @export
#' @importFrom dplyr group_by mutate all_of summarise across any_of
#' @importFrom forcats fct_inorder fct_collapse
#' @importFrom tidycensus moe_sum
collapse_acs_variables <- function(data,
                                   ...,
                                   other_level = NULL,
                                   name_col = "NAME",
                                   variable_col = "variable",
                                   label_col = "label",
                                   na.rm = TRUE,
                                   digits = 2) {
  stopifnot(
    all(has_name(data, c(variable_col, "estimate", "moe",
                         "perc_estimate", "perc_moe")))
  )

  if (has_name(data, name_col)) {
    data <- dplyr::group_by(data, .data[[name_col]])
  }

  data <- dplyr::mutate(
    data,
    "{label_col}" := forcats::fct_inorder(.data[[variable_col]]),
    "{label_col}" := forcats::fct_collapse(
      .f = .data[[label_col]],
      ...,
      other_level = other_level
    ),
    .after = dplyr::all_of(variable_col)
  )

  if (has_name(data, name_col)) {
    data <- dplyr::group_by(data, .data[[name_col]], .data[[label_col]])
  } else {
    data <- dplyr::group_by(data, .data[[label_col]])
  }

  dplyr::summarise(
    data,
    "{variable_col}" := list(unique(.data[[variable_col]])),
    estimate = round(sum(estimate, na.rm = na.rm), digits = digits),
    moe = round(
      tidycensus::moe_sum(moe, estimate = estimate, na.rm = na.rm),
      digits = digits
    ),
    perc_estimate = round(sum(perc_estimate, na.rm = na.rm), digits = digits),
    perc_moe = round(
      tidycensus::moe_sum(perc_moe, estimate = perc_estimate, na.rm = na.rm),
      digits = digits
    ),
    dplyr::across(
      -dplyr::any_of(c(name_col, label_col, variable_col, "estimate", "moe")),
      function(x) {
        list(unique(x))
      }
    ),
    .groups = "drop"
  )
}
