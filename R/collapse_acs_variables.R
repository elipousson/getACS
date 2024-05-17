#' Collapse variables into a new label column using `forcats::fct_collapse()`
#'
#' [collapse_acs_variables()] uses [forcats::fct_collapse()] to aggregated
#' variables while creating a new label column. Other variables are retained in
#' list columns of unique values. The aggregated values for `perc_moe` may not
#' be accurate after transformation with this function. To group by additional
#' variables, passed a grouped data frame to data and set `.add = TRUE`.
#'
#' @param data ACS data frame input.
#' @inheritParams forcats::fct_collapse
#' @param name_col Name column name, Default: 'NAME'
#' @param variable_col Variable column name, Default: 'variable'
#' @param label_col Label column name, Default: 'label'. Label is a factor
#'   column added to the returned data frame.
#' @param value_col,moe_col Value and margin of error column names (default to
#'   "estimate" and "moe").
#' @param na.rm Passed to [sum()], Default: `TRUE`
#' @param digits Passed to [round()], Default: 2
#' @param extensive Must be `TRUE`. If `FALSE`, summarize collapsed variables
#'   using a weighted mean. This is a planned feature.
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
#'   collapse_acs_variables(
#'     edu_data,
#'     "Total" = table_vars[1],
#'     "5th Grade or less" = table_vars[5:9],
#'     "6th to 8th Grade" = table_vars[10:12],
#'     "9th to 11th Grade" = table_vars[13:15],
#'     other_level = "Other"
#'   )
#' }
#' }
#' @seealso
#'  [forcats::fct_collapse()], [camiller::add_grps()]
#' @rdname collapse_acs_variables
#' @inheritParams dplyr::group_by
#' @export
#' @importFrom dplyr group_by mutate all_of
collapse_acs_variables <- function(data,
                                   ...,
                                   other_level = NULL,
                                   name_col = "NAME",
                                   variable_col = "variable",
                                   label_col = "label",
                                   value_col = "estimate",
                                   moe_col = "moe",
                                   na.rm = TRUE,
                                   digits = 2,
                                   .add = FALSE,
                                   extensive = TRUE) {
  check_has_name(data, c(variable_col, value_col, moe_col))
  check_installed("forcats")

  if (has_name(data, name_col)) {
    check_string(name_col, allow_empty = FALSE)
    data <- dplyr::group_by(data, .data[[name_col]], .add = .add)
  }

  data <- dplyr::mutate(
    data,
    "{label_col}" := forcats::fct_inorder(.data[[variable_col]]),
    "{label_col}" := forcats::fct_collapse(
      .f = .data[[label_col]],
      ...,
      other_level = other_level
    ),
    .after = all_of(variable_col)
  )

  data <- dplyr::group_by(data, .data[[label_col]], .add = TRUE)

  # TODO: Expose extensive parameter after implementing weighted mean summary
  stopifnot(
    is_true(extensive)
  )

  if (!extensive) {
    data <- summarise_acs_intensive(
      data = data,
      name_col = name_col,
      variable_col = variable_col,
      label_col = label_col,
      value_col = value_col,
      moe_col = moe_col,
      na.rm = na.rm,
      digits = digits
    )

    return(data)
  }

  summarise_acs_extensive(
    data = data,
    name_col = name_col,
    variable_col = variable_col,
    label_col = label_col,
    value_col = value_col,
    moe_col = moe_col,
    na.rm = na.rm,
    digits = digits
  )
}

#' Summarise ACS data assuming extensive variables
#'
#' @importFrom dplyr summarise across
#' @importFrom tidycensus moe_sum
#' @noRd
summarise_acs_extensive <- function(
    data,
    name_col = "NAME",
    variable_col = "variable",
    label_col = "label",
    value_col = "estimate",
    moe_col = "moe",
    na.rm = TRUE,
    digits = 2) {
  perc_cols <- acs_perc_cols(
    value_col = value_col,
    moe_col = moe_col
  )

  # FIXME: Parameterize this code with across to allow for cases when the
  # perc_cols are not available and avoid this awkward if
  if (!all(has_name(data, perc_cols))) {
    data <- dplyr::summarise(
      data,
      "{variable_col}" := list(unique(.data[[variable_col]])),
      "{value_col}" := round(
        sum(.data[[value_col]], na.rm = na.rm),
        digits = digits
      ),
      "{moe_col}" := round(
        tidycensus::moe_sum(
          .data[[moe_col]],
          estimate = .data[[value_col]],
          na.rm = na.rm
        ),
        digits = digits
      ),
      dplyr::across(
        -any_of(
          c(name_col, label_col, variable_col, value_col, moe_col)
        ),
        function(x) {
          list(unique(x))
        }
      ),
      .groups = "drop"
    )

    return(data)
  }

  dplyr::summarise(
    data,
    "{variable_col}" := list(unique(.data[[variable_col]])),
    "{value_col}" := round(
      sum(.data[[value_col]], na.rm = na.rm),
      digits = digits
    ),
    "{moe_col}" := round(
      tidycensus::moe_sum(
        .data[[moe_col]],
        estimate = .data[[value_col]],
        na.rm = na.rm
      ),
      digits = digits
    ),
    "{perc_cols[[1]]}" := round(
      sum(.data[[perc_cols[[1]]]], na.rm = na.rm),
      digits = digits
    ),
    "{perc_cols[[2]]}" := round(
      tidycensus::moe_sum(
        .data[[perc_cols[[2]]]],
        estimate = .data[[perc_cols[[1]]]],
        na.rm = na.rm
      ),
      digits = digits
    ),
    dplyr::across(
      -any_of(
        c(name_col, label_col, variable_col, value_col, moe_col, perc_cols)
      ),
      function(x) {
        list(unique(x))
      }
    ),
    .groups = "drop"
  )
}


#' Summarise ACS data assuming intensive variables
#'
#' @importFrom dplyr summarise across
#' @noRd
summarise_acs_intensive <- function(
    data,
    name_col = "NAME",
    variable_col = "variable",
    label_col = "label",
    value_col = "estimate",
    moe_col = "moe",
    na.rm = TRUE,
    digits = 2) {
  dplyr::summarise(
    data,
    "{variable_col}" := list(unique(.data[[variable_col]])),
    # FIXME: This could be a weighted mean if some valid weight is included in
    # the dataset. As is, these values may be invalid if the relative size of
    # the collapsed groups is very different.
    "{value_col}" := round(
      mean(.data[[value_col]], na.rm = na.rm),
      digits = digits
    ),
    dplyr::across(
      -any_of(
        c(name_col, label_col, variable_col, value_col, moe_col)
      ),
      function(x) {
        list(unique(x))
      }
    ),
    .groups = "drop"
  )
}
