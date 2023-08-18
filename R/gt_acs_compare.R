#' Create a gt table with values compared by name, geography, or other grouping variable
#'
#' [gt_acs_compare()] is a variant of [gt_acs()] designed to support the
#' comparison of multiple named areas or multiple geographies side-by-side in a
#' combined gt table. This function *only* works if the arrangement of rows is
#' the same for each group and all groups have the same number of rows.
#'
#' @param .by Name of grouping column. Defaults to `name_col`
#' @param name_col Name column. Defaults to "NAME". Dropped from table by
#'   default.
#' @param variable_col Variable column name. Defaults to "variable". Dropped
#'   from table by default.
#' @param drop_cols Columns to drop before creating gt table. Defaults to
#'   `c(name_col, variable_col, "GEOID")`
#' @param value_col_label,perc_value_col_label Labels for value/estimate column
#'   and percent value/estimate column. Not yet implemented as an option.
#' @param return_gt If `TRUE` (default), return a formatted gt table. If
#'   `FALSE`, return the data frame for a comparison table.
#' @inheritParams select_acs_cols
#' @inheritDotParams gt_acs -perc_cols -est_spanner -perc_spanner
#'   -combined_spanner -decimals
#' @export
#' @importFrom withr with_environment
#' @importFrom rlang current_env
#' @importFrom gt cols_label fmt_number fmt_percent cols_merge_uncert
#'   tab_spanner_delim
gt_acs_compare <- function(data,
                           .by = name_col,
                           name_col = "NAME",
                           value_cols = c("estimate", "moe"),
                           perc_value_cols = c("perc_estimate", "perc_moe"),
                           column_title_col = "column_title",
                           variable_col = "variable",
                           drop_cols = c(name_col, variable_col, "GEOID"),
                           value_col_label = "Est.",
                           perc_value_col_label = "% share",
                           decimals = 0,
                           return_gt = TRUE,
                           ...) {
  acs_data_compare <- cbind_grouped_acs_data(
    data = data,
    .by = .by,
    name_col = name_col,
    value_cols = value_cols,
    perc_value_cols = perc_value_cols,
    column_title_col = column_title_col,
    variable_col = variable_col,
    drop_cols = drop_cols,
    call = call
  )

  if (!return_gt) {
    return(geography_data)
  }

  acs_data_tbl <- gt_acs(
    acs_data_compare,
    est_cols = NULL,
    perc_cols = NULL,
    est_spanner = NULL,
    perc_spanner = NULL,
    combined_spanner = NULL,
    name_col = NULL,
    name_col_label = NULL,
    ...
  )

  acs_data_tbl <- withr::with_environment(
    env = rlang::current_env(),
    {
      gt::cols_label(
        acs_data_tbl,
        .list = list2(
          ends_with(value_cols[[1]]) ~ value_col_label,
          ends_with(perc_value_cols) ~ perc_value_col_label
        )
      )
    }
  )

  acs_data_tbl <- acs_data_tbl |>
    gt::fmt_number(
      columns = ends_with(value_cols),
      decimals = decimals
    ) |>
    gt::fmt_percent(
      columns = ends_with(perc_value_cols),
      decimals = decimals
    ) |>
    gt::cols_merge_uncert(
      col_val = ends_with(value_cols[[1]]),
      col_uncert = ends_with(value_cols[[2]])
    ) |>
    gt::cols_merge_uncert(
      col_val = ends_with(perc_value_cols[[1]]),
      col_uncert = ends_with(perc_value_cols[[2]])
    )

  gt::tab_spanner_delim(
    acs_data_tbl,
    delim = "_",
    columns = -any_of(column_title_col),
    split = "first",
    limit = 1
  )
}


#' @noRd
#' @importFrom dplyr group_by group_rows group_keys ungroup rename_with select
#'   any_of all_of ends_with any_of
#' @importFrom vctrs vec_chop vec_cbind
#' @importFrom purrr map2 list_cbind
cbind_grouped_acs_data <- function(data,
                                   .by = name_col,
                                   name_col = "NAME",
                                   value_cols = c("estimate", "moe"),
                                   perc_value_cols = c("perc_estimate", "perc_moe"),
                                   column_title_col = "column_title",
                                   variable_col = "variable",
                                   drop_cols = c(name_col, variable_col, "GEOID"),
                                   call = caller_env()) {
  check_name(.by, call = call)

  grouped_acs_data <- dplyr::group_by(data, .data[[.by]])

  acs_data_list <- vctrs::vec_chop(
    grouped_acs_data,
    indices = dplyr::group_rows(grouped_acs_data)
  )

  acs_data_list <- purrr::map2(
    acs_data_list,
    dplyr::group_keys(grouped_acs_data)[[.by]],
    function(acs_data, group_nm) {
      acs_data <- dplyr::ungroup(acs_data)

      acs_data <- dplyr::rename_with(
        acs_data,
        function(nm) {
          paste0(group_nm, "_", nm)
        },
        any_of(c(ends_with(value_cols), ends_with(perc_value_cols)))
      )

      acs_data <- dplyr::select(acs_data, -dplyr::any_of(drop_cols))

      select_acs_cols(
        acs_data,
        name_col = NULL,
        column_title_col = NULL,
        value_cols = paste0(group_nm, "_", value_cols),
        perc_value_cols = paste0(group_nm, "_", perc_value_cols),
        denominator_start = "denominator",
        keep_denominator = FALSE
      )
    }
  )

  vctrs::vec_cbind(
    dplyr::select(
      data,
      dplyr::all_of(column_title_col)
    )[seq(nrow(acs_data_list[[1]])), ],
    purrr::list_cbind(acs_data_list),
    .error_call = call
  )
}
