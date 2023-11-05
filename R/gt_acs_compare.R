
#' Create a gt table with values compared by name, geography, or variable
#'
#' [gt_acs_compare()] is a variant of [gt_acs()] that uses [pivot_acs_wider()]
#' to support comparisons of multiple named areas or multiple geographies
#' side-by-side in a combined gt table.
#'
#' @inheritParams pivot_acs_wider
#' @inheritParams cols_acs_label
#' @inheritParams select_acs
#' @inheritParams fmt_acs_values
#' @inheritParams tab_acs_spanner_delim
#' @inheritParams tab_acs_source_note
#' @param id_cols Defaults to `column_title_col`. See [tidyr::pivot_longer()]
#'   for details.
#' @param hide_na_cols If `TRUE` (default), hide any columns with all `NA`
#'   values using [gt::cols_hide()].
#' @param use_spanner If `TRUE` (default), create spanners for the comparison
#'   geographies.
#' @name gt_acs_compare
#' @family gt table
#' @keywords gt
#' @export
#' @importFrom withr with_environment
#' @importFrom rlang current_env
#' @importFrom gt cols_label fmt_number fmt_percent cols_merge_uncert
#'   tab_spanner_delim
gt_acs_compare <- function(data,
                           name_col = "NAME",
                           value_col = "estimate",
                           moe_col = "moe",
                           perc_prefix = "perc",
                           perc_sep = "_",
                           variable_col = "variable",
                           column_title_col = "column_title",
                           value_label = "Est.",
                           moe_label = "MOE",
                           perc_value_label = "% share",
                           perc_moe_label = "% MOE",
                           column_title_label = NULL,
                           id_cols = column_title_col,
                           id_expand = FALSE,
                           names_from = name_col,
                           values_from = NULL,
                           names_vary = "slowest",
                           names_glue = NULL,
                           names_sep = "_",
                           decimals = 0,
                           currency_value = FALSE,
                           merge_moe = TRUE,
                           split = "last",
                           limit = 1,
                           reverse = TRUE,
                           source_note = NULL,
                           append_note = FALSE,
                           hide_na_cols = TRUE,
                           survey = "acs5",
                           year = 2021,
                           table = NULL,
                           prefix = "Source: ",
                           end = ".",
                           use_md = FALSE,
                           use_spanner = TRUE,
                           ...) {
  data <- pivot_acs_wider(
    data,
    id_cols = id_cols,
    id_expand = id_expand,
    name_col = name_col,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    names_from = names_from,
    values_from = values_from,
    names_sep = names_sep,
    names_vary = names_vary,
    names_glue = names_glue
  )

  gt_object <- .gt_ext(data, ..., hide_na_cols = hide_na_cols)

  gt_object <- cols_acs_label(
    gt_object,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep,
    column_title_col = column_title_col,
    value_label = value_label,
    moe_label = moe_label,
    perc_value_label = perc_value_label,
    perc_moe_label = perc_moe_label,
    column_title_label = column_title_label
  )

  gt_object <- fmt_acs_values(
    gt_object,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    decimals = decimals,
    perc_sep = perc_sep,
    merge_moe = merge_moe,
    currency_value = currency_value
  )

  # FIXME: is this still needed if hide_na is part of the gt wrapper function?
  gt_object <- .cols_hide_na(gt_object)

  if (use_spanner) {
    gt_object <- tab_acs_spanner_delim(
      gt_object,
      column_title_col = column_title_col,
      value_col = value_col,
      moe_col = moe_col,
      perc_prefix = perc_prefix,
      delim = names_sep,
      split = split,
      limit = limit,
      reverse = reverse
    )
  }

  gt_object <- tab_acs_source_note(
    gt_object = gt_object,
    source_note = source_note,
    append_note = append_note,
    survey = survey,
    year = year,
    table = table,
    prefix = prefix,
    end = end,
    use_md = use_md
  )

  gt_object
}


#' @rdname gt_acs_compare
#' @name gt_acs_compare_vars
#' @export
gt_acs_compare_vars <- function(data,
                                name_col = "NAME",
                                value_col = "estimate",
                                moe_col = "moe",
                                perc_prefix = "perc",
                                perc_sep = "_",
                                variable_col = "variable",
                                column_title_col = "column_title",
                                value_label = NULL,
                                moe_label = "MOE",
                                id_cols = name_col,
                                names_from = variable_col,
                                values_from = c(value_col, moe_col),
                                use_spanner = FALSE,
                                ...) {

  if (is.null(value_label)) {
    value_label <- c(
      set_names(
        unique(data[[column_title_col]]),
        paste0(value_col, perc_sep, unique(data[[names_from]]))
      )
    )
  }

  gt_acs_compare(
    data,
    name_col = name_col,
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep,
    variable_col = variable_col,
    column_title_col = column_title_col,
    value_label = value_label,
    moe_label = moe_label,
    id_cols = id_cols,
    names_from = variable_col,
    values_from = values_from,
    use_spanner = use_spanner,
    ...
  )
}
