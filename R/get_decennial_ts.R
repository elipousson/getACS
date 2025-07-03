#' Load decennial variables
#'
#' [load_decennial_vars()] downloads metadata for the specified year and dataset
#' using [tidycensus::load_variables()] then creates new `table_id`, `indent`,
#' `line_number`, and `column_title` columns similar to those provided in the
#' prepared Census Reporter metadata used by [label_acs_metadata()].
#'
#' @inheritParams tidycensus::load_variables
#' @keywords internal
load_decennial_vars <- function(
  year = 2020,
  dataset = NULL,
  cache = TRUE,
  quiet = FALSE
) {
  if (length(dataset) > 1) {
    if (length(year) == 1) {
      var_list <- purrr::map(
        dataset,
        \(x) {
          load_decennial_vars(year, x, cache, quiet)
        }
      )
    } else {
      params <- vctrs::vec_recycle_common(year = year, dataset = dataset)

      var_list <- purrr::map2(
        params[["dataset"]],
        params[["year"]],
        \(x, y) {
          load_decennial_vars(y, x, cache, quiet)
        }
      )
    }

    return(purrr::list_rbind(var_list))
  }

  possibly_load_variables <- purrr::possibly(
    tidycensus::load_variables,
    otherwise = FALSE,
    quiet = quiet
  )

  var_list <- purrr::map(
    rlang::set_names(year, year),
    \(x, ...) {
      x_dataset <- "sf1"
      if (x == 2020) {
        x_dataset <- "pl"
      }

      x_dataset <- dataset %||% x_dataset

      df <- possibly_load_variables(
        year = x,
        dataset = x_dataset,
        cache = cache
      )

      if (isFALSE(df)) {
        return(NULL)
      }

      df <- df |>
        dplyr::mutate(
          dataset = x_dataset,
          year = as.character(x)
        )

      x_dataset <- NULL

      df
    }
  )

  var_list <- vctrs::list_drop_empty(var_list)

  if (rlang::is_empty(var_list)) {
    return(invisible(NULL))
  }

  vars <- var_list |>
    purrr::list_rbind() |>
    dplyr::rename(
      variable = name
    ) |>
    dplyr::mutate(
      table_id = dplyr::if_else(
        year != 2000,
        stringr::str_sub(variable, end = 2),
        stringr::str_sub(variable, end = 5)
      ),
      label = stringr::str_trim(label),
      label = stringr::str_remove(label, "^!!"),
      indent = stringr::str_count(label, "!!")
    ) |>
    dplyr::mutate(
      line_number = dplyr::row_number(),
      .by = c("year", "dataset", "table_id")
    )

  wider_names <- paste0(
    "column_title",
    c(seq.int(max(vars[["indent"]]), 1), "")
  )

  vars |>
    tidyr::separate_wider_delim(
      label,
      names = wider_names,
      delim = "!!",
      too_few = "align_end",
      too_many = "drop",
      cols_remove = FALSE
    ) |>
    dplyr::select(
      !dplyr::matches("[1-9]$")
    ) |>
    dplyr::mutate(
      column_title = stringr::str_remove(
        column_title,
        ":$"
      )
    )
}


#' Get multiple years of decennial US Census data for time series analysis
#'
#' [get_decennial_ts()] is a wrapper for [tidycensus::get_decennial()] to handle
#' time series data.
#'
#' @param variables If any year value is 2020, variables must be the same length
#'   as year with each value corresponding to one of the years requested. This
#'   is a temporary requirement to address the mismatch between the available
#'   data for 2000 and 2010 relative to 2020.
#'   Default: `NULL`
#' @param year If year is length 1, it is treated as the max year and decennial
#'   Census years back to 2000, are added to the vector of requested years.
#'   Default: 2020
#' @param label If `TRUE` (default), use [label_decennial_data()] to add
#'   formatted label columns to the decennial Census data frame.
#' @inheritParams tidycensus::get_decennial
#' @inheritDotParams tidycensus::get_decennial
#' @returns A data frame with decennial Census data.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   md_counties <- get_decennial_ts(
#'     geography = "county",
#'     variables = c("P001001", "P001001", "P1_001N"),
#'     year = 2020,
#'     county = "Baltimore city",
#'     state = "MD",
#'     geometry = FALSE
#'   )
#' }
#' }
#' @seealso
#'  [tidycensus::get_decennial()]
#' @rdname get_decennial_ts
#' @export
#' @importFrom tidycensus get_decennial
#' @importFrom vctrs vec_cbind
get_decennial_ts <- function(
  geography,
  variables = NULL,
  table = NULL,
  cache_table = TRUE,
  year = 2020,
  sumfile = NULL,
  state = NULL,
  county = NULL,
  geometry = FALSE,
  summary_var = NULL,
  label = TRUE,
  ...
) {
  params <- get_geography_params(
    geography,
    year = year,
    state = state,
    county = county,
    allow_decennial = TRUE,
    call = call
  )

  #   if (has_length(year, 1)) {
  #     allow_years <- c(2000, 2010, 2020)
  #     if (!all(year %in% allow_years)) {
  #       cli::cli_abort(
  #         c("{.arg year} can't include any values other than {allow_years}.",
  #           "i" = "Try using NHGIS for earlier decennial Census data:
  #         {.url https://www.nhgis.org/}"
  #         )
  #       )
  #     }
  #     # year <- seq(2000, year, by = 10)
  #   }

  # if (all((2020 %in% year)) && (length(year) != length(variables))) {
  #   cli_abort(
  #     "{.arg variables} must be the same length as {.arg year} if
  #     year values include 2020."
  #   )
  # }

  # if (length(county) > 1) {
  #   # FIXME: Figure out how to handle multiple counties
  #   county_year_grid <- vctrs::vec_expand_grid(county = county, year = year)
  # }

  decennial_list <- vec_tidycensus(
    year = year,
    !!!params,
    variables = variables,
    table = table,
    cache_table = cache_table,
    sumfile = sumfile,
    geometry = geometry,
    summary_var = summary_var,
    ...,
    .fn = tidycensus::get_decennial
  )

  decennial_df <- list_rbind(
    set_names(decennial_list, year),
    names_to = "year"
  )

  decennial_df <- vctrs::vec_cbind(
    decennial_df,
    as.data.frame(do.call(cbind, params)),
    .error_call = call
  )

  if (label) {
    decennial_df <- label_decennial_data(
      decennial_df,
      year = year,
      dataset = sumfile
    )
  }

  decennial_df
}

#' Label Decennial Census data
#'
#' [label_decennial_data()] left joins the data frame of variables from
#' [load_decennial_vars()] to the input data.
#'
#' @inheritParams load_decennial_vars
#' @inheritDotParams load_decennial_vars
#' @keywords internal
label_decennial_data <- function(data, year = 2020, dataset = NULL, ...) {
  vars <- load_decennial_vars(year, dataset = dataset, ...)

  data |>
    dplyr::left_join(
      vars
    )
}
