#' Make crosswalk data for U.S. Census blocks and tracts
#'
#' [make_block_xwalk()] joined U.S. Census blocks data from [tigris::blocks()]
#' to a data frame from [tigris::tracts()] to provide a crosswalk between both
#' geographies. If `year = 2020`, the suffix parameter is not used. If year is
#' any other year than 2020, the by parameter must be changed from the default
#' value of `c("TRACTCE20" = "TRACTCE")`. 2020 is also the only year where
#' [tigris::blocks()] includes the population and household count data required
#' to use this crosswalk data frame with [make_area_xwalk()].
#'
#' @inheritParams tigris::blocks
#' @param by Specification of join variables in the format of c("block column
#'   name for tract" = "tract column name"). Passed to [dplyr::left_join()].
#' @inheritDotParams tigris::blocks
#' @param keep_zipped_shapefile Passed to [tigris::blocks()] and
#'   [tigris::tracts()] to keep and re-use the zipped shapefile.
#' @param crs Coordinate reference system to return.
#' @param ... Additional parameters passed to [tigris::blocks()] and
#'   [tigris::tracts()].
#' @export
#' @importFrom cli cli_progress_step
#' @importFrom tigris blocks tracts
#' @importFrom dplyr left_join
#' @importFrom sf st_drop_geometry st_make_valid st_crs st_transform
make_block_xwalk <- function(state,
                             county = NULL,
                             year = 2020,
                             by = c("TRACTCE20" = "TRACTCE"),
                             keep_zipped_shapefile = TRUE,
                             suffix = c("_block", "_tract"),
                             crs = NULL,
                             ...) {
  cli::cli_progress_step(
    "Downloading blocks"
  )

  block_sf <- tigris::blocks(
    state = state,
    county = county,
    year = year,
    keep_zipped_shapefile = keep_zipped_shapefile,
    ...
  )

  cli::cli_progress_step(
    "Downloading tracts"
  )

  tract_sf <- tigris::tracts(
    state = state,
    county = county,
    year = year,
    keep_zipped_shapefile = keep_zipped_shapefile,
    ...
  )

  block_xwalk <- dplyr::left_join(
    x = block_sf,
    y = sf::st_drop_geometry(tract_sf),
    by = by,
    suffix = suffix
  )

  block_xwalk <- sf::st_make_valid(block_xwalk)

  if (is.null(crs)) {
    return(block_xwalk)
  }

  sf::st_transform(block_xwalk, crs = sf::st_crs(crs))
}

#' Make and use crosswalk data based on U.S. Census block-level weights for U.S.
#' Census tracts and non-Census geographic areas
#'
#' `make_area_xwalk()` creates a crosswalk data frame based on the `weight_col`
#' parameter (if `year = 2020`, use "POP20" for population, "HOUSING20" for
#' households, or "ALAND20" for land area). Using this function with other
#' years, requires users to add population data to the block_xwalk as the
#' [tigris::blocks()] function only includes population and household count data
#' for the 2020 year. This function has also not been tested when areas include
#' overlapping geometry and the results may be invalid for those overlapping
#' areas if that is the case.
#'
#' @inheritParams make_block_xwalk
#' @param area A sf object with an arbitrary geography overlapping with the
#'   block_xwalk. Required. If area only partly overlaps with block_xwalk,
#'   add_coverage should be set to `TRUE` (default).
#' @param block_xwalk Block-tract crosswalk sf object. If `NULL`, state is
#'   required to create a crosswalk using [make_block_xwalk()]
#' @param weight_col Column name in input block_xwalk to use for weighting.
#'   Generated weight_col used by [use_area_xwalk()] should be the same as the
#'   weight_col for [make_area_xwalk()] but include the "perc_" prefix. Defaults
#'   to "HOUSING20" for [make_block_xwalk()] and "perc_HOUSING20" for
#'   [use_area_xwalk()].
#' @param name_col Name column in area.
#' @param geoid_col,tract_col GeoID for Census tract and Census tract ID column
#'   in block_xwalk
#' @param digits Digits to use for percent share of weight value.
#' @param add_coverage If `TRUE` (default), it is assumed that area does not
#'   cover the full extent of the block_xwalk and an additional feature is added
#'   with the difference between the unioned area geometry and unioned
#'   block_xwalk geometry. This additional coverage ensures that blocks are
#'   accurately assigned to this alternate geography but it is excluded from the
#'   returned data frame.
#' @param erase_water If `TRUE`, apply [tigris::erase_water()] to input area and
#'   block_xwalk before joining. Defaults to `FALSE`.
#' @param keep_geometry If `TRUE`, area_xwalk is a sf object with the same
#'   geometry as the input area.
#' @param crs Coordinate reference system to use for input data. Recommended to
#'   set to a projected CRS if input area data is in a geographic CRS.
#' @param ... Passed to [make_block_xwalk()].
#' @returns A tibble or a sf object.
#' @seealso [tidycensus::interpolate_pw()], [areal::aw_interpolate()]
#' @export
#' @importFrom dplyr select all_of filter group_by summarise across mutate
#'   ungroup left_join
#' @importFrom tigris erase_water
#' @importFrom cli cli_progress_step
#' @importFrom vctrs vec_rbind
#' @importFrom sf st_make_valid st_as_sf st_transform st_crs st_join
#'   st_drop_geometry
make_area_xwalk <- function(area,
                            block_xwalk = NULL,
                            state = NULL,
                            county = NULL,
                            year = 2020,
                            name_col = "NAME",
                            weight_col = "HOUSING20",
                            geoid_col = "GEOID",
                            tract_col = "TRACTCE20",
                            by = c("TRACTCE20" = "TRACTCE"),
                            suffix = c("_block", "_tract"),
                            digits = 2,
                            extensive = TRUE,
                            add_coverage = TRUE,
                            erase_water = FALSE,
                            keep_geometry = FALSE,
                            crs = NULL,
                            ...) {
  check_name(name_col)
  check_name(tract_col)
  check_name(weight_col)
  check_character(by)
  check_sf(area)

  if (!has_name(area, name_col)) {
    cli_abort(
      "{.arg area} is missing the supplied {.arg name_col}: {.val {name_col}}"
    )
  }

  block_xwalk <- block_xwalk %||%
    make_block_xwalk(
      state = state,
      county = county,
      year = year,
      by = by,
      suffix = suffix,
      ...
    )

  check_sf(block_xwalk)

  area <- sf::st_make_valid(area)

  if (keep_geometry) {
    area_geometry <- dplyr::select(area, dplyr::all_of(name_col))
  }

  if (!is.null(crs)) {
    block_xwalk <- sf::st_transform(block_xwalk, crs = crs)
    area <- sf::st_transform(area, crs = crs)
  } else {
    block_xwalk <- sf::st_transform(block_xwalk, sf::st_crs(area))
  }

  if (erase_water) {
    area <- tigris::erase_water(area)
    block_xwalk <- tigris::erase_water(block_xwalk)
  }

  if (add_coverage) {
    cli::cli_progress_step("Adding coverage for {.arg block_xwalk}")

    coverage_name <- tempfile(tmpdir = "")

    area_coverage <- try_fetch(
      st_make_valid_coverage(block_xwalk, area),
      error = function(cnd) {
        cli_abort(
          c("Valid spatial coverage for the area of {.arg block_xwalk} outside the {.arg area_xwalk} can't be created.",
            "*" = "Set {.code add_coverage = FALSE} and try again."
          ),
          parent = cnd
        )
      }
    )

    if (is_empty(area_coverage)) {
      cli_abort(
        c("{.arg area} is not covered by {.arg block_xwalk}",
          "i" = "Supply a {.arg block_xwalk} covering the full geometry of
          {.arg area} or set {.arg add_coverage} to {.code FALSE}"
        )
      )
    }

    area_coverage <- data.frame(
      coverage_name,
      area_coverage
    )

    area <- vctrs::vec_rbind(
      area,
      set_names(area_coverage, c(name_col, "geometry"))
    )

    area <- sf::st_make_valid(sf::st_as_sf(area))
  }

  cli::cli_progress_step("Joining {.arg block_xwalk} to {.arg area}")

  area_xwalk <- suppressWarnings(sf::st_join(
    block_xwalk,
    area,
    left = FALSE,
    largest = TRUE,
    suffix = c("_block", "")
  ))

  area_xwalk <- sf::st_drop_geometry(area_xwalk)

  cli::cli_progress_step(
    "Summarizing {weight_col} by {tract_col} and {name_col}"
  )

  area_xwalk <- dplyr::filter(
    area_xwalk,
    .data[[weight_col]] > 0
  )

  area_xwalk <- dplyr::group_by(
    area_xwalk,
    .data[[geoid_col]], .data[[tract_col]], .data[[name_col]]
  )

  area_xwalk <- dplyr::summarise(
    area_xwalk,
    dplyr::across(
      dplyr::all_of(weight_col),
      function(x) {
        sum(x, na.rm = TRUE)
      }
    ),
    .groups = "drop_last"
  )

  area_xwalk <- dplyr::mutate(
    area_xwalk,
    "perc_{weight_col}" := round(
      .data[[weight_col]] / sum(.data[[weight_col]], na.rm = TRUE),
      digits = digits
    )
  )

  area_xwalk <- dplyr::ungroup(area_xwalk)

  if (add_coverage) {
    area_xwalk <- dplyr::filter(area_xwalk, .data[[name_col]] != coverage_name)
  }

  if (keep_geometry) {
    area_xwalk <- dplyr::left_join(area_xwalk, area_geometry)
    area_xwalk <- sf::st_as_sf(area_xwalk)
  }

  area_xwalk
}

#' @noRd
#' @importFrom sf st_make_valid st_difference
st_make_valid_coverage <- function(x, y, is_coverage = TRUE) {
  sf::st_make_valid(
    sf::st_difference(
      st_make_valid_union(x, is_coverage),
      st_make_valid_union(y, is_coverage)
    )
  )
}

#' @noRd
#' @importFrom sf st_make_valid st_union
st_make_valid_union <- function(x, is_coverage = TRUE) {
  sf::st_make_valid(sf::st_union(x, is_coverage = is_coverage))
}


#' @details Using an area crosswalk
#'
#' After creating an area crosswalk with [make_area_xwalk()], you can pass the
#' crosswalk to [use_area_xwalk()] along with a data frame from
#' [tidycensus::get_acs()] or [get_acs_tables()]. At a minimum, the data must
#' have a column with the same name as geoid_col along with columns named
#' "variable", "estimate", and "moe". Please note that this approach to
#' aggregation does *not* work well if your data contains "jam" values, e.g. the
#' substitution of 0 for "1939 or older" for the Median Year Built variable.
#' Ideally, the weight used for aggregation should be based on household counts
#' when aggregating a household-level variable and population counts when
#' aggregating a individual-level variable.
#'
#' @param area_xwalk A area crosswalk data frame created with
#'   [make_area_xwalk()]. Required for [use_area_xwalk()].
#' @param geography A character string used as general description for area
#'   geography type. Defaults to "area" but typical values could include
#'   "neighborhood", "planning district", or "service area".
#' @param value_col,moe_col Value and margin of error column names (defaults to
#'   "estimate" and "moe").
#' @param extensive If `TRUE` (default) calculate new estimate values as
#'   weighted sums and re-calculate margin of error with
#'   [tidycensus::moe_sum()]. If `FALSE`, calculate new estimate values as
#'   weighted means (appropriate for ACS median variables) and drop the margin
#'   of error. `perc` is also always set to `FALSE` if extensive is `FALSE`.
#' @rdname make_area_xwalk
#' @inheritParams label_acs_metadata
#' @export
#' @importFrom cli cli_progress_step
#' @importFrom dplyr select all_of left_join summarise
#' @importFrom tidycensus moe_sum
use_area_xwalk <- function(data,
                           area_xwalk,
                           geography = "area",
                           name_col = "NAME",
                           geoid_col = "GEOID",
                           suffix = c("_area", ""),
                           weight_col = "perc_HOUSING20",
                           variable_col = "variable",
                           value_col = "estimate",
                           moe_col = "moe",
                           digits = 0,
                           perc = TRUE,
                           extensive = TRUE) {
  check_data_frame(area_xwalk)
  check_data_frame(data)
  check_has_name(area_xwalk, c(geoid_col, weight_col))
  check_has_name(data, c(geoid_col, variable_col, value_col, moe_col))

  cli::cli_progress_step("Joining {.arg data} to {.arg area_xwalk}")

  data <- dplyr::select(
    data,
    dplyr::all_of(c(geoid_col, variable_col, value_col, moe_col))
  )

  area_data <- dplyr::left_join(
    area_xwalk,
    data,
    by = geoid_col,
    suffix = suffix,
    relationship = "many-to-many"
  )

  cli::cli_progress_step("Summarizing {.arg data} by {geography}")

  if (extensive) {
    area_data <- summarise_weighted_sum(
      area_data,
      weight_col = weight_col,
      variable_col = variable_col,
      value_col = value_col,
      moe_col = moe_col,
      name_col = name_col,
      digits = digits
    )
  } else {
    area_data <- summarise_weighted_mean(
      area_data,
      weight_col = weight_col,
      variable_col = variable_col,
      value_col = value_col,
      moe_col = moe_col,
      name_col = name_col,
      digits = digits
    )

    if (perc) {
      perc <- FALSE
      cli_alert_warning(
        "{.arg perc} must be {.code FALSE} if {.arg extensive} is {.code FALSE}"
      )
    }
  }

  area_data[["geography"]] <- geography

  suppressMessages(
    label_acs_metadata(area_data, perc = perc, geoid_col = name_col)
  )
}


#' @noRd
summarise_weighted_sum <- function(data,
                                   weight_col = "perc_HOUSING20",
                                   name_col = "NAME",
                                   variable_col = "variable",
                                   value_col = "estimate",
                                   moe_col = "moe",
                                   na.rm = TRUE,
                                   digits = 2) {
  dplyr::summarise(
    data,
    "{value_col}" := round(
      sum(.data[[value_col]] * .data[[weight_col]], na.rm = na.rm),
      digits = digits
    ),
    "{moe_col}" := round(
      tidycensus::moe_sum(.data[[moe_col]], .data[[value_col]] * .data[[weight_col]]),
      digits = digits
    ),
    .by = dplyr::all_of(c(name_col, variable_col))
  )
}

#' @noRd
summarise_weighted_mean <- function(data,
                                    weight_col = "perc_HOUSING20",
                                    name_col = "NAME",
                                    variable_col = "variable",
                                    value_col = "estimate",
                                    moe_col = "moe",
                                    na.rm = TRUE,
                                    digits = 2) {
  dplyr::summarise(
    data,
    "{value_col}" := round(
      weighted.mean(.data[[value_col]], w = .data[[weight_col]], na.rm = na.rm),
      digits = digits
    ),
    # FIXME: Explore options to calculate an interpolated MOE per this method:
    # https://github.com/datadesk/census-data-aggregator#approximating-medians
    # "{moe_col}" := round(
    #   weighted.mean(.data[[moe_col]], w = data[[paste0("perc_", weight_col)]], na.rm = na.rm),
    #   digits = digits
    # ),
    .by = dplyr::all_of(c(name_col, variable_col))
  )
}
