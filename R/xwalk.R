#' Make a crosswalk data frame for U.S. Census blocks and tracts
#'
#' `make_block_xwalk()` joined U.S. Census blocks data from [tigris::blocks()]
#' to a data frame from [tigris::tracts()] to provide a crosswalk between both
#' geographies. If `year = 2020`, the suffix parameter is not used. If year is
#' any other year than 2020, the by parameter must be changed from the default
#' value of `c("TRACTCE20" = "TRACTCE")`. 2020 is also the only year where
#' [tigris::blocks()] includes the population and household count data required
#' to use this crosswalk data frame with [make_area_xwalk()].
#'
#' @inheritParams tigris::blocks
#' @inheritParams dplyr::left_join
#' @inheritDotParams tigris::blocks
#' @param keep_zipped_shapefile Passed to [tigris::blocks()] and
#'   [tigris::tracts()] to keep and re-use the zipped shapefile.
#' @export
#' @importFrom cli cli_progress_message
#' @importFrom tigris blocks tracts
#' @importFrom dplyr left_join
#' @importFrom sf st_drop_geometry
make_block_xwalk <- function(state,
                             county = NULL,
                             year = 2020,
                             by = c("TRACTCE20" = "TRACTCE"),
                             keep_zipped_shapefile = TRUE,
                             suffix = c("_block", "_tract"),
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

  dplyr::left_join(
    x = block_sf,
    y = sf::st_drop_geometry(tract_sf),
    by = by,
    suffix = suffix
  )
}

#' Make a crosswalk data frame for U.S. Census tracts and arbitrary areas based
#' on block-level attributes
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
#' @param weight_col Column name to use for weighting
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
#' @export
#' @importFrom cli cli_progress_step
#' @importFrom vctrs vec_rbind
#' @importFrom sf st_join
#' @importFrom dplyr filter group_by summarise across all_of mutate
make_area_xwalk <- function(area,
                            block_xwalk = NULL,
                            state = NULL,
                            county = NULL,
                            year = 2020,
                            name_col = "name",
                            weight_col = "HOUSING20",
                            geoid_col = "GEOID",
                            tract_col = "TRACTCE20",
                            by = c("TRACTCE20" = "TRACTCE"),
                            suffix = c("_block", "_tract"),
                            digits = 2,
                            add_coverage = TRUE,
                            ...) {
  block_xwalk <- block_xwalk %||%
    make_block_xwalk(
      state = state,
      county = county,
      year = year,
      by = by,
      suffix = suffix,
      ...
    )

  check_name(name_col)
  check_name(tract_col)
  check_name(weight_col)
  check_character(by)
  check_sf(area)
  check_sf(block_xwalk)

  block_xwalk <- sf::st_transform(block_xwalk, sf::st_crs(area))

  if (add_coverage) {
    cli::cli_progress_step("Adding coverage for block_xwalk")

    coverage_name <- tempfile(tmpdir = "")

    area_coverage <- data.frame(
      coverage_name,
      st_make_valid_coverage(block_xwalk, area)
    )

    area <- vctrs::vec_rbind(
      set_names(area_coverage, c(name_col, "geometry")),
      area
    )

    area <- sf::st_as_sf(area)
  }

  cli::cli_progress_step("Joining block_xwalk to area")

  area_xwalk <- suppressWarnings(sf::st_join(
    block_xwalk,
    area,
    left = FALSE,
    largest = TRUE
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
#' @rdname make_area_xwalk
#' @inheritParams label_acs_metadata
#' @export
use_area_xwalk <- function(data,
                           area_xwalk,
                           name_col = "NAME",
                           geoid_col = "GEOID",
                           suffix = c("_area", ""),
                           weight_col = "HOUSING20",
                           geography = "area",
                           digits = 0,
                           perc = TRUE,
                           ...) {
  stopifnot(
    is.data.frame(area_xwalk),
    is.data.frame(data),
    all(has_name(area_xwalk, c(geoid_col, paste0("perc_", weight_col)))),
    all(has_name(data, c(geoid_col, "variable", "estimate", "moe")))
  )

  cli::cli_progress_step("Joining {.arg data} to {.arg area_xwalk}")

  data <- dplyr::select(data, dplyr::all_of(c(geoid_col, "variable", "estimate", "moe")))

  area_data <- dplyr::left_join(
    area_xwalk,
    data,
    by = geoid_col,
    suffix = suffix,
    relationship = "many-to-many"
  )

  cli::cli_progress_step("Summarizing {.arg data} by {geography}")

  area_data <- dplyr::summarise(
    area_data,
    estimate = round(
      sum(estimate * .data[[paste0("perc_", weight_col)]], na.rm = TRUE),
      digits = digits
    ),
    moe = round(
      tidycensus::moe_sum(moe, estimate * .data[[paste0("perc_", weight_col)]]),
      digits = digits
    ),
    .by = dplyr::all_of(c(name_col, "variable"))
  )

  area_data[["geography"]] <- geography

  suppressMessages(
    label_acs_metadata(area_data, perc = perc, geoid_col = name_col)
  )
}
