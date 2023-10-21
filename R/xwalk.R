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
#' @param suffix Suffixes added to the output to disambiguate column names from
#'   the block and tract data. Unused for 2020 data.
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
#'   coverage should be set to `TRUE` (default).
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
#' @param coverage If `TRUE` (default), it is assumed that area does not cover
#'   the full extent of the block_xwalk and an additional feature is added with
#'   the difference between the unioned area geometry and unioned block_xwalk
#'   geometry. This additional coverage ensures that blocks are accurately
#'   assigned to this alternate geography but it is excluded from the returned
#'   data frame. If `coverage` is `TRUE` and all features in area overlap with
#'   block_xwalk, the function issues a warning and then resets coverage to
#'   `FALSE`. The reverse option is applied if any features from area do not
#'   overlap
#' @param erase If `TRUE`, apply [tigris::erase_water()] to input area and
#'   block_xwalk before joining. Defaults to `FALSE`. If `erase` is a sf object,
#'   the geometry of the input sf is erased from area and block_xwalk. This
#'   option is intended to support erasing open space or other non-developed
#'   land as well as water areas.
#' @param placement String with option for joining `area` and `block_xwalk`:
#'   "largest", "surface", or "centroid". "largest" joins the two using
#'   [sf::st_join()] with largest set to `TRUE`. "surface" first transforms
#'   block_xwalk using [sf::st_point_on_surface()] and "centroid" uses
#'   [sf::st_centroid()].
#' @inheritParams tigris::erase_water
#' @param keep_geometry If `TRUE`, area_xwalk is a sf object with the same
#'   geometry as the input area. Defaults to `FALSE`.
#' @param crs Coordinate reference system to use for input data. Recommended to
#'   set to a projected CRS if input area data is in a geographic CRS.
#' @param ... Passed to [make_block_xwalk()].
#' @returns A tibble or a sf object.
#' @seealso [tidycensus::interpolate_pw()], [areal::aw_interpolate()]
#' @export
#' @importFrom dplyr select filter group_by summarise across mutate ungroup
#'   left_join
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
                            placement = c("largest", "surface", "centroid"),
                            digits = 2,
                            extensive = TRUE,
                            coverage = TRUE,
                            erase = FALSE,
                            area_threshold = 0.75,
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

  if (!is.null(crs)) {
    area <- sf::st_transform(area, crs = crs)
  }

  block_xwalk <- sf::st_transform(block_xwalk, crs = sf::st_crs(area))

  if (keep_geometry) {
    area_geometry <- dplyr::select(area, all_of(name_col))
  }

  cli::cli_progress_step("Checking {.arg block_xwalk} and {.arg area} geometry")

  if (coverage && st_is_all_predicate(block_xwalk, area)) {
    cli::cli_bullets(
      c(
        "!" = "All features in {.arg block_xwalk} already intersect with {.arg area}",
        "*" = "Setting {.arg coverage} to {.code FALSE} to avoid inaccurate results."
      )
    )

    coverage <- FALSE
  }

  if (is_true(erase) || inherits_any(erase, c("sfc", "sf"))) {
    what <- "geometry"
    if (is_true(erase)) {
      what <- "water areas"
    }

    cli::cli_progress_step("Erasing {what} from {.arg area}")

    area <- erase_input_sf(
      area,
      erase = erase,
      area_threshold = area_threshold,
      year = year
    )

    cli::cli_progress_step("Erasing {what} from {.arg block_xwalk}")

    block_xwalk <- erase_input_sf(
      block_xwalk,
      erase = erase,
      area_threshold = area_threshold,
      year = year
    )
  }

  if (coverage) {
    cli::cli_progress_step("Adding coverage for {.arg block_xwalk}")

    coverage_name <- tempfile(tmpdir = "")

    area <- rbind_area_coverage(
      area = area,
      coverage_name = coverage_name,
      name_col = name_col,
      block_xwalk = block_xwalk
    )
  }

  cli::cli_progress_step("Joining {.arg block_xwalk} to {.arg area}")

  area_xwalk <- use_block_xwalk(block_xwalk, area, placement)

  area_xwalk <- sf::st_drop_geometry(area_xwalk)

  cli::cli_progress_step(
    "Summarizing {weight_col} by {tract_col} and {name_col}"
  )

  area_xwalk <- summarise_area_weight(
    area_xwalk = area_xwalk,
    name_col = name_col,
    weight_col = weight_col,
    geoid_col = geoid_col,
    tract_col = tract_col,
    digits = digits
  )

  if (coverage) {
    area_xwalk <- dplyr::filter(area_xwalk, .data[[name_col]] != coverage_name)
  }

  if (keep_geometry) {
    area_xwalk <- dplyr::left_join(area_xwalk, area_geometry, by = name_col)
    area_xwalk <- sf::st_as_sf(area_xwalk)
  }

  area_xwalk
}

#' @noRd
erase_input_sf <- function(input_sf,
                           area_threshold = 0.75,
                           year = NULL,
                           erase = FALSE,
                           call = caller_env()) {
  if (inherits(erase, "sf")) {
    input_sf <- sf::st_difference(
      input_sf,
      sf::st_union(sf::st_transform(erase, crs = sf::st_crs(input_sf)))
    )

    return(input_sf)
  }

  check_logical(erase, call = call)

  if (!erase) {
    return(input_sf)
  }

  suppressMessages(
    tigris::erase_water(
      input_sf,
      area_threshold = area_threshold,
      year = year
    )
  )
}

#' @noRd
use_block_xwalk <- function(block_xwalk,
                            area,
                            placement = c("largest", "surface", "centroid"),
                            error_call = caller_env()) {
  placement <- arg_match(placement, error_call = error_call)

  if (placement == "surface") {
    block_xwalk <- suppressWarnings(sf::st_point_on_surface(block_xwalk))
  } else if (placement == "centroid") {
    block_xwalk <- suppressWarnings(sf::st_centroid(block_xwalk))
  }

  suppressWarnings(sf::st_join(
    block_xwalk,
    area,
    left = FALSE,
    largest = TRUE,
    suffix = c("_block", "")
  ))
}

#' @noRd
summarise_area_weight <- function(area_xwalk,
                                  name_col = "NAME",
                                  weight_col = "HOUSING20",
                                  geoid_col = "GEOID",
                                  tract_col = "TRACTCE20",
                                  digits = 2) {
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
      all_of(weight_col),
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

  dplyr::ungroup(area_xwalk)
}

#' @noRd
rbind_area_coverage <- function(area,
                                coverage_name,
                                name_col = "NAME",
                                block_xwalk,
                                error_call = caller_env()) {
  area_coverage <- try_fetch(
    st_make_valid_coverage(block_xwalk, area),
    error = function(cnd) {
      cli_abort(
        c("Valid spatial coverage for the area of {.arg block_xwalk} outside the {.arg area_xwalk} can't be created.",
          "*" = "Set {.code coverage = FALSE} and try again."
        ),
        parent = cnd,
        call = error_call
      )
    }
  )

  if (is_empty(area_coverage)) {
    cli_abort(
      c("{.arg area} is not covered by {.arg block_xwalk}",
        "i" = "Supply a {.arg block_xwalk} covering the full geometry of
          {.arg area} or set {.arg coverage} to {.code FALSE}"
      ),
      call = error_call
    )
  }

  area_coverage <- set_names(
    data.frame(
      coverage_name,
      area_coverage
    ),
    c(name_col, attr(area, "sf_column"))
  )

  area <- vctrs::vec_rbind(area, area_coverage, .error_call = error_call)

  sf::st_make_valid(sf::st_as_sf(area))
}

#' @details Using an area crosswalk
#'
#' After creating an area crosswalk with [make_area_xwalk()], you can pass the
#' crosswalk to [use_area_xwalk()] along with a data frame from
#' [tidycensus::get_acs()] or [get_acs_tables()]. At a minimum, the data must
#' have a column with the same name as geoid_col along with columns named
#' "variable", "estimate", and "moe".
#'
#' Please note that this approach to aggregation does *not* work well if your
#' data contains "jam" values, e.g. the substitution of 0 for "1939 or older"
#' for the Median Year Built variable. Ideally, the weight used for aggregation
#' should be based on household counts when aggregating a household-level
#' variable and population counts when aggregating a individual-level variable.
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
    all_of(c(geoid_col, variable_col, value_col, moe_col))
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
    .by = all_of(c(name_col, variable_col))
  )
}

#' @noRd
#' @importFrom stats weighted.mean
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
      stats::weighted.mean(.data[[value_col]], w = .data[[weight_col]], na.rm = na.rm),
      digits = digits
    ),
    # FIXME: Explore options to calculate an interpolated MOE per this method:
    # https://github.com/datadesk/census-data-aggregator#approximating-medians
    # "{moe_col}" := round(
    #   weighted.mean(.data[[moe_col]], w = data[[paste0("perc_", weight_col)]], na.rm = na.rm),
    #   digits = digits
    # ),
    .by = all_of(c(name_col, variable_col))
  )
}
