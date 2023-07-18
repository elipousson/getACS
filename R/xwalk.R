#' Make a block-tract xwalk
#'
#' @inheritParams tigris::blocks
#' @inheritParams dplyr::left_join
#' @inheritDotParams tigris::blocks
#' @keywords internal
#' @export
make_block_xwalk <- function(state,
                             county = NULL,
                             year = 2020,
                             by = c("TRACTCE20" = "TRACTCE"),
                             suffix = c("_block", "_tract"),
                             keep_zipped_shapefile = TRUE,
                             ...) {
  cli::cli_progress_message(
    "Downloading blocks"
  )

  block_sf <- tigris::blocks(
    state = state,
    county = county,
    year = year,
    keep_zipped_shapefile = keep_zipped_shapefile,
    ...
  )

  cli::cli_progress_message(
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
    block_sf,
    sf::st_drop_geometry(tract_sf),
    suffix = suffix,
    by = by
  )
}

#' Make a crosswalk for an arbitrary geography
#'
#' This function is not yet working.
#'
#' @inheritParams make_block_xwalk
#' @param weight Column name to use for weighting
#' @keywords internal
#' @export
make_area_xwalk <- function(area,
                            block_xwalk = NULL,
                            state = NULL,
                            county = NULL,
                            year = 2021,
                            by = c("TRACTCE20" = "TRACTCE"),
                            suffix = c("_block", "_tract"),
                            weight = "HOUSING20",
                            .group_by = c("name", "TRACTCE20"),
                            digits = 2,
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

  stopifnot(
    inherits(area, "sf"),
    inherits(block_xwalk, "sf"),
    has_name(block_xwalk, weight)
  )

  area_xwalk <- sf::st_join(
    sf::st_transform(block_xwalk, sf::st_crs(area)),
    area,
    left = FALSE,
    largest = TRUE
  )

  area_xwalk <- sf::st_drop_geometry(area_xwalk)

  print(names(area_xwalk))

  area_xwalk <- dplyr::filter(
    area_xwalk,
    {{ weight }} > 0
  )

  area_xwalk <- dplyr::summarise(
    area_xwalk,
    "{{ weight }}" := sum({{ weight }}, na.rm = TRUE),
    .by = .group_by
  )

  dplyr::mutate(
    area_xwalk,
    "weight" = round({{ weight }} / sum({{ weight }}, na.rm = TRUE), digits = digits)
  )
}
