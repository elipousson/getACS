.onLoad <- function(lib, pkg) {
  utils::data(
    list = c("race_iteration"),
    package = pkg,
    envir = parent.env(environment())
  )
}

utils::globalVariables(
  c(
    "cols", "column_id", "column_title", "denominator_estimate",
    "denominator_moe", "estimate", "moe", "table_id", "table_title", "variable"
  )
)

#' From `cliExtras::cli_quiet`
#'
#' @param quiet If `FALSE` (default), leave `cli.default_handler` option
#'   unchanged. If `TRUE`, set `cli.default_handler` to [suppressMessages]
#'   temporarily with [rlang::local_options()]
#' @keywords internal
cli_quiet <- function(quiet = FALSE,
                      push = FALSE,
                      .frame = rlang::caller_env()) {
  if (rlang::is_false(quiet)) {
    return(invisible(NULL))
  }

  if (rlang::is_true(push)) {
    return(rlang::push_options("cli.default_handler" = suppressMessages))
  }

  rlang::local_options("cli.default_handler" = suppressMessages, .frame = .frame)
}


#' @noRd
check_sf <- function(x, allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (inherits(x, "sf")) {
    return(invisible(NULL))
  }

  stop_input_type(
    x,
    what = "sf",
    allow_null = allow_null,
    call = call
  )
}

#' @noRd
check_has_name <- function(x,
                           nm,
                           allow_null = FALSE,
                           allow_any = FALSE,
                           arg = caller_arg(x),
                           call = caller_env()) {
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  nm_check <- has_name(x, nm)
  has_nm <- all(nm_check)

  msg <- c("{.arg {arg}} must have names {.val {nm}}",
    "i" = "{.arg {arg}} is missing {length(nm[!nm_check])} name{?s}: {.val {nm[!nm_check]}}"
  )

  if (allow_any) {
    has_nm <- any(nm_check)
    msg <- "{.arg {arg}} must have any of the names {.val {nm}}"
  }

  if (has_nm) {
    return(invisible(NULL))
  }

  cli_abort(msg, call = call)
}

#' Helper for recoding based on a named list
#'
#' @keywords internal
#' @export
fct_recode_with_list <- function(x, list = NULL, in_order = TRUE, ordered = NA) {
  check_installed("forcats")

  x <- forcats::fct_recode(x, !!!list)

  if (!in_order) {
    return(x)
  }

  forcats::fct_inorder(x, ordered = ordered)
}

#' @noRd
st_is_predicate <- function(x,
                            y,
                            .f_x = sf::st_point_on_surface,
                            .f_y = sf::st_union,
                            .predicate = sf::st_intersects) {
  if (is_function(.f_x)) {
    x <- suppressWarnings(.f_x(x))
  }

  if (is_function(.f_y)) {
    y <- suppressWarnings(.f_y(y))
  }

  as.logical(.predicate(x, y, sparse = FALSE))
}

#' @noRd
st_is_all_predicate <- function(x,
                                y,
                                .f_x = sf::st_point_on_surface,
                                .f_y = sf::st_union,
                                .predicate = sf::st_intersects) {
  all(st_is_predicate(x, y, .f_x, .f_y, .predicate = .predicate))
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
