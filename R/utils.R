utils::globalVariables(
  c(
    "cols",
    "column_id",
    "column_title",
    "estimate",
    "moe",
    "table_id",
    "variable",
    # Added 2023-11-04
    "name",
    "label",
    "indent",
    "GEOID",
    "what",
    "perc_cols",
    "acs_label"
  )
)

#' From `cliExtras::cli_quiet`
#'
#' @param quiet If `FALSE` (default), leave `cli.default_handler` option
#'   unchanged. If `TRUE`, set `cli.default_handler` to [suppressMessages]
#'   temporarily with [rlang::local_options()]
#' @keywords internal
cli_quiet <- function(
  quiet = FALSE,
  push = FALSE,
  .frame = rlang::caller_env()
) {
  if (rlang::is_false(quiet)) {
    return(invisible(NULL))
  }

  if (rlang::is_true(push)) {
    return(rlang::push_options("cli.default_handler" = suppressMessages))
  }

  rlang::local_options(
    "cli.default_handler" = suppressMessages,
    .frame = .frame
  )
}

#' Check if object has specified names
#' @noRd
check_has_name <- function(
  x,
  nm,
  allow_null = FALSE,
  allow_any = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (allow_null && is_null(x)) {
    return(invisible(NULL))
  }

  nm_check <- has_name(x, nm)
  has_nm <- all(nm_check)

  msg <- c(
    "{.arg {arg}} must have names {.val {nm}}",
    "i" = "{.arg {arg}} is missing {length(nm[!nm_check])} name{?s}:
    {.val {nm[!nm_check]}}"
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

#' Check if object has a specified geometry type
#' @noRd
check_geometry_is_type <- function(
  x,
  type,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!inherits_any(x, c("sfg", "sfc", "sf"))) {
    stop_input_type(
      x,
      what = c("sfg", "sfc", "sf"),
      allow_null = allow_null,
      call = call
    )
  }

  if (sf::st_is(x, type)) {
    return(invisible(NULL))
  }

  cli::cli_abort(
    "{.arg {arg}} must have {type} geometry.",
    call = call
  )
}

#' Helper for recoding based on a named list
#'
#' @keywords internal
#' @export
fct_recode_with_list <- function(
  x,
  list = NULL,
  in_order = TRUE,
  ordered = NA
) {
  check_installed("forcats")

  x <- forcats::fct_recode(x, !!!list)

  if (!in_order) {
    return(x)
  }

  forcats::fct_inorder(x, ordered = ordered)
}
