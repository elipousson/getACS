#' @noRd
moe_zscore <- function(moe_level = 90) {
  zscore_i <- moe_level == c(90, 95, 99)

  stopifnot(
    any(zscore_i)
  )

  c(1.645, 1.96, 2.575)[zscore_i]
}

#' @noRd
moe_se <- function(moe, moe_level = 90) {
  moe / moe_zscore(moe_level)
}

#' @noRd
acs_cv <- function(estimate, moe, moe_level = 90, digits = 2) {
  round(moe_se(moe, moe_level) / estimate, digits = digits)
}

#' @noRd
reliability_when <- function(cv, type = c("census", "esri")) {
  type <- arg_match(type)

  if (type == "esri") {
    # https://www.esri.com/about/newsroom/arcuser/aggregation-improves-reliability-concerns-for-american-community-survey-data/
    reliability <- dplyr::case_when(
      cv < 0 ~ NA_character_,
      cv <= 0.12 ~ "high",
      cv <= 0.40 ~ "medium",
      cv > 0.40 ~ "low"
    )

    return(reliability)
  }

  dplyr::case_when(
    cv < 0 ~ NA_character_,
    cv < 0.15 ~ "high",
    cv <= 0.30 ~ "medium",
    cv > 0.30 ~ "low"
  )
}

#' Add columns for the coefficient of variation and reliability category
#'
#'
#' @param value_col,moe_col Value and margin of error column names (default to
#'   "estimate" and "moe").
#' @param moe_level The confidence level of the margin of error. Defaults to 90
#'   (which is the same default as [tidycensus::get_acs()]).
#' @param cv_col Coefficient of variation column name. Defaults to "cv".
#' @param reliability_col Reliability category column name. Defaults to
#'   "reliability".
#' @export
assign_acs_reliability <- function(data,
                            value_col = "estimate",
                            moe_col = "moe",
                            moe_level = 90,
                            cv_col = "cv",
                            reliability_col = "reliability") {
  data |>
    dplyr::mutate(
      "{cv_col}" := acs_cv(.data[[value_col]], .data[[moe_col]], moe_level),
      "{reliability_col}" := reliability_when(.data[[cv_col]]),
      .after = all_of(moe_col)
    )
}
