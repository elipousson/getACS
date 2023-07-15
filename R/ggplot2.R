#' Label a ggplot2 plot and add a caption based on an ACS survey year
#'
#' @inheritParams acs_survey_label
#' @keywords internal
labs_acs_survey <- function(...,
                            caption = NULL,
                            survey = "acs5",
                            year = 2021,
                            prefix = "Source: ",
                            tables = NULL) {
  check_installed("ggplot2")

  ggplot2::labs(
    ...,
    caption = c(
      caption,
      acs_survey_label_tables(
        survey = survey,
        year = year,
        prefix = prefix,
        tables = tables
      )
    )
  )
}
