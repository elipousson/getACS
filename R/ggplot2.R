#' Label a ggplot2 plot and add a caption based on an ACS survey year
#'
#' `labs_acs_survey()` uses `acs_survey_label_table()` to create a label for a
#' ggplot2 plot passed to the caption parameter of [ggplot2::labs()].
#'
#' @inheritParams acs_survey_label_table
#' @inheritParams ggplot2::labs
#' @inheritDotParams ggplot2::labs
#' @keywords ggplot2
#' @export
labs_acs_survey <- function(...,
                            caption = NULL,
                            survey = "acs5",
                            year = 2021,
                            prefix = "Source: ",
                            table = NULL) {
  ggplot2::labs(
    ...,
    caption = c(
      caption,
      acs_survey_label_table(
        survey = survey,
        year = year,
        prefix = prefix,
        table = table
      )
    )
  )
}


#' Scales for plotting ACS data with ggplot2
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @name scale_acs
#' @keywords ggplot2
NULL

#' @rdname scale_acs
#' @name scale_x_acs_estimate
#' @export
scale_x_acs_estimate <- function(name = "Estimate",
                                 ...,
                                 labels = scales::label_comma()) {
  ggplot2::scale_x_continuous(
    name = name,
    labels = labels,
    ...
  )
}

#' @rdname scale_acs
#' @name scale_y_acs_percent
#' @export
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales label_percent
scale_y_acs_percent <- function(name = "Est. % of total",
                                ...,
                                labels = scales::label_percent()) {
  ggplot2::scale_y_continuous(
    name = name,
    labels = labels,
    ...
  )
}

#' @rdname scale_acs
#' @name scale_x_acs_percent
#' @export
#' @importFrom ggplot2 scale_x_continuous
scale_x_acs_percent <- function(name = "Est. % of total",
                                ...,
                                labels = scales::label_percent()) {
  ggplot2::scale_x_continuous(
    name = name,
    labels = labels,
    ...
  )
}

#' @rdname scale_acs
#' @name scale_y_acs_estimate
#' @export
#' @importFrom scales label_comma
scale_y_acs_estimate <- function(name = "Estimate",
                                 ...,
                                 labels = scales::label_comma()) {
  ggplot2::scale_y_continuous(
    name = name,
    labels = labels,
    ...
  )
}


#' @rdname scale_acs
#' @name scale_x_acs_ts
#' @inheritParams acs_survey_ts
#' @export
scale_x_acs_ts <- function(name = "Year",
                           ...,
                           breaks = NULL,
                           survey = "acs5",
                           year = 2021) {
  ggplot2::scale_x_continuous(
    name = name,
    breaks = breaks %||% suppressMessages(acs_survey_ts(survey, year)),
    ...
  )
}

#' @rdname scale_acs
#' @name scale_y_acs_ts
#' @export
scale_y_acs_ts <- function(name = "Year",
                           ...,
                           breaks = NULL,
                           survey = "acs5",
                           year = 2021) {
  ggplot2::scale_y_continuous(
    name = name,
    breaks = breaks %||% suppressMessages(acs_survey_ts(survey, year)),
    ...
  )
}
