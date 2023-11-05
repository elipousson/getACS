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

#' `geom_` functions for plotting ACS data with ggplot2
#'
#' `r lifecycle::badge("experimental")`
#'
#' These functions wrap existing `{ggplot2}` functions and provide default
#' aesthetic mappings that work with the structure of input ACS data to support
#' shorter, less repetitive plotting code.
#'
#' @name geom_acs
#' @keywords ggplot2 internal
NULL


#' @rdname geom_acs
#' @name aes_errorbarh
#' @inheritParams ggplot2::aes
#' @export
#' @importFrom ggplot2 aes
aes_errorbarh <- function(xmin = NULL,
                          xmax = NULL,
                          ...,
                          value_col = "estimate",
                          moe_col = "moe",
                          perc_prefix = "perc",
                          perc_sep = "_",
                          perc = FALSE) {
  if (perc) {
    perc_cols <- acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep)
    value_col <- perc_cols[[1]]
    moe_col <- perc_cols[[2]]
  }

  ggplot2::aes(
    xmin = .data[[value_col]] - .data[[moe_col]],
    xmax = .data[[value_col]] + .data[[moe_col]],
    ...
  )
}

#' @rdname geom_acs
#' @name aes_errorbarv
#' @inheritParams ggplot2::aes
#' @export
#' @importFrom ggplot2 aes
aes_errorbarv <- function(ymin = NULL,
                          ymax = NULL,
                          ...,
                          value_col = "estimate",
                          moe_col = "moe",
                          perc_prefix = "perc",
                          perc_sep = "_",
                          perc = FALSE) {
  if (perc) {
    perc_cols <- acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep)
    value_col <- perc_cols[[1]]
    moe_col <- perc_cols[[2]]
  }

  ggplot2::aes(
    ymin = ymin %||% .data[[value_col]] - .data[[moe_col]],
    ymax = ymax %||% .data[[value_col]] + .data[[moe_col]],
    ...
  )
}

#' @rdname geom_acs
#' @name geom_acs_errorbarh
#' @inheritParams ggplot2::geom_errorbarh
#' @export
#' @importFrom ggplot2 geom_errorbarh aes
geom_acs_errorbarh <- function(mapping = NULL,
                               data = NULL,
                               ...,
                               value_col = "estimate",
                               moe_col = "moe",
                               perc_prefix = "perc",
                               perc_sep = "_",
                               perc = FALSE) {

  ggplot2::geom_errorbarh(
    mapping = utils::modifyList(
      mapping %||% aes(),
      aes_errorbarh(
        value_col = value_col,
        moe_col = moe_col,
        perc_prefix = perc_prefix,
        perc_sep = perc_sep,
        perc = perc
      )
    ),
    data = data,
    ...
  )
}

#' @rdname geom_acs
#' @name geom_acs_errorbarv
#' @inheritParams ggplot2::geom_errorbarh
#' @export
#' @importFrom ggplot2 geom_errorbarh aes
geom_acs_errorbarv <- function(mapping = NULL,
                               data = NULL,
                               ...,
                               value_col = "estimate",
                               moe_col = "moe",
                               perc_prefix = "perc",
                               perc_sep = "_",
                               perc = FALSE) {

  ggplot2::geom_errorbar(
    mapping = utils::modifyList(
      mapping %||% aes(),
      aes_errorbarv(
       value_col = value_col,
       moe_col = moe_col,
       perc_prefix = perc_prefix,
       perc_sep = perc_sep,
       perc = perc
     )
    ),
    data = data,
    ...
  )
}

