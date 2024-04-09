#' Label a ggplot2 plot and add a caption based on an ACS survey year
#'
#' `labs_acs_survey()` uses `acs_survey_label_table()` to create a label for a
#' ggplot2 plot passed to the caption parameter of [ggplot2::labs()].
#'
#' @inheritParams acs_survey_label_table
#' @param .data Optional data frame with "table_id" column used in place of
#'   `table` if table is `NULL`. Ignored if `table` is supplied.
#' @inheritParams ggplot2::labs
#' @inheritDotParams ggplot2::labs
#' @keywords ggplot2
#' @export
labs_acs_survey <- function(...,
                            caption = NULL,
                            survey = "acs5",
                            year = 2022,
                            prefix = "Source: ",
                            table = NULL,
                            .data = NULL) {
  if (!is.null(.data)) {
    table <- table %||% .data[["table_id"]]
  }

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
#' @param perc If `TRUE`, use the `scale_x_acs_percent` or
#'   `scale_y_acs_percent`. Defaults to `FALSE`.
#' @keywords ggplot2
NULL

#' @rdname scale_acs
#' @name scale_x_acs
#' @export
scale_x_acs <- function(
    ...,
    perc = FALSE) {
  if (perc) {
    return(scale_x_acs_percent(...))
  }

  scale_x_acs_estimate(...)
}

#' @rdname scale_acs
#' @name scale_y_acs
#' @export
scale_y_acs <- function(
    ...,
    perc = FALSE) {
  if (perc) {
    return(scale_y_acs_percent(...))
  }

  scale_y_acs_estimate(...)
}

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
                           year = 2022) {
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
                           year = 2022) {
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
    perc_cols <- acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep, perc)
    value_col <- perc_cols[["value_col"]]
    moe_col <- perc_cols[["moe_col"]]
  }

  ggplot2::aes(
    xmin = xmin %||% .data[[value_col]] - .data[[moe_col]],
    xmax = xmax %||% .data[[value_col]] + .data[[moe_col]],
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
    perc_cols <- acs_perc_cols(value_col, moe_col, perc_prefix, perc_sep, perc)
    value_col <- perc_cols[["value_col"]]
    moe_col <- perc_cols[["moe_col"]]
  }

  ggplot2::aes(
    ymin = ymin %||% .data[[value_col]] - .data[[moe_col]],
    ymax = ymax %||% .data[[value_col]] + .data[[moe_col]],
    ...
  )
}

#' @rdname geom_acs
#' @export
geom_acs_errorbar <- function(mapping = NULL,
                              data = NULL,
                              ...,
                              na.rm = TRUE,
                              orientation = NA,
                              value_col = "estimate",
                              moe_col = "moe",
                              perc_prefix = "perc",
                              perc_sep = "_",
                              perc = FALSE) {
  if (identical(orientation, "y")) {
    geom_acs_errorbarv(
      mapping = mapping,
      data = data,
      ...,
      na.rm = na.rm,
      value_col = value_col,
      moe_col = moe_col,
      perc_prefix = perc_prefix,
      perc_sep = perc_sep,
      perc = perc
    )
  } else {
    geom_acs_errorbarh(
      mapping = mapping,
      data = data,
      ...,
      na.rm = na.rm,
      value_col = value_col,
      moe_col = moe_col,
      perc_prefix = perc_prefix,
      perc_sep = perc_sep,
      perc = perc
    )
  }
}

#' @rdname geom_acs
#' @name geom_acs_errorbarh
#' @inheritParams ggplot2::geom_errorbarh
#' @export
#' @importFrom ggplot2 geom_errorbarh aes
geom_acs_errorbarh <- function(mapping = NULL,
                               data = NULL,
                               ...,
                               na.rm = TRUE,
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
    na.rm = na.rm,
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
                               na.rm = TRUE,
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
    na.rm = na.rm,
    ...
  )
}

#' Mapping ACS column names
#'
#' @export
#' @keywords internal
aes_acs_col <- function(
    x = "estimate",
    y = "column_title",
    fill = y) {
  mapping <- aes()

  if (is_string(x)) {
    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(x = .data[[x]])
    )
  } else if (!is.null(x)) {
    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(x = x)
    )
  }

  if (is_string(y)) {
    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(y = .data[[y]])
    )
  } else if (!is.null(y)) {
    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(y = y)
    )
  }

  if (is_string(fill)) {
    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(fill = .data[[fill]])
    )
  } else if (!is.null(fill)) {
    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(fill = fill)
    )
  }

  mapping
}

#' Creating a ggplot2 geom with mapping, scale, and labels
#'
#' @export
#' @keywords internal
geom_acs_col <- function(
    mapping = NULL,
    data = NULL,
    position = "stack",
    ...,
    x = "estimate",
    y = "column_title",
    fill = y,
    value_col = "estimate",
    moe_col = "moe",
    perc_prefix = "perc",
    perc_sep = "_",
    perc = TRUE,
    orientation = NA,
    errorbar_value = TRUE,
    errorbar_params = list(linewidth = 0.5, height = 0.35),
    scale_value = TRUE,
    scale_params = list()) {
  y_orientation <- identical(orientation, "y")

  if (is_true(errorbar_value)) {
    errorbar_value <- rlang::exec(
      .fn = geom_acs_errorbar,
      perc = perc,
      value_col = value_col,
      moe_col = moe_col,
      perc_prefix = perc_prefix,
      perc_sep = perc_sep,
      orientation = orientation,
      !!!errorbar_params
    )
  }

  if (is_true(scale_value)) {
    scale_fn <- scale_x_acs
    if (y_orientation) {
      scale_fn <- scale_y_acs
    }

    scale_value <- rlang::exec(
      .fn = scale_fn,
      !!!scale_params,
      perc = perc
    )
  }

  cols <- acs_perc_cols(
    value_col = value_col,
    moe_col = moe_col,
    perc_prefix = perc_prefix,
    perc_sep = perc_sep,
    perc = perc
  )

  if (perc && is_string(x) && x != cols[["value_col"]]) {
    x <- cols[["value_col"]]
  }

  if (y_orientation) {
    if (identical(fill, x)) {
      fill <- y
    }

    acs_mapping <- aes_acs_col(x = y, y = x, fill = fill)
  } else {
    acs_mapping <- aes_acs_col(x = x, y = y, fill = fill)
  }

  acs_mapping <- mapping %||%
    acs_mapping

  acs_geom <- ggplot2::geom_col(
    data = data,
    position = position,
    ...
  )

  list(
    acs_mapping,
    acs_geom,
    errorbar_value,
    scale_value
  )
}
