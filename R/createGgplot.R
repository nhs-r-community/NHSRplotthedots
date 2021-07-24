#' Create ggplot2
#'
#' Creates a ggplot2 object using the parameters passed in.
#'
#' @param x an object created by [spc()]
#' @param pointSize Specify the plotting point size for the ggplot output. Default is 2.5.
#' @param percentageYAxis Specify whether the y axis values are percentages. Accepted values are TRUE for percentage y
#'     axis, FALSE for integer y axis. Defaults to FALSE.
#' @param mainTitle Specify a character string value for the ggplot title.
#' @param xAxisLabel Specify a character string value for the x axis title.
#' @param yAxisLabel Specify a character string value for the y axis title.
#' @param fixedXAxisMultiple Specify whether, if producing a faceted spc, x axis should be fixed for all facet plots.
#'     Accepted values are TRUE for fixed x axes or FALSE for individual x axes.
#' @param fixedYAxisMultiple Specify whether, if producing a faceted spc, y axis should be fixed for all facet plots.
#'     Accepted values are TRUE for fixed y axes or FALSE for individual y axes.
#' @param xAxisDateFormat Specify how dates on the x axis should be displayed. Format should be provided
#'     as a character string using 'd m Y' etc syntax.
#' @param xAxisBreaks Specify an interval value for breaks on the x axis. Value should be a character string expressing
#'     interval length and type, e.g. "3 months", "7 days".
#' @param yAxisBreaks Specify an interval value for breaks on the y axis. Value should be a numeric vector of length 1,
#'     either an integer for integer scales or a decimal value for percentage scales. This option is ignored if faceting
#'     is in use.
#' @param ... currently ignored
#' @return The ggplot2 object
#' @export
createGgplot <- function(x,
                         pointSize = 2.5,
                         percentageYAxis = FALSE,
                         mainTitle = "SPC Chart",
                         xAxisLabel = NULL,
                         yAxisLabel = NULL,
                         fixedXAxisMultiple = TRUE,
                         fixedYAxisMultiple = TRUE,
                         xAxisDateFormat = "%d/%m/%Y",
                         xAxisBreaks = NULL,
                         yAxisBreaks = NULL,
                         ...) {

  if (!inherits(x, "ptd_spc_df")) {
    stop("x argument must be an 'ptc_spc_df' objected, created by spc()")
  }
  .data <- x

  validatePlotOptions(pointSize,
                      percentageYAxis,
                      mainTitle,
                      xAxisLabel,
                      yAxisLabel,
                      fixedXAxisMultiple,
                      fixedYAxisMultiple,
                      xAxisDateFormat,
                      xAxisBreaks,
                      yAxisBreaks)

  # Colour Palette for ggplot
  .darkgrey <- "#7B7D7D"
  .orange <- "#fab428"
  .skyblue <- "#289de0"
  .purple <- "#361475"
  .red <- "#de1b1b"

  options <- attr(.data, "options")

  # set x axis breaks
  xaxislabels  <- if (is.null(xAxisBreaks)) {
    .data[["x"]]
  } else {
    xaxis <- .data[["x"]]
    start <- min(xaxis, na.rm = TRUE)
    end <- max(xaxis, na.rm = TRUE)

    seq.Date(from = as.Date(start), to = as.Date(end), by = xAxisBreaks)
  }

  plot <- ggplot(.data, aes(x = .data$x, y = .data$y)) +
    theme_minimal() +
    geom_line(aes(y = .data$upl), linetype = "dashed", size = pointSize / 2.666666, color = .darkgrey) +
    geom_line(aes(y = .data$lpl), linetype = "dashed", size = pointSize / 2.666666, color = .darkgrey) +
    geom_line(aes(y = .data$target), linetype = "dashed", size = pointSize / 2.666666, color = .purple, na.rm = TRUE) +
    geom_line(aes(y = .data$trajectory), linetype = "dashed", size = pointSize / 2.666666, color = .red, na.rm = TRUE) +
    geom_line(aes(y = mean)) +
    geom_line(color = .darkgrey, size = pointSize / 2.666666) +
    geom_point(color = .darkgrey, size = pointSize) +
    geom_point(aes(x = .data$x, y = .data$specialCauseImprovement), color = .skyblue, size = pointSize, na.rm = TRUE) +
    geom_point(aes(x = .data$x, y = .data$specialCauseConcern), color = .orange, size = pointSize, na.rm = TRUE) +
    labs(title = mainTitle,
         x = xAxisLabel %||% capitalise(options[["dateField"]]),
         y = yAxisLabel %||% capitalise(options[["valueField"]])) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(
      breaks = xaxislabels,
      labels = format(xaxislabels, format = xAxisDateFormat)
    ) +
    theme(
      plot.margin = unit(c(5, 5, 5, 5), "mm"), #5mm of white space around plot edge
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major.x = element_blank(), #remove major x gridlines
      panel.grid.minor.x = element_blank() #remove minor x gridlines
    )

  # Apply facet wrap if a facet field is present
  if (!is.null(options$facetField)) {
    # For multiple facet chart, derived fixed/free scales value from x and y axis properties
    facetScales <- if (fixedXAxisMultiple) {
      ifelse(fixedYAxisMultiple, "fixed", "free_y")
    } else {
      ifelse(fixedYAxisMultiple, "free_x", "free")
    }

    plot <- plot +
      facet_wrap(vars(.data$f), scales = facetScales)
  }

  if (percentageYAxis %||% FALSE) {
    plot <- plot +
      scale_y_continuous(labels = scales::percent_format(yAxisBreaks %||% 0.1))
  } else if (!is.null(yAxisBreaks)) {
    yaxis <- c(.data[["y"]], .data[["upl"]], .data[["lpl"]])
    start <- floor(min(yaxis, na.rm = TRUE) / yAxisBreaks) * yAxisBreaks
    end <- max(yaxis, na.rm = TRUE)

    yaxislabels <- seq(from = start, to = end, by = yAxisBreaks)

    plot <- plot +
      scale_y_continuous(breaks = yaxislabels, labels = yaxislabels)
  }

  # finally, apply any theme overrides
  plot <- plot +
    plotOptions$themeOverride

  plot
}

#' @rdname createGgplot
#' @export
plot.ptd_spc_df <- function(x,
                            pointSize = 2.5,
                            percentageYAxis = FALSE,
                            mainTitle = "SPC Chart",
                            xAxisLabel = NULL,
                            yAxisLabel = NULL,
                            fixedXAxisMultiple = TRUE,
                            fixedYAxisMultiple = TRUE,
                            xAxisDateFormat = "%d/%m/%Y",
                            xAxisBreaks = NULL,
                            yAxisBreaks = NULL,
                            ...) {
  createGgplot(x,
               pointSize,
               percentageYAxis,
               mainTitle,
               xAxisLabel,
               yAxisLabel,
               fixedXAxisMultiple,
               fixedYAxisMultiple,
               xAxisDateFormat,
               xAxisBreaks,
               yAxisBreaks)
}

validatePlotOptions <- function(pointSize = NULL,
                                percentageYAxis = NULL,
                                mainTitle = NULL,
                                xAxisLabel = NULL,
                                yAxisLabel = NULL,
                                fixedXAxisMultiple = NULL,
                                fixedYAxisMultiple = NULL,
                                xAxisDateFormat = NULL,
                                xAxisBreaks = NULL,
                                yAxisBreaks = NULL) {
  if (!is.null(pointSize)) {
    assertthat::assert_that(
      is.numeric(pointSize),
      assertthat::is.scalar(pointSize),
      pointSize > 0,
      pointSize <= 10,
      msg = "pointSize must be a single number greater than 0 and less than or equal to 10."
    )
  }

  if (!is.null(percentageYAxis)) {
    assertthat::assert_that(
      is.logical(percentageYAxis),
      assertthat::is.scalar(percentageYAxis),
      msg = "percentageYAxis argument must a single logical."
    )
  }

  if (!is.null(mainTitle)) {
    assertthat::assert_that(
      is.character(mainTitle),
      assertthat::is.scalar(mainTitle),
      msg = "mainTitle argument must be a character of length 1."
    )
  }

  if (!is.null(xAxisLabel)) {
    assertthat::assert_that(
      is.character(xAxisLabel),
      assertthat::is.scalar(xAxisLabel),
      msg = "xAxisLabel argument must be a character of length 1."
    )
  }

  if (!is.null(yAxisLabel)) {
    assertthat::assert_that(
      is.character(yAxisLabel),
      assertthat::is.scalar(yAxisLabel),
      msg = "yAxisLabel argument must be a character of length 1."
    )
  }

  if (!is.null(fixedXAxisMultiple)) {
    assertthat::assert_that(
      is.logical(fixedXAxisMultiple),
      assertthat::is.scalar(fixedXAxisMultiple),
      msg = "fixedXAxisMultiple argument must be a logical of length 1."
    )
  }

  if (!is.null(fixedYAxisMultiple)) {
    assertthat::assert_that(
      is.logical(fixedYAxisMultiple),
      assertthat::is.scalar(fixedYAxisMultiple),
      msg = "fixedYAxisMultiple argument must be a logical of length 1."
    )
  }

  if (!is.null(xAxisDateFormat)) {
    assertthat::assert_that(
      is.character(xAxisDateFormat),
      assertthat::is.scalar(xAxisDateFormat),
      msg = "xAxisDateFormat argument must be a character of length 1."
    )
  }

  if (!is.null(xAxisBreaks)) {
    assertthat::assert_that(
      is.character(xAxisBreaks),
      assertthat::is.scalar(xAxisBreaks),
      grepl("^\\d+ (day|week|month|quarter|year)s?$", xAxisBreaks),
      msg = paste0(
        "xAxisBreaks argument must be a character of length 1, and be a valid string for seq.Date 'by'. ",
        "See seq.Date for more information."
      )
    )
  }

  if (!is.null(yAxisBreaks)) {
    assertthat::assert_that(
      is.numeric(yAxisBreaks),
      assertthat::is.scalar(yAxisBreaks),
      msg = "yAxisBreaks argument must be a numeric of length 1."
    )
  }

  invisible(TRUE)
}
