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
#' @param colours Specify the colours to use in the plot, use the [spcColours()] function to change defaults.
#' @param themeOverride Specify a list containing ggplot theme elements which can be used to override the default
#'     appearance of the plot.
#' @param ... currently ignored
#' @return The ggplot2 object
#' @export
createGgplot <- function(x,
                         pointSize = 4,
                         percentageYAxis = FALSE,
                         mainTitle = NULL,
                         xAxisLabel = NULL,
                         yAxisLabel = NULL,
                         fixedXAxisMultiple = TRUE,
                         fixedYAxisMultiple = TRUE,
                         xAxisDateFormat = "%d/%m/%y",
                         xAxisBreaks = NULL,
                         yAxisBreaks = NULL,
                         colours = spcColours(),
                         themeOverride = NULL,
                         ...) {

  assertthat::assert_that(
    inherits(x, "ptd_spc_df"),
    msg = "x argument must be an 'ptd_spc_df' object, created by spc()."
  )
  # argument needs to be called x for s3 plot method, but rename it to .data so it's more obvious through the rest of
  # the method
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
                      yAxisBreaks,
                      colours,
                      themeOverride)

  options <- attr(.data, "options")

  mainTitle <- if (is.null(mainTitle)) {
    paste0(
      "SPC Chart of ",
      capitalise(options$valueField),
      ", starting ",
      format(min(.data[["x"]], na.rm = TRUE), format = "%d/%m/%Y")
    )
  }

  lineSize <- pointSize / 3

  plot <- ggplot(.data, aes(x = .data$x, y = .data$y)) +
    geom_line(aes(y = .data$upl),
              linetype = "dashed", size = lineSize, colour = colours$upl) +
    geom_line(aes(y = .data$lpl),
              linetype = "dashed", size = lineSize, colour = colours$lpl) +
    geom_line(aes(y = .data$target),
              linetype = "dashed", size = lineSize, colour = colours$target, na.rm = TRUE) +
    geom_line(aes(y = .data$trajectory),
              linetype = "dashed", size = lineSize, colour = colours$trajectory, na.rm = TRUE) +
    geom_line(aes(y = mean),
              linetype = "solid", colour = colours$mean_line) +
    geom_line(linetype = "solid", size = lineSize, colour = colours$value_line) +
    geom_point(aes(colour = .data$pointType), size = pointSize) +
    scale_colour_manual(values = colours[c("common_cause",
                                           "special_cause_improvement",
                                           "special_cause_concern")],
                        labels = titleCase) +
    labs(title = mainTitle,
         x = xAxisLabel %||% capitalise(options[["dateField"]]),
         y = yAxisLabel %||% capitalise(options[["valueField"]])) +
    theme_minimal() +
    theme(
      plot.margin = unit(c(5, 5, 5, 5), "mm"), #5mm of white space around plot edge
      axis.text.x = element_text(angle = 90, hjust = 1),
      panel.grid.major.x = element_blank(), #remove major x gridlines
      panel.grid.minor.x = element_blank(), #remove minor x gridlines
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    themeOverride

  plot <- plot + if (is.null(xAxisBreaks)) {
    scale_x_datetime(
      breaks = sort(unique(.data$x)),
      date_labels = xAxisDateFormat
    )
  } else {
    scale_x_datetime(
      date_breaks = xAxisBreaks,
      date_labels = xAxisDateFormat
    )
  }

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

  plot
}

#' @rdname createGgplot
#' @export
plot.ptd_spc_df <- function(x,
                            pointSize = 4,
                            percentageYAxis = FALSE,
                            mainTitle = NULL,
                            xAxisLabel = NULL,
                            yAxisLabel = NULL,
                            fixedXAxisMultiple = TRUE,
                            fixedYAxisMultiple = TRUE,
                            xAxisDateFormat = "%d/%m/%y",
                            xAxisBreaks = NULL,
                            yAxisBreaks = NULL,
                            colours = spcColours(),
                            themeOverride = NULL,
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
               yAxisBreaks,
               colours,
               themeOverride,
               ...)
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
                                yAxisBreaks = NULL,
                                colours = NULL,
                                themeOverride = NULL) {
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

  if (!is.null(colours)) {
    assertthat::assert_that(
      inherits(colours, "ptd_spc_colours"),
      msg = "colours must be an object created by spcColours()."
    )
  }

  if (!is.null(themeOverride)) {
    assertthat::assert_that(
      inherits(themeOverride, c("theme", "gg")),
      msg = "themeOverride must be an object created by theme()."
    )
  }

  invisible(TRUE)
}
