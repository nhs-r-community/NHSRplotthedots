#' Create ggplot2
#'
#' Creates a ggplot2 object using the parameters passed in.
#'
#' @param .data A data frame containing the information to be plotted.
#' @param pointSize Specify the plotting point size for the ggplot output. Default is 2.5.
#' @param percentageYAxis Specify whether the y axis values are percentages. Percentages in the data frame should be
#'     decimal values. Accepted values are TRUE for percentage y axis, FALSE for integer y axis.
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
#' @return The ggplot2 object

create_spc_plot <- function(.data,
                            pointSize = 2.5,
                            percentageYAxis = FALSE,
                            mainTitle = NULL,
                            xAxisLabel = NULL,
                            yAxisLabel = NULL,
                            fixedXAxisMultiple = NULL,
                            fixedYAxisMultiple = NULL,
                            xAxisDateFormat = "%d/%m/%Y",
                            xAxisBreaks = NULL,
                            yAxisBreaks = NULL) {

  # Colour Palette for ggplot
  .darkgrey <- "#7B7D7D"
  .orange <- "#fab428"
  .skyblue <- "#289de0"
  .purple <- "#361475"
  .red <- "#de1b1b"

  options <- attr(.data, "options")

  # set x axis breaks
  if (is.null(options$xAxisBreaks)) {
    xaxislabels <- .data$x
  } else {
    xaxis <- .data$x
    start <- min(xaxis, na.rm = TRUE)
    end <- max(xaxis, na.rm = TRUE)
    xaxislabels <- seq.Date(from = as.Date(start), to = as.Date(end), by = options$xAxisBreaks)
  }

  # set point size
  pointSize <- ifelse(is.null(options$pointSize), 2, options$pointSize)

  # set x axis date format
  xAxisDateFormat <- ifelse(is.null(options$xAxisDateFormat), "%d/%m/%Y", options$xAxisDateFormat)

  # set main plot title
  plottitle <- ifelse(is.null(options$mainTitle), "SPC Chart", options$mainTitle)

  # set x axis label
  xlabel <- ifelse(is.null(options$xAxisLabel), capitalise(options$dateField), options$xAxisLabel)

  # set y axis label
  ylabel <- ifelse(is.null(options$yAxisLabel), capitalise(options$valueField), options$yAxisLabel)

  # set y axis breaks
  yAxisBreaks <- options$yAxisBreaks

  # set x axis fixed scaling for facet plots
  scaleXFixed <- ifelse(is.null(options$fixedXAxisMultiple), TRUE, options$fixedXAxis)

  # set y axis fixed scaling for facet plots
  scaleYFixed <- ifelse(is.null(options$fixedYAxisMultiple), TRUE, options$fixedYAxis)

  # For multiple facet chart, derived fixed/free scales value from x and y axis properties
  facetScales <- if (scaleYFixed == TRUE && scaleXFixed == TRUE) {
    "fixed"
  } else if (scaleYFixed == TRUE && scaleXFixed == FALSE) {
    "free_x"
  } else if (scaleYFixed == FALSE && scaleXFixed == TRUE) {
    "free_y"
  } else if (scaleYFixed == FALSE && scaleXFixed == FALSE) {
    "free"
  }

  # set percentage y axis
  convertToPercentages <- if (is.null(options$percentageYAxis)) {
    0
  } else if (is.numeric(options$percentageYAxis)) {
    options$percentageYAxis
  } else if (is.logical(options$percentageYAxis)) {
    0.1 * as.numeric(options$percentageYAxis)
  }

  if (!(is.null(yAxisBreaks))) {
    yaxis <- c(.data$y, .data$upl, .data$lpl)
    start <- floor(min(yaxis, na.rm = TRUE) / yAxisBreaks) * yAxisBreaks
    end <- max(yaxis, na.rm = TRUE)
    yaxislabels <- seq(from = start, to = end, by = yAxisBreaks)
  }

  plot <- ggplot(.data, aes(x = .data$x, y = .data$y)) +
    theme_minimal() +
    geom_line(aes(y = .data$upl), linetype = "dashed", size = pointSize / 2.666666, color = .darkgrey) +
    geom_line(aes(y = .data$lpl), linetype = "dashed", size = pointSize / 2.666666, color = .darkgrey) +
    geom_line(aes(y = .data$target), linetype = "dashed", size = pointSize / 2.666666, color = .purple, na.rm = TRUE) +
    geom_line(aes(y = .data$trajectory), linetype = "dashed", size = pointSize / 2.666666, color = .red, na.rm = TRUE) +
    geom_line(aes(y = mean)) +
    geom_line(color = .darkgrey, size = pointSize / 2.666666) +
    geom_point(color = .darkgrey, size = pointSize)

  # Apply facet wrap if a facet field is present
  if (!is.null(options$facetField)) {
    plot <- plot +
      facet_wrap(vars(.data$f), scales = facetScales)
  }

  plot <- plot +
    geom_point(aes(x = .data$x, y = .data$specialCauseImprovement), color = .skyblue, size = pointSize, na.rm = TRUE) +
    geom_point(aes(x = .data$x, y = .data$specialCauseConcern), color = .orange, size = pointSize, na.rm = TRUE) +
    labs(title = plottitle,
         x = xlabel,
         y = ylabel) +
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

  # if the plot is not faceted (ie it's the default facet column name)
  if (is.null(options$facetField)) {
    if (convertToPercentages == FALSE) {
      if (!(is.null(yAxisBreaks))) {
        plot <- plot +
          scale_y_continuous(breaks = yaxislabels, labels = yaxislabels)
      }
    } else if (convertToPercentages != 0) {
      percentLimit <- max(.data$upl, na.rm = TRUE)

      interval <- if (!(is.null(yAxisBreaks))) {
        yAxisBreaks
      } else {
        convertToPercentages
      }

      plot <- plot +
        scale_y_continuous(labels = scales::percent, breaks = seq(from = 0, to = percentLimit, by = interval))
    }
    # else if the plot is faceted
  } else if (convertToPercentages != 0) {
    percentLimit <- max(.data$upl, na.rm = TRUE)

    plot <- plot +
      scale_y_continuous(labels = scales::percent)
  }

  # finally, apply any theme overrides
  plot <- plot +
    plotOptions$themeOverride

  plot
}

#' Plot ptd_spc_df object
#'
#' Plot function for a ptd_spc_df object. It calls [create_spc_plot()].
#'
#' @seealso create_spc_plot
#'
#' @export
#' @param x data passed to .data argument of [create_spc_plot()]
#' @param ... other arguments passed to [create_spc_plot()]
plot.ptd_spc_df <- function(x, ...) {
  create_spc_plot(x, ...)
}
