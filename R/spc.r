#' SPC Plotting Function
#'
#' `spc` returns a plot object or data table with SPC values using NHSI 'plot the dots' logic.
#'
#' This function is designed to produce consistent SPC charts
#' across Information Department reporting, according to the 'plot the dots'
#' logic produced by NHSI. The function can return either a plot or data frame.
#'
#'
#' @param .data A data frame containing a value field, a date field,
#' and a category field (if for faceting). There should be no gaps in the time series
#' for each category.
#' @param valueField Specify the field name which contains the value data, to be plotted on y axis.
#' Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param dateField Specify the field name which contains the date data, to be plotted on x axis.
#' Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param facetField Optional: Specify field name which contains a grouping/faceting variable. SPC logic will be applied
#'     to each group separately, with outputs combined. Currently accepts 1 variable only.
#' Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param options Optional: A list object containing additional control and formatting properties. Preferably created
#'     using the spcOptions function.
#'
#' @export spc
#'
#' @return A ggplot2 object of the spc charts.  This will automatically print the plot, but can also be saved as an
#'     object if you want to manipulate it further.
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom rlang .data
#' @examples
#' library(NHSRdatasets)
#' data("ae_attendances")
#'
#' # Pick a trust at random to look at their data for two years
#' trust1 <- subset(ae_attendances, org_code == "RJZ" & type == 1)
#'
#' # Basic chart with improvement direction decreasing
#' spc(trust1,
#'   valueField = "breaches", dateField = "period",
#'   options = spcOptions(improvementDirection = "decrease")
#' )
#'
#'
#' # Pick a few trust, and plot individually using facet
#' # Also set the x-axis scale to vary for each and date groups to 3 months
#' orgs <- ae_attendances$org_code %in% c("RAS", "RJZ", "RR1", "RJC", "RQ1")
#' trusts4 <- subset(ae_attendances, orgs & type == 1)
#'
#' spc(trusts4,
#'   valueField = "breaches", dateField = "period", facetField = "org_code",
#'   options = spcOptions(
#'     improvementDirection = "decrease",
#'     fixedYAxisMultiple = FALSE,
#'     xAxisBreaks = "3 months"
#'   )
#' )
#'
#'
#' # Save the first chart as an object this time then alter the ggplot theme
#' my_spc <- spc(trust1,
#'   valueField = "breaches", dateField = "period",
#'   options = spcOptions(improvementDirection = "decrease")
#' )
#' my_spc + ggplot2::theme_classic()
spc <- function(.data,
                valueField,
                dateField,
                facetField = NULL,
                options = NULL) {
  # validate all inputs.  Validation problems will generate an error and stop code execution.
  validateParameters(.data, valueField, dateField, facetField, options)

  if (is.null(facetField)) { # If no facet field specified, bind a pseudo-facet field for grouping/joining purposes
    facetField <- "pseudo_facet_col_name"
  }

  df <- spcStandard(.data, valueField, dateField, facetField, options)

  # Declare improvement direction as integer
  improvementDirection <- if (is.null(options$improvementDirection)) {
    1
  } else if (options$improvementDirection == "increase") {
    1
  } else if (options$improvementDirection == "decrease") {
    -1
  } else {
    options$improvementDirection
  }

  # set output chart
  outputChart <- is.null(options$outputChart) || options$outputChart

  # set x axis breaks
  if (is.null(options$xAxisBreaks)) {
    xaxislabels <- df$x
  } else {
    xaxis <- df$x
    start <- min(xaxis, na.rm = TRUE)
    end <- max(xaxis, na.rm = TRUE)
    xaxislabels <- seq.Date(from = as.Date(start), to = as.Date(end), by = options$xAxisBreaks)
  }

  # set point size
  pointSize <- ifelse(is.null(options$pointSize), 4, options$pointSize)

  # set x axis date format
  xAxisDateFormat <- ifelse(is.null(options$xAxisDateFormat), "%d/%m/%y", options$xAxisDateFormat)

  # set main plot title
  plottitle <- ifelse(
    is.null(options$mainTitle),
    paste0(
      "SPC Chart of ", capitalise(valueField), ", starting ", format(min(df$x, na.rm = TRUE), format = "%d/%m/%Y")
    ),
    options$mainTitle
  )

  # set x axis label
  xlabel <- ifelse(is.null(options$xAxisLabel), capitalise(dateField), options$xAxisLabel)

  # set y axis label
  ylabel <- ifelse(is.null(options$yAxisLabel), capitalise(valueField), options$yAxisLabel)

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

  # set plot theme override
  themeOverride <- if (is.null(options$plotThemeOverride)) {
    NULL
  } else {
    options$plotThemeOverride
  }

  ## Plot the dots SPC logic ----
  df <- calculatePointHighlighting(df, improvementDirection)

  ## Create ggplot using plot the dots colours OR output data frame ----
  # Create chart if required
  if (outputChart) {

    # build a list of plotOptions to pass into the createGgplot() function
    plotOptions <- list(
      pointSize = pointSize,
      plottitle = plottitle,
      xlabel = xlabel,
      ylabel = ylabel,
      xaxislabels = xaxislabels,
      xAxisDateFormat = xAxisDateFormat,
      convertToPercentages = convertToPercentages,
      facetScales = facetScales,
      yAxisBreaks = yAxisBreaks,
      themeOverride = themeOverride
    )

    # make and return the plot
    return(createGgplot(df, facetField, plotOptions))
  }

  df
}
