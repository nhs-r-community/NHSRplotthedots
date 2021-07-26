#' SPC Options Function
#'
#' `spcOptions` returns a list object containing properties which adjust the output of the 'spc' function.
#'
#' This function is designed to allow greater control over SPC charts created using this package.  spcOptions is a list
#' with named slots for known parameters within the spc function. It should be supplied to the options argument within
#' the spc function, with the options listed within spcOptions.  See examples below.
#'
#'
#' @param rebase Specify a field name which contains a control limit rebasing flag.
#'     This field should contain integer values 0 and 1, and any date value where the rebase field is 1 will
#'     trigger a recalculation of the control limits.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param fixAfterNPoints Specify a number points after which to fix SPC calculations.
#' @param improvementDirection Specify whether an increase or decrease in measured variable signifies
#'     process improvement. Accepted values are 1 or 'increase' for increase as improvement or -1 or 'decrease' for
#'     decrease as improvement.
#' @param outputChart Specify whether the function should output a ggplot object or a data table of SPC values.
#'     Accepted values are TRUE for a chart output, or FALSE for a data table output.
#' @param pointSize Specify the plotting point size for the ggplot output. Default is 2.5.
#' @param percentageYAxis Specify whether the y axis values are percentages. Percentages in the data frame should be
#'     decimal values. Accepted values are TRUE for percentage y axis, FALSE for integer y axis.
#' @param target Specify a field name which contains a target value.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param trajectory Specify a field name which contains a trajectory value.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
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
#' @param plotThemeOverride Specify a list containing ggplot theme elements which can be used to override the default
#'     appearance of the plot.
#'
#' @export spcOptions
#'
#' @examples
#' library(NHSRdatasets)
#' data("ae_attendances")
#'
#' # Pick a few trust, and plot individually using facet
#' # Also set the xaxis scale to vary for each and date groups to 3 months, using spcOptions
#'
#' orgs <- ae_attendances$org_code %in% c("RAS", "RJZ", "RR1", "RJC", "RQ1")
#' trusts4 <- subset(ae_attendances, orgs & type == 1)
#'
#' # spcOptions should be supplied the the options argument within the spc function.
#' spc(trusts4,
#'   valueField = "breaches", dateField = "period", facetField = "org_code",
#'   options = spcOptions(
#'     improvementDirection = "decrease",
#'     fixedYAxisMultiple = FALSE,
#'     xAxisBreaks = "3 months"
#'   )
#' )
spcOptions <- function(rebase = NULL,
                       fixAfterNPoints = NULL,
                       improvementDirection = NULL,
                       outputChart = NULL,
                       pointSize = NULL,
                       percentageYAxis = NULL,
                       target = NULL,
                       trajectory = NULL,
                       mainTitle = NULL,
                       xAxisLabel = NULL,
                       yAxisLabel = NULL,
                       fixedXAxisMultiple = NULL,
                       fixedYAxisMultiple = NULL,
                       xAxisDateFormat = NULL,
                       xAxisBreaks = NULL,
                       yAxisBreaks = NULL,
                       plotThemeOverride = NULL) {
  if (!(is.null(improvementDirection))) {
    if (length(improvementDirection) > 1) {
      stop(
        "Improvement direction should be numeric (1 or -1) or character (increase or decrease). ",
        "Multiple values are not valid."
      )
    } else {
      if (is.numeric(improvementDirection)) {
        if (improvementDirection != 1 && improvementDirection != -1) {
          stop("Improvment direction should be set as 1 for 'increase' or -1 for 'decrease'.")
        }
      } else if (is.character(improvementDirection)) {
        if (improvementDirection != "increase" && improvementDirection != "decrease") {
          stop("Improvment direction should be set as 'increase' or 'decrease'.")
        }
      } else {
        stop("Improvement direction should be numeric (1 or -1) or character (increase or decrease).")
      }
    }
  }

  if (!(is.null(outputChart))) {
    if (length(outputChart) > 1) {
      stop("outputChart should be a logical vector of length 1.")
    } else if (!(is.logical(outputChart))) {
      stop("outputChart should be a logical vector of length 1.")
    }
  }

  if (!(is.null(fixAfterNPoints))) {
    if (length(fixAfterNPoints) > 1) {
      stop("fixAfterNPoints should be a numeric vector of length 1.")
    } else if (!(is.numeric(fixAfterNPoints))) {
      stop("fixAfterNPoints should be a numeric vector of length 1.")
    }
  }

  if (!(is.null(pointSize))) {
    if (length(pointSize) > 1) {
      stop("pointSize should be a numeric vector of length 1.")
    } else if (!(is.numeric(pointSize))) {
      stop("pointSize should be a numeric vector of length 1.")
    }
  }

  if (!(is.null(percentageYAxis))) {
    if (length(percentageYAxis) > 1) {
      stop("percentageYAxis should be a logical vector or decimal value of length 1.")
    } else if (!(is.logical(percentageYAxis)) && !(is.numeric(percentageYAxis))) {
      stop("percentageYAxis should be a logical vector or decimal value of length 1.")
    }
  }

  if (!(is.null(mainTitle))) {
    if (length(mainTitle) > 1) {
      stop("mainTitle should be a character vector of length 1.")
    } else if (!(is.character(mainTitle))) {
      stop("mainTitle should be a character vector of length 1.")
    }
  }

  if (!(is.null(xAxisLabel))) {
    if (length(xAxisLabel) > 1) {
      stop("xAxisLabel should be a character vector of length 1.")
    } else if (!(is.character(xAxisLabel))) {
      stop("xAxisLabel should be a character vector of length 1.")
    }
  }

  if (!(is.null(yAxisLabel))) {
    if (length(yAxisLabel) > 1) {
      stop("yAxisLabel should be a character vector of length 1.")
    } else if (!(is.character(yAxisLabel))) {
      stop("yAxisLabel should be a character vector of length 1.")
    }
  }

  if (!(is.null(xAxisDateFormat))) {
    if (length(xAxisDateFormat) > 1) {
      stop("xAxisDateFormat should be a character vector of length 1.")
    } else if (!(is.character(xAxisDateFormat))) {
      stop("xAxisDateFormat should be a character vector of length 1.")
    }
  }

  if (!(is.null(fixedXAxisMultiple))) {
    if (length(fixedXAxisMultiple) > 1) {
      stop("fixedXAxisMultiple should be a logical vector of length 1.")
    } else if (!(is.logical(fixedXAxisMultiple))) {
      stop("fixedXAxisMultiple should be a logical vector of length 1.")
    }
  }

  if (!(is.null(fixedYAxisMultiple))) {
    if (length(fixedYAxisMultiple) > 1) {
      stop("fixedYAxisMultiple should be a logical vector of length 1.")
    } else if (!(is.logical(fixedYAxisMultiple))) {
      stop("fixedYAxisMultiple should be a logical vector of length 1.")
    }
  }

  list(
    rebase = rebase,
    fixAfterNPoints = fixAfterNPoints,
    improvementDirection = improvementDirection,
    outputChart = outputChart,
    pointSize = pointSize,
    percentageYAxis = percentageYAxis,
    target = target,
    trajectory = trajectory,
    mainTitle = mainTitle,
    xAxisLabel = xAxisLabel,
    yAxisLabel = yAxisLabel,
    mainTitle = mainTitle,
    xAxisLabel = xAxisLabel,
    yAxisLabel = yAxisLabel,
    fixedXAxisMultiple = fixedXAxisMultiple,
    fixedYAxisMultiple = fixedYAxisMultiple,
    xAxisDateFormat = xAxisDateFormat,
    xAxisBreaks = xAxisBreaks,
    yAxisBreaks = yAxisBreaks,
    plotThemeOverride = plotThemeOverride
  )
}
