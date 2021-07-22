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
#' @param target Specify a field name which contains a target value.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param trajectory Specify a field name which contains a trajectory value.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
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
                       target = NULL,
                       trajectory = NULL) {

  if (!is.null(rebase) && !(is.character(rebase) && length(rebase) == 1)) {
    stop("spcOptions: rebase argument must be a 'character' of length 1.")
  }

  if (!is.null(fixAfterNPoints) && !(
    is.numeric(fixAfterNPoints) &&
    length(fixAfterNPoints) == 1 &&
    fixAfterNPoints >= 12
  )) {
    stop("spcOptions: fixAfterNPoints must be a single numeric that is greater than or equal to 12.")
  }

  if (!is.null(improvementDirection) && (
    length(improvementDirection) > 1 ||
    (is.numeric(improvementDirection) && !improvementDirection %in% c(-1, 1)) ||
    (is.character(improvementDirection) && !improvementDirection %in% c("increase", "decrease"))
  )) {
    stop("spcOptions: Improvement direction should be a single numeric (1 or -1) or character (increase or decrease).")
  }

  if (!is.null(target) && !(is.character(target) && length(target) == 1)) {
    stop("spcOptions: target argument must be a 'character' of length 1.")
  }

  if (!is.null(trajectory) && !(is.character(trajectory) && length(trajectory) == 1)) {
    stop("spcOptions: trajectory argument must be a 'character' of length 1.")
  }

  structure(
    list(
      rebase = rebase,
      fixAfterNPoints = fixAfterNPoints,
      improvementDirection = improvementDirection,
      target = target,
      trajectory = trajectory
    ),
    class = "ptd_spc_options"
  )
}

# double dispatch
validate.ptd_spc_options <- function(options, .data) {
  UseMethod("validate.ptd_spc_options", .data)
}

validate.ptd_spc_options.data.frame <- function(options, .data) {
  check <- function(op) {
    if (is.null(options[[op]])) return (TRUE)
    if (options[[op]] %in% colnames(.data)) return (TRUE)
    stop(op, ": '", options[[op]], "' must be a valid column name in the data frame.")
  }
  check("rebase")
  check("target")
  check("trajectory")

  invisible(TRUE)
}

print.ptd_spc_options <- function(x, ...) {
  f <- function(s, surround = "'") {
    if (is.null(s)) {
      crayon::blue("not set")
    } else {
      crayon::red(paste0(surround, s, surround))
    }
  }

  l <- min(
    max(sapply(x, function(v) {
      if (!is.character(v)) return(8)
      length(strsplit(v, "")[[1]])
    })) + 24,
    120
  )

  lines <- c(
    crayon::bold("Plot the Dots SPC options:"),
    paste(rep("=", l), collapse = ""),
    paste0(crayon::bold("rebase:"), "               ", f(x$rebase)),
    paste0(crayon::bold("fixAfterNPoints:"), "      ", f(x$fixAfterNPoints, "")),
    paste0(crayon::bold("improvementDirection:"), " ", f(x$improvementDirection)),
    paste0(crayon::bold("target:"), "               ", f(x$target)),
    paste0(crayon::bold("trajectory:"), "           ", f(x$trajectory)),
    paste(rep("-", l), collapse = "")
  )

  cat(lines, sep = "\n")
}

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

# pointSize = NULL,
# percentageYAxis = NULL,
# mainTitle = NULL,
# xAxisLabel = NULL,
# yAxisLabel = NULL,
# fixedXAxisMultiple = NULL,
# fixedYAxisMultiple = NULL,
# xAxisDateFormat = NULL,
# xAxisBreaks = NULL,
# yAxisBreaks = NULL) {

# if (!(is.null(pointSize))) {
#   if (length(pointSize) > 1) {
#     stop("pointSize should be a numeric vector of length 1.")
#   } else if (!(is.numeric(pointSize))) {
#     stop("pointSize should be a numeric vector of length 1.")
#   }
# }
#
# if (!(is.null(percentageYAxis))) {
#   if (length(percentageYAxis) > 1) {
#     stop("percentageYAxis should be a logical vector or decimal value of length 1.")
#   } else if (!(is.logical(percentageYAxis)) && !(is.numeric(percentageYAxis))) {
#     stop("percentageYAxis should be a logical vector or decimal value of length 1.")
#   }
# }
#
# if (!(is.null(mainTitle))) {
#   if (length(mainTitle) > 1) {
#     stop("mainTitle should be a character vector of length 1.")
#   } else if (!(is.character(mainTitle))) {
#     stop("mainTitle should be a character vector of length 1.")
#   }
# }
#
# if (!(is.null(xAxisLabel))) {
#   if (length(xAxisLabel) > 1) {
#     stop("xAxisLabel should be a character vector of length 1.")
#   } else if (!(is.character(xAxisLabel))) {
#     stop("xAxisLabel should be a character vector of length 1.")
#   }
# }
#
# if (!(is.null(yAxisLabel))) {
#   if (length(yAxisLabel) > 1) {
#     stop("yAxisLabel should be a character vector of length 1.")
#   } else if (!(is.character(yAxisLabel))) {
#     stop("yAxisLabel should be a character vector of length 1.")
#   }
# }
#
# if (!(is.null(xAxisDateFormat))) {
#   if (length(xAxisDateFormat) > 1) {
#     stop("xAxisDateFormat should be a character vector of length 1.")
#   } else if (!(is.character(xAxisDateFormat))) {
#     stop("xAxisDateFormat should be a character vector of length 1.")
#   }
# }
#
# if (!(is.null(fixedXAxisMultiple))) {
#   if (length(fixedXAxisMultiple) > 1) {
#     stop("fixedXAxisMultiple should be a logical vector of length 1.")
#   } else if (!(is.logical(fixedXAxisMultiple))) {
#     stop("fixedXAxisMultiple should be a logical vector of length 1.")
#   }
# }
#
# if (!(is.null(fixedYAxisMultiple))) {
#   if (length(fixedYAxisMultiple) > 1) {
#     stop("fixedYAxisMultiple should be a logical vector of length 1.")
#   } else if (!(is.logical(fixedYAxisMultiple))) {
#     stop("fixedYAxisMultiple should be a logical vector of length 1.")
#   }
# }

# pointSize = pointSize,
# percentageYAxis = percentageYAxis,
# mainTitle = mainTitle,
# xAxisLabel = xAxisLabel,
# yAxisLabel = yAxisLabel,
# mainTitle = mainTitle,
# xAxisLabel = xAxisLabel,
# yAxisLabel = yAxisLabel,
# fixedXAxisMultiple = fixedXAxisMultiple,
# fixedYAxisMultiple = fixedYAxisMultiple,
# xAxisDateFormat = xAxisDateFormat,
# xAxisBreaks = xAxisBreaks,
# yAxisBreaks = yAxisBreaks
