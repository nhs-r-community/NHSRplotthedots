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
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
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
#'     valueField = "breaches", dateField = "period",
#'     improvementDirection = "decrease")
#'
#' # Pick a few trust, and plot individually using facet
#' # Also set the x-axis scale to vary for each and date groups to 3 months
#' orgs <- ae_attendances$org_code %in% c("RAS", "RJZ", "RR1", "RJC", "RQ1")
#' trusts4 <- subset(ae_attendances, orgs & type == 1)
#'
#' s <- spc(trusts4,
#'          valueField = "breaches", dateField = "period", facetField = "org_code",
#'          improvementDirection = "decrease")
#' plot(s, fixedYAxisMultiple = FALSE, xAxisBreaks = "3 months")
#'
#' # Save the first chart as an object this time then alter the ggplot theme
#' my_spc <- spc(trust1,
#'               valueField = "breaches", dateField = "period",
#'               improvementDirection = "decrease")
#'
#' plot(my_spc) + ggplot2::theme_classic()
spc <- function(.data,
                valueField,
                dateField,
                facetField = NULL,
                rebase = NULL,
                fixAfterNPoints = NULL,
                improvementDirection = "increase",
                target = NULL,
                trajectory = NULL) {
  if (!inherits(.data, "data.frame")) {
    stop("spc: .data must be a data.frame")
  }

  # validate all inputs.  Validation problems will generate an error and stop code execution.
  options <- spcOptions(valueField, dateField, facetField, rebase, fixAfterNPoints, improvementDirection, target,
                        trajectory)

  validate(options, .data)

  # Declare improvement direction as integer
  improvementDirection <- ifelse(options$improvementDirection == "increase", 1, -1)

  df <- .data %>%
    spcStandard(options) %>%
    calculatePointHighlighting(improvementDirection)

  class(df) <- c("ptd_spc_df", class(df))
  attr(df, "options") <- options

  df
}

#' @export
print.ptd_spc_df <- function(x, ...) {
  p <- plot(x, ...)
  print(p)
}
