% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spcOptions.r
\name{spcOptions}
\alias{spcOptions}
\title{SPC Options Function}
\usage{
spcOptions(
  rebase = NULL,
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
  yAxisBreaks = NULL
)
}
\arguments{
\item{rebase}{Specify a field name which contains a control limit rebasing flag.
This field should contain integer values 0 and 1, and any date value where the rebase field is 1 will
trigger a recalculation of the control limits.
Field name can be specified using non-standard evaluation (i.e. no quotation marks).}

\item{improvementDirection}{Specify whether an increase or decrease in measured variable signifies
process improvement. Accepted values are 1 or 'increase' for increase as improvement or -1 or 'decrease' for decrease as improvement.}

\item{outputChart}{Specify whether the function should output a ggplot object or a data table of SPC values.
Accepted values are TRUE for a chart output, or FALSE for a data table output.}

\item{pointSize}{Specify the plotting point size for the ggplot output. Default is 2.5.}

\item{percentageYAxis}{Specify whether the y axis values are percentages. Percentages in the data frame should be
decimal values. Accepted values are TRUE for percentage y axis, FALSE for integer y axis.}

\item{target}{Specify a field name which contains a target value.
Field name can be specified using non-standard evaluation (i.e. no quotation marks).}

\item{trajectory}{Specify a field name which contains a trajectory value.
Field name can be specified using non-standard evaluation (i.e. no quotation marks).}

\item{mainTitle}{Specify a character string value for the ggplot title.}

\item{xAxisLabel}{Specify a character string value for the x axis title.}

\item{yAxisLabel}{Specify a character string value for the y axis title.}

\item{fixedXAxisMultiple}{Specify whether, if producing a faceted spc, x axis should be fixed for all facet plots.
Accepted values are TRUE for fixed x axes or FALSE for individual x axes.}

\item{fixedYAxisMultiple}{Specify whether, if producing a faceted spc, y axis should be fixed for all facet plots.
Accepted values are TRUE for fixed y axes or FALSE for individual y axes.}

\item{xAxisDateFormat}{Specify how dates on the x axis should be displayed. Format should be provided
as a character string using 'd m Y' etc syntax.}

\item{xAxisBreaks}{Specify an interval value for breaks on the x axis. Value should be a character string expressing
interval length and type, e.g. "3 months", "7 days".}

\item{yAxisBreaks}{Specify an interval value for breaks on the y axis. Value should be a numeric vector of length 1,
either an integer for integer scales or a decimal value for percentage scales. This option is ignored if faceting is in use.}
}
\description{
`spcOptions` returns a list object containing properties which adjust the output of the 'spc' function.
}
\details{
This function is designed to allow greater control over SPC charts
created using this package.
}
