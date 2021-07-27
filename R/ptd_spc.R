#' SPC Plotting Function
#'
#' `ptd_spc` returns a plot object or data table with SPC values using NHSI 'plot the dots' logic.
#'
#' This function is designed to produce consistent SPC charts
#' across Information Department reporting, according to the 'plot the dots'
#' logic produced by NHSI. The function can return either a plot or data frame.
#'
#'
#' @param .data A data frame containing a value field, a date field,
#' and a category field (if for faceting). There should be no gaps in the time series
#' for each category.
#' @param value_field Specify the field name which contains the value data, to be plotted on y axis.
#' Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param date_field Specify the field name which contains the date data, to be plotted on x axis.
#' Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param facet_field Optional: Specify field name which contains a grouping/faceting variable. SPC logic will be applied
#'     to each group separately, with outputs combined. Currently accepts 1 variable only.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param rebase Specify a field name which contains a control limit rebasing flag.
#'     This field should contain integer values 0 and 1, and any date value where the rebase field is 1 will
#'     trigger a recalculation of the control limits.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param fix_after_n_points Specify a number points after which to fix SPC calculations.
#' @param improvement_direction Specify whether an increase or decrease in measured variable signifies
#'     process improvement. Accepted values are 'increase' for increase as improvement or 'decrease' for
#'     decrease as improvement. Defaults to 'increase'.
#' @param target Specify a field name which contains a target value.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param trajectory Specify a field name which contains a trajectory value.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#'
#' @export ptd_spc
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
#' ptd_spc(trust1,
#'   value_field = "breaches", date_field = "period",
#'   improvement_direction = "decrease"
#' )
#'
#' # Pick a few trust, and plot individually using facet
#' # Also set the x-axis scale to vary for each and date groups to 3 months
#' orgs <- ae_attendances$org_code %in% c("RAS", "RJZ", "RR1", "RJC", "RQ1")
#' trusts4 <- subset(ae_attendances, orgs & type == 1)
#'
#' s <- ptd_spc(trusts4,
#'   value_field = "breaches", date_field = "period", facet_field = "org_code",
#'   improvement_direction = "decrease"
#' )
#' plot(s, fixed_y_axis_multiple = FALSE, x_axis_breaks = "3 months")
#'
#' # Save the first chart as an object this time then alter the ggplot theme
#' my_spc <- ptd_spc(trust1,
#'   value_field = "breaches", date_field = "period",
#'   improvement_direction = "decrease"
#' )
#'
#' plot(my_spc) + ggplot2::theme_classic()
ptd_spc <- function(.data,
                    value_field,
                    date_field,
                    facet_field = NULL,
                    rebase = NULL,
                    fix_after_n_points = NULL,
                    improvement_direction = "increase",
                    target = NULL,
                    trajectory = NULL) {
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "ptd_spc: .data must be a data.frame"
  )

  # validate all inputs.  Validation problems will generate an error and stop code execution.
  options <- ptd_spc_options(
    value_field, date_field, facet_field, rebase, fix_after_n_points, improvement_direction, target,
    trajectory
  )

  ptd_validate_spc_options(options, .data)

  .data[[date_field]] <- as.POSIXct(.data[[date_field]], tz = "utc")

  # Declare improvement direction as integer
  improvement_direction <- ifelse(options$improvement_direction == "increase", 1, -1)

  df <- .data %>%
    ptd_spc_standard(options) %>%
    ptd_calculate_point_highlighting(improvement_direction)

  class(df) <- c("ptd_spc_df", class(df))
  attr(df, "options") <- options

  df
}

#' @export
print.ptd_spc_df <- function(x, ...) {
  p <- plot(x, ...)
  print(p)
}

#' @export
summary.ptd_spc_df <- function(object, ...) {
  options <- attr(object, "options")
  print(options)

  s <- object %>%
    group_by(.data$f, .data$rebase_group) %>%
    summarise(across(c(.data$mean, .data$lpl, .data$upl), first),
      n = n(),
      common_cause = n - sum(.data$special_cause_flag),
      special_cause_improvement = sum(.data$point_type == "special_cause_improvement"),
      special_cause_concern = sum(.data$point_type == "special_cause_concern"),
      .groups = "drop"
    )

  if (is.null(options$facet_field)) {
    s <- select(s, -.data$f)
  }

  if (is.null(options$rebase)) {
    s <- select(s, -.data$rebase_group)
  }

  s
}
