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
#' @param facet_field Optional: Specify field name which contains a grouping/faceting variable. SPC logic will be
#'     applied to each group separately, with outputs combined. Currently accepts 1 variable only.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param rebase Specify a date vector of dates when to rebase, or, if facet_field is set, a named list of date vectors
#'     of when to rebase. Each item in the list should be named after the facet you wish to rebase. See [ptd_rebase()].
#' @param fix_after_n_points Specify a number points after which to fix SPC calculations.
#' @param improvement_direction Specify whether process improvement is represented by an increase or decrease in
#'     measured variable, or is neutral.  Accepted values are 'increase' for increase as improvement, 'decrease' for
#'     decrease as improvement, and 'neutral' where neither direction represents an improvement. Defaults to 'increase'.
#' @param target Specify a single value, which will apply the same target to every facet of an SPC chart,
#'     or named values of targets, where the names correspond to the names of the facets, in order to
#'     have different targets for each facet. See [ptd_target()].
#' @param trajectory Specify a field name which contains a trajectory value.
#'     Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param screen_outliers Whether we should screen for outliers or not when calculating the control limits. Defaults to
#'     `TRUE`.
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
#' library(dplyr)
#' data("ae_attendances")
#'
#' # Pick a trust at random to look at their data for two years
#' trust1 <- subset(ae_attendances, org_code == "RJZ" & type == 1)
#'
#' # Basic chart with improvement direction decreasing
#' ptd_spc(trust1,
#'   value_field = breaches, date_field = period,
#'   improvement_direction = "decrease"
#' )
#'
#' # Pick a few trust, and plot individually using facet
#' # Also set the x-axis scale to vary for each and date groups to 3 months
#' orgs <- c("RAS", "RJZ", "RR1", "RJC", "RQ1")
#' trusts4 <- filter(ae_attendances, org_code %in% orgs, type == 1)
#'
#' s <- ptd_spc(trusts4,
#'   value_field = breaches, date_field = period, facet_field = org_code,
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
                    facet_field,
                    rebase = ptd_rebase(),
                    fix_after_n_points = NULL,
                    improvement_direction = "increase",
                    target = ptd_target(),
                    trajectory,
                    screen_outliers = TRUE) {
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = "ptd_spc: .data must be a data.frame"
  )

  value_field <- quo_name(enquo(value_field))
  date_field <- quo_name(enquo(date_field))
  facet_field <- if (!missing(facet_field)) quo_name(enquo(facet_field))
  trajectory <- if (!missing(trajectory)) quo_name(enquo(trajectory))

  # validate all inputs.  Validation problems will generate an error and stop code execution.
  options <- ptd_spc_options(
    value_field, date_field, facet_field, rebase, fix_after_n_points, improvement_direction, target,
    trajectory, screen_outliers
  )

  ptd_validate_spc_options(options, .data)

  # promote value field to a double (in case it's an integer)
  .data[[value_field]] <- as.double(.data[[value_field]])

  .data[[date_field]] <- to_datetime(.data[[date_field]])

  # add rebase column
  .data <- ptd_add_rebase_column(.data, date_field, facet_field, rebase)

  # Declare improvement direction as integer
  improvement_direction <- switch(options$improvement_direction,
    "increase" = 1,
    "neutral" = 0,
    "decrease" = -1
  )

  df <- .data %>%
    ptd_spc_standard(options) %>%
    ptd_calculate_point_type(improvement_direction) %>%
    ptd_add_short_group_warnings() %>%
    # add target column: we need to have called ptd_spc_standard to add the facet field
    ptd_add_target_column(target)

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


  if (!is.null(options$target)) {
    at <- ptd_calculate_assurance_type(object)

    s <- s %>%
      inner_join(at, by = "f") %>%
      group_by(.data$f) %>%
      mutate(assurance_type = ifelse(.data$rebase_group == max(.data$rebase_group),
                                     .data$assurance_type,
                                     as.character(NA))) %>%
      ungroup()
  }

  if (is.null(options$facet_field)) {
    s <- select(s, -.data$f)
  }

  if (is.null(options$rebase)) {
    s <- select(s, -.data$rebase_group)
  }

  s
}
