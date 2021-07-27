#' SPC Standard Calculations (internal function)
#'
#' Returns a data frame containing SPC fields which are common to all methodologies, including 'plot the dots'
#'
#' This function is designed to produce consistent SPC charts
#' across Information Department reporting, according to the 'plot the dots'
#' logic produced by NHSI. The function can return either a plot or data frame.
#'
#'
#' @param .data A data frame containing a value field, a date field,
#' and a category field (if for faceting). There should be no gaps in the time series
#' for each category.
#' @param options created by spcOptions function
#'
#' @noRd

#' @import dplyr

ptd_spc_standard <- function(.data, options = NULL) {
  # get values from options
  value_field <- options$value_field
  date_field <- options$date_field
  facet_field <- options$facet_field
  rebaseField <- options$rebase
  fix_after_n_points <- options$fix_after_n_points
  targetField <- options$target
  trajectoryField <- options$trajectory

  # set trajectory field
  if (is.null(trajectoryField)) {
    .data$trajectory <- rep(as.numeric(NA), nrow(.data))
  } else {
    .data$trajectory <- .data[[trajectoryField]]
  }

  # Set target field or create pseudo
  if (is.null(targetField)) {
    .data$target <- rep(as.numeric(NA), nrow(.data))
  } else {
    .data$target <- .data[[targetField]]
  }

  # Set facet/grouping or create pseudo
  # If no facet field specified, bind a pseudo-facet field for grouping/joining purposes
  if (is.null(facet_field)) {
    .data$facet <- "no facet"
  } else {
    .data$facet <- .data[[facet_field]]
  }

  # Set rebase field or create pseudo
  if (!(is.null(rebaseField))) {
    .data$rebase <- .data[[rebaseField]]
  } else {
    .data$rebase <- rep(0, nrow(.data)) # If no rebase field supplied, create an empty rebase field (all NAs)
  }

  # Constants
  limit <- 2.66
  limitclose <- 2 * (limit / 3)

  # Restructure starting data frame
  .data <- .data %>%
    select(
      y = .data[[value_field]],
      x = .data[[date_field]],
      f = .data$facet,
      rebase = .data$rebase,
      trajectory = .data$trajectory,
      target = .data$target
    ) %>%
    # Group data frame by facet
    group_by(.data$f) %>%
    # Order data frame by facet, and x axis variable
    arrange(.data$f, .data$x) %>%
    # convert rebase 0/1's to group indices
    mutate(rebase_group = cumsum(.data$rebase)) %>%
    group_by(.data$rebase_group, .add = TRUE) %>%
    mutate(
      fixY = ifelse(row_number() <= (fix_after_n_points %||% Inf), .data$y, NA),
      mean = mean(.data$fixY, na.rm = TRUE),
      amr = mean(abs(diff(.data$fixY)), na.rm = TRUE),
      # identify lower/upper process limits
      lpl = .data$mean - (limit * .data$amr),
      upl = .data$mean + (limit * .data$amr),
      # identify near lower/upper process limits
      nlpl = .data$mean - (limitclose * .data$amr),
      nupl = .data$mean + (limitclose * .data$amr),

      # Identify any points which are outside the upper or lower process limits
      outsideLimits = (.data$y > .data$upl | .data$y < .data$lpl),
      # Identify whether a point is above or below the mean
      relativeToMean = sign(.data$y - .data$mean),

      # Identify if a point is between the near process limits and process limits
      closeToLimits = !.data$outsideLimits & (.data$y < .data$nlpl | .data$y > .data$nupl)
    ) %>%
    # clean up by removing columns that no longer serve a purpose and ungrouping data
    select(-.data$nlpl, -.data$nupl, -.data$amr, -.data$rebase) %>%
    ungroup()
}
