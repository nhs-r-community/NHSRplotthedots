#' Calculate point highlighting (internal function)
#'
#' Performs calculations to identify which data points should be highlighted in the final plot
#'   based on points outside process limits, trends, shifts, etc.
#'
#' @param .data A data frame containing the information to be plotted.
#' @param improvement_direction An integer signifying whether improvement is represented by increasing or decreasing
#'     values
#' @return The calculated data frame
#'
#' @noRd
#'

ptd_calculate_point_type <- function(.data, improvement_direction) {
  # Begin plot the dots logical tests
  .data %>%
    group_by(.data$f, .data$rebase_group) %>%
    mutate(
      special_cause_flag = ptd_special_cause_flag(
        .data$y,
        .data$relative_to_mean,
        .data$close_to_limits,
        .data$outside_limits
      ),
      point_type = case_when(
        !special_cause_flag ~ "common_cause",
        improvement_direction == 0 ~ "special_cause_neutral",
        relative_to_mean == improvement_direction ~ "special_cause_improvement",
        TRUE ~ "special_cause_concern"
      )
    ) %>%
    ungroup()
}

ptd_seven_point_one_side_mean <- function(v) {
  # pad the vector with 6 zero's at the beginning
  vp <- c(rep(0, 6), v)
  vapply(seq_along(v) + 6, function(i) {
    all(vp[[i]] == vp[i - 1:6]) & vp[[i]] != 0
  }, numeric(1))
}

ptd_part_of_seven_trend <- function(v) {
  # pad the vector with 6 zero's at the beginning
  vp <- c(v, rep(0, 6))
  # either, this value is already part of 7, or one of the following 6 points is
  vapply(seq_along(v), function(i) {
    any(vp[i + 0:6] == 1)
  }, numeric(1))
}

ptd_seven_point_trend <- function(y) {
  # edge case: length(v) < 7
  if (length(y) < 7) {
    return(numeric(length(y)))
  }
  # the first 6 points will be 0
  c(
    rep(0, 6),
    vapply(seq_along(y)[-(1:6)], function(i) { # Exclude Linting
      d <- sign(diff(y[i - 0:6])) * -1
      if (all(d == 1)) {
        return(1)
      }
      if (all(d == -1)) {
        return(-1)
      }
      0
    }, numeric(1))
  )
}

ptd_two_in_three <- function(v, rtm) {
  if (length(v) == 0) {
    return(numeric())
  }
  # pad the vectors with two 0 at start, two 0 at end
  vp <- c(0, 0, v, 0, 0)
  rtmp <- c(0, 0, rtm, 0, 0) # relative to mean

  vapply(seq_along(v), function(i) {
    ((sum(vp[i + 0:2]) >= 2) & (abs(sum(rtmp[i + 0:2])) == 3)) ||
    ((sum(vp[i + 1:3]) >= 2) & (abs(sum(rtmp[i + 1:3])) == 3)) ||
    ((sum(vp[i + 2:4]) >= 2) & (abs(sum(rtmp[i + 2:4])) == 3))
  }, numeric(1))
}

ptd_part_of_two_in_three <- function(v, x) {
  as.numeric(v == 1 & abs(x) == 1)
}

ptd_special_cause_flag <- function(y, relative_to_mean, close_to_limits, outside_limits) {
  part_seven_point_one_side_mean <- ptd_part_of_seven_trend(ptd_seven_point_one_side_mean(relative_to_mean))
  part_seven_point_trend <- ptd_part_of_seven_trend(ptd_seven_point_trend(y))
  part_two_in_three <- ptd_part_of_two_in_three(ptd_two_in_three(close_to_limits, relative_to_mean), close_to_limits)

  as.numeric(
    abs(outside_limits) == 1 |
      abs(part_seven_point_one_side_mean) == 1 |
      abs(part_seven_point_trend) == 1 |
      part_two_in_three == 1
  )
}
