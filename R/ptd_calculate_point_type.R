#' Calculate point highlighting (internal function)
#'
#' Performs calculations to identify which data points should be highlighted in
#'  the final plot based on points outside process limits, trends, shifts, etc.
#'
#' @param .data A data frame containing the information to be plotted.
#' @param improvement_direction An integer signifying whether improvement is
#'  represented by increasing or decreasing values.
#' @returns The calculated data frame
#'
#' @noRd
ptd_calculate_point_type <- function(.data, improvement_direction) {
  # Begin plot the dots logical tests
  .data %>%
    dplyr::group_by(.data$f, .data$rebase_group) %>%
    dplyr::mutate(
      special_cause_type = ptd_special_cause_type(
        .data$y,
        .data$relative_to_mean,
        .data$close_to_limits,
        .data$outside_limits
      ),
      special_cause_flag = .data[["special_cause_type"]] != "Common Cause",
      point_type = ptd_point_type(.data[["special_cause_type"]], improvement_direction)
    ) %>%
    dplyr::ungroup()
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
    any(abs(vp[i + 0:6]) == 1)
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

ptd_special_cause_type <- function(y, relative_to_mean, close_to_limits, outside_limits) {
  part_seven_point_trend <- ptd_part_of_seven_trend(ptd_seven_point_trend(y)) # Exclude Linting
  part_two_in_three <- ptd_part_of_two_in_three(ptd_two_in_three(close_to_limits, relative_to_mean), close_to_limits) # Exclude Linting
  part_seven_point_one_side_mean <- ptd_part_of_seven_trend(ptd_seven_point_one_side_mean(relative_to_mean)) # Exclude Linting

  # calculate the sign of difference of points in y
  sdy <- sign(diff(y))
  # if a point is part of a trend either side, use right side
  sdy <- ifelse(
    c(part_seven_point_trend[-1], 0) & part_seven_point_trend,
    c(sdy, utils::tail(sdy, 1)),
    c(0, sdy)
  )

  above_or_below <- ifelse(relative_to_mean > 0, "Above", "Below") # Exclude Linting

  dplyr::case_when(
    outside_limits != 0 ~ ifelse(relative_to_mean == 1, "Above UCL", "Below LCL"),
    part_seven_point_trend != 0 ~ paste0("7 Point Trend (", ifelse(sdy == 1, "In", "De"), "creasing)"),
    part_two_in_three != 0 ~ paste0("2 in 3 ", above_or_below, " CL"),
    part_seven_point_one_side_mean != 0 ~ paste0("7 Points ", above_or_below, " CL"),
    .default = "Common Cause"
  )
}

ptd_point_type <- function(special_cause_type, improvement_direction) {
  v <- dplyr::case_when(
    stringr::str_detect(special_cause_type, "Above|Increasing") ~ 1,
    stringr::str_detect(special_cause_type, "Below|Decreasing") ~ -1,
    .default = 0
  )

  if (improvement_direction == 0) {
    dplyr::case_when(
      v > 0 ~ "special_cause_neutral_high",
      v < 0 ~ "special_cause_neutral_low",
      .default = "common_cause"
    )
  } else {
    # orient based on improvement direction
    v <- v * improvement_direction

    dplyr::case_when(
      v > 0 ~ "special_cause_improvement",
      v < 0 ~ "special_cause_concern",
      .default = "common_cause"
    )
  }
}
