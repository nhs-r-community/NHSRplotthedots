#' Calculate point highlighting (internal function)
#'
#' Performs calculations to identify which data points should be highlighted in the final plot
#'   based on points outside process limits, trends, shifts, etc.
#'
#' @param df A data frame containing the information to be plotted.
#' @param improvementDirection An integer signifying whether improvement is represented by increasing or decreasing
#'     values
#' @return The calculated data frame
#'
#' @noRd
#'

calculatePointHighlighting <- function(df, improvementDirection) {
  # Begin plot the dots logical tests
  df %>%
    mutate(
      sevenPointOneSideOfMean = as.numeric(
        # Identify if a point is the 7th in a run above or below the mean
        (.data$relativeToMean == lag(.data$relativeToMean, 1) & .data$f == lag(.data$f, 1)) &
          (.data$relativeToMean == lag(.data$relativeToMean, 2) & .data$f == lag(.data$f, 2)) &
          (.data$relativeToMean == lag(.data$relativeToMean, 3) & .data$f == lag(.data$f, 3)) &
          (.data$relativeToMean == lag(.data$relativeToMean, 4) & .data$f == lag(.data$f, 4)) &
          (.data$relativeToMean == lag(.data$relativeToMean, 5) & .data$f == lag(.data$f, 5)) &
          (.data$relativeToMean == lag(.data$relativeToMean, 6) & .data$f == lag(.data$f, 6))
      ),
      partOfSevenPointOneSideOfMean = as.numeric(
        # Identify if any of the six points following the current point are the 7th in a run above or below the mean
        # (i.e. part of that run)
        .data$sevenPointOneSideOfMean == 1 |
          (lead(.data$sevenPointOneSideOfMean, 1) == 1 & lead(.data$f, 1) == .data$f) |
          (lead(.data$sevenPointOneSideOfMean, 2) == 1 & lead(.data$f, 2) == .data$f) |
          (lead(.data$sevenPointOneSideOfMean, 3) == 1 & lead(.data$f, 3) == .data$f) |
          (lead(.data$sevenPointOneSideOfMean, 4) == 1 & lead(.data$f, 4) == .data$f) |
          (lead(.data$sevenPointOneSideOfMean, 5) == 1 & lead(.data$f, 5) == .data$f) |
          (lead(.data$sevenPointOneSideOfMean, 6) == 1 & lead(.data$f, 6) == .data$f)
      ),
      sevenPointTrend = case_when(
        # Identify if a point is the 6th in an increasing or decreasing trend
        (.data$y > lag(.data$y, 1) & .data$f == lag(.data$f, 1)) &
          (lag(.data$y, 1) > lag(.data$y, 2) & lag(.data$f, 1) == lag(.data$f, 2)) &
          (lag(.data$y, 2) > lag(.data$y, 3) & lag(.data$f, 2) == lag(.data$f, 3)) &
          (lag(.data$y, 3) > lag(.data$y, 4) & lag(.data$f, 3) == lag(.data$f, 4)) &
          (lag(.data$y, 4) > lag(.data$y, 5) & lag(.data$f, 4) == lag(.data$f, 5)) &
          (lag(.data$y, 5) > lag(.data$y, 6) & lag(.data$f, 5) == lag(.data$f, 6)) ~ 1,
        (.data$y < lag(.data$y, 1) & .data$f == lag(.data$f, 1)) &
          (lag(.data$y, 1) < lag(.data$y, 2) & lag(.data$f, 1) == lag(.data$f, 2)) &
          (lag(.data$y, 2) < lag(.data$y, 3) & lag(.data$f, 2) == lag(.data$f, 3)) &
          (lag(.data$y, 3) < lag(.data$y, 4) & lag(.data$f, 3) == lag(.data$f, 4)) &
          (lag(.data$y, 4) < lag(.data$y, 5) & lag(.data$f, 4) == lag(.data$f, 5)) &
          (lag(.data$y, 5) < lag(.data$y, 6) & lag(.data$f, 5) == lag(.data$f, 6)) ~ -1,
        TRUE ~ 0
      ),
      partOfSevenPointTrend = case_when(
        # Identify if a point belongs to a 7 point increasing or decreasing trend
        # Part of a 7 point ascending trend
        .data$sevenPointTrend == 1 |
          lead(.data$sevenPointTrend, 1) == 1 & .data$f == lead(.data$f, 1) |
          lead(.data$sevenPointTrend, 2) == 1 & .data$f == lead(.data$f, 2) |
          lead(.data$sevenPointTrend, 3) == 1 & .data$f == lead(.data$f, 3) |
          lead(.data$sevenPointTrend, 4) == 1 & .data$f == lead(.data$f, 4) |
          lead(.data$sevenPointTrend, 5) == 1 & .data$f == lead(.data$f, 5) |
          lead(.data$sevenPointTrend, 6) == 1 & .data$f == lead(.data$f, 6) ~ 1,
        # Part of a 7 point descending trend
        .data$sevenPointTrend == -1 |
          lead(.data$sevenPointTrend, 1) == -1 & .data$f == lead(.data$f, 1) |
          lead(.data$sevenPointTrend, 2) == -1 & .data$f == lead(.data$f, 2) |
          lead(.data$sevenPointTrend, 3) == -1 & .data$f == lead(.data$f, 3) |
          lead(.data$sevenPointTrend, 4) == -1 & .data$f == lead(.data$f, 4) |
          lead(.data$sevenPointTrend, 5) == -1 & .data$f == lead(.data$f, 5) |
          lead(.data$sevenPointTrend, 6) == -1 & .data$f == lead(.data$f, 6) ~ -1,
        TRUE ~ 0
      ),
      twoInThree = case_when(
        # Identify if two out of three points in a set are between the process limits and near process limits
        (abs(.data$closeToLimits) +
          abs(lag(.data$closeToLimits, 1, default = 0)) +
          abs(lag(.data$closeToLimits, 2, default = 0)) >= 2
        ) &
          .data$f == lag(.data$f, 1) &
          .data$f == lag(.data$f, 2) ~ 1,
        (abs(.data$closeToLimits) +
          abs(lag(.data$closeToLimits, 1, default = 0)) +
          abs(lead(.data$closeToLimits, 1, default = 0)) >= 2
        ) &
          .data$f == lag(.data$f, 1) &
          .data$f == lead(.data$f, 1) ~ 1,
        (abs(.data$closeToLimits) +
          abs(lead(.data$closeToLimits, 1, default = 0)) +
          abs(lead(.data$closeToLimits, 2, default = 0)) >= 2
        ) &
          .data$f == lead(.data$f, 1) &
          .data$f == lead(.data$f, 2) ~ 1,
        TRUE ~ 0
      ),
      partOfTwoInThree = as.numeric(
        # Identify if a point belongs to a 2 in 3 set
        .data$twoInThree == 1 & abs(.data$closeToLimits) == 1
      ),
      specialCauseFlag = as.numeric(
        # Identify a special cause variation for any of the four rules
        abs(.data$outsideLimits) == 1 |
          abs(.data$partOfSevenPointOneSideOfMean) == 1 |
          abs(.data$partOfSevenPointTrend) == 1 |
          .data$partOfTwoInThree == 1
      ),
      specialCauseConcern = case_when(
        # Identify a special cause variation against the improvement direction
        .data$outsideLimits == 1 & .data$relativeToMean == (improvementDirection * -1) ~ .data$y,
        .data$partOfSevenPointOneSideOfMean == 1 & .data$relativeToMean == (improvementDirection * -1) ~ .data$y,
        .data$partOfTwoInThree == 1 & .data$relativeToMean == (improvementDirection * -1) ~ .data$y,
        .data$partOfSevenPointTrend == 1 & improvementDirection == -1 ~ .data$y,
        .data$partOfSevenPointTrend == -1 & improvementDirection == 1 ~ .data$y,
      ),
      specialCauseImprovement = case_when(
        # Identify a special cause variation towards the improvement direction
        .data$outsideLimits == 1 & .data$relativeToMean == improvementDirection ~ .data$y,
        .data$partOfSevenPointOneSideOfMean == 1 & .data$relativeToMean == improvementDirection ~ .data$y,
        .data$partOfTwoInThree == 1 & .data$relativeToMean == improvementDirection ~ .data$y,
        .data$partOfSevenPointTrend == 1 & improvementDirection == 1 ~ .data$y,
        .data$partOfSevenPointTrend == -1 & improvementDirection == -1 ~ .data$y
      )
    )
}
