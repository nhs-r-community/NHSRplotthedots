#' Calculate point highlighting (internal function)
#'
#' Performs calculations to identify which data points should be highlighted in the final plot
#'   based on points outside process limits, trends, shifts, etc.
#'
#' @param .data A data frame containing the information to be plotted.
#' @param improvementDirection An integer signifying whether improvement is represented by increasing or decreasing
#'     values
#' @return The calculated data frame
#'
#' @noRd
#'

calculatePointHighlighting <- function(.data, improvementDirection) {
  # Begin plot the dots logical tests
  .data %>%
    group_by(.data$f) %>%
    mutate(
      sevenPointOneSideOfMean = sevenPointOneSideOfMean(.data$relativeToMean),
      partOfSevenPointOneSideOfMean = partOfSevenTrend(.data$sevenPointOneSideOfMean),
      sevenPointTrend = sevenPointTrend(.data$y),
      partOfSevenPointTrend = partOfSevenTrend(.data$sevenPointTrend),
      twoInThree = twoInThree(.data$closeToLimits),
      partOfTwoInThree = partOfTwoInThree(.data$twoInThree, .data$closeToLimits),
      specialCauseFlag = specialCauseFlag(
        .data$outsideLimits,
        .data$partOfSevenPointOneSideOfMean,
        .data$partOfSevenPointTrend,
        .data$partOfTwoInThree
      ),
      specialCauseConcern = specialCauseConcern(
        .data$outsideLimits,
        .data$partOfSevenPointOneSideOfMean,
        .data$partOfTwoInThree,
        .data$partOfSevenPointTrend,
        .data$y,
        .data$relativeToMean,
        improvementDirection
      ),
      specialCauseImprovement = specialCauseImprovement(
        .data$outsideLimits,
        .data$partOfSevenPointOneSideOfMean,
        .data$partOfTwoInThree,
        .data$partOfSevenPointTrend,
        .data$y,
        .data$relativeToMean,
        improvementDirection
      )
    ) %>%
    ungroup()
}

sevenPointOneSideOfMean <- function(v) {
  # pad the vector with 6 zero's at the beginning
  vp <- c(rep(0, 6), v)
  vapply(seq_along(v) + 6, function(i) {
    all(vp[[i]] == vp[i - 1:6]) & vp[[i]] != 0
  }, numeric(1))
}

partOfSevenTrend <- function(v) {
  # pad the vector with 6 zero's at the beginning
  vp <- c(v, rep(0, 6))
  # either, this value is already part of 7, or one of the following 6 points is
  vapply(seq_along(v), function(i) {
    any(vp[i + 0:6] == 1)
  }, numeric(1))
}

sevenPointTrend <- function(y) {
  # edge case: length(v) < 7
  if (length(y) < 7) return(numeric(length(y)))
  # the first 6 points will be 0
  c(rep(0, 6),
    vapply(seq_along(y)[- (1:6)], function(i) {
      d <- sign(diff(y[i - 0:6])) * -1
      if (all(d ==  1)) return(1)
      if (all(d == -1)) return(-1)
      0
    }, numeric(1)))
}

twoInThree <- function(v) {
  if (length(v) == 0) return(numeric())
  # pad the vector with two 0 at start, two 0 at end
  vp <- c(0, 0, v, 0, 0)
  vapply(seq_along(v), function(i) {
    sum(vp[i + 0:2]) >= 2 || sum(vp[i + 1:3]) >= 2 || sum(vp[i + 2:4]) >= 2
  }, numeric(1))
}

partOfTwoInThree <- function(v, x) {
  as.numeric(v == 1 & abs(x) == 1)
}

specialCauseFlag <- function(outsideLimits,
                             partOfSevenPointOneSideOfMean,
                             partOfSevenPointTrend,
                             partOfTwoInThree) {
  as.numeric(
    abs(outsideLimits) == 1 |
    abs(partOfSevenPointOneSideOfMean) == 1 |
    abs(partOfSevenPointTrend) == 1 |
    partOfTwoInThree == 1
  )
}

specialCauseImprovement <- function(outsideLimits,
                                    partOfSevenPointOneSideOfMean,
                                    partOfTwoInThree,
                                    partOfSevenPointTrend,
                                    y,
                                    relativeToMean,
                                    improvementDirection) {
  case_when(
    outsideLimits == 1 & relativeToMean == improvementDirection ~ y,
    partOfSevenPointOneSideOfMean == 1 & relativeToMean == improvementDirection ~ y,
    partOfTwoInThree == 1 & relativeToMean == improvementDirection ~ y,
    abs(partOfSevenPointTrend) == 1 & improvementDirection == partOfSevenPointTrend ~ y
  )
}

specialCauseConcern <- function(outsideLimits,
                                partOfSevenPointOneSideOfMean,
                                partOfTwoInThree,
                                partOfSevenPointTrend,
                                y,
                                relativeToMean,
                                improvementDirection) {
  specialCauseImprovement(outsideLimits,
                          partOfSevenPointOneSideOfMean,
                          partOfTwoInThree,
                          partOfSevenPointTrend,
                          y,
                          relativeToMean,
                          improvementDirection * - 1)
}
