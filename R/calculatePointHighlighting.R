#' Calculate point highlighting (internal function)
#'
#' Performs calculations to identify which data points should be highlighted in the final plot
#'   based on points outside process limits, trends, shifts, etc.
#'
#' @param df A data frame containing the information to be plotted.
#' @param improvementDirection An integer signifying whether improvement is represented by increasing or decreasing values
#' @return The calculated data frame
#'
#' @noRd
#'

calculatePointHighlighting <- function(df, improvementDirection){

  # Begin plot the dots logical tests
  df <- df %>%
    mutate(sevenPointTrend = case_when( # Identify if a point is the 7th in a run above or below the mean
      (relativeToMean == lag(relativeToMean,1) & f == lag(f,1))
      & (relativeToMean == lag(relativeToMean,2) & f == lag(f,2))
      & (relativeToMean == lag(relativeToMean,3) & f == lag(f,3))
      & (relativeToMean == lag(relativeToMean,4) & f == lag(f,4))
      & (relativeToMean == lag(relativeToMean,5) & f == lag(f,5))
      & (relativeToMean == lag(relativeToMean,6) & f == lag(f,6))
      ~ 1
      ,TRUE ~ 0
    )
    ) %>%
    mutate(
      partOfSevenPointTrend = case_when( # Identify if any of the six points following the current point are the 7th in a run above or below the mean (i.e. part of that run)
        sevenPointTrend == 1
        | (lead(sevenPointTrend,1) == 1 & lead(f,1) == f)
        | (lead(sevenPointTrend,2) == 1 & lead(f,2) == f)
        | (lead(sevenPointTrend,3) == 1 & lead(f,3) == f)
        | (lead(sevenPointTrend,4) == 1 & lead(f,4) == f)
        | (lead(sevenPointTrend,5) == 1 & lead(f,5) == f)
        | (lead(sevenPointTrend,6) == 1 & lead(f,6) == f)
        ~ 1
        ,TRUE ~ 0
      )
      ,sixPointGrowth = case_when( # Identify if a point is is the 6th in an increasing or decreaseing trend
        (.data$y > lag(.data$y,1) & f == lag(f,1))
        & (lag(.data$y,1) > lag(.data$y,2) & lag(f,1) == lag(f,2))
        & (lag(.data$y,2) > lag(.data$y,3) & lag(f,2) == lag(f,3))
        & (lag(.data$y,3) > lag(.data$y,4) & lag(f,3) == lag(f,4))
        & (lag(.data$y,4) > lag(.data$y,5) & lag(f,4) == lag(f,5))
        & (lag(.data$y,5) > lag(.data$y,6) & lag(f,5) == lag(f,6))
        ~ 1
        ,(.data$y < lag(.data$y,1) & f == lag(f,1))
        & (lag(.data$y,1) < lag(.data$y,2) & lag(f,1) == lag(f,2))
        & (lag(.data$y,2) < lag(.data$y,3) & lag(f,2) == lag(f,3))
        & (lag(.data$y,3) < lag(.data$y,4) & lag(f,3) == lag(f,4))
        & (lag(.data$y,4) < lag(.data$y,5) & lag(f,4) == lag(f,5))
        & (lag(.data$y,5) < lag(.data$y,6) & lag(f,5) == lag(f,6))
        ~ -1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      partOfSixPointGrowth = case_when( # Identify if a point belongs to a 6 point increasing or decreasing trend
        abs(sixPointGrowth) == 1
        | (abs(lead(sixPointGrowth,1)) == 1 & f == lead(f,1))
        | (abs(lead(sixPointGrowth,2)) == 1 & f == lead(f,2))
        | (abs(lead(sixPointGrowth,3)) == 1 & f == lead(f,3))
        | (abs(lead(sixPointGrowth,4)) == 1 & f == lead(f,4))
        | (abs(lead(sixPointGrowth,5)) == 1 & f == lead(f,5))
        ~ 1
        ,TRUE ~ 0
      )
      ,twoInThree = case_when( # Identify if two out of three points in a set are between the process limits and near process limits
        (abs(closeToLimits) + abs(lag(closeToLimits,1,default = 0)) + abs(lag(closeToLimits,2,default = 0)) >= 2)
        & f == lag(f,1)
        & f == lag(f,2)
        ~ 1
        ,(abs(closeToLimits) + abs(lag(closeToLimits,1,default = 0)) + abs(lead(closeToLimits,1,default = 0)) >= 2)
        & f == lag(f,1)
        & f == lead(f,1)
        ~ 1
        ,(abs(closeToLimits) + abs(lead(closeToLimits,1,default = 0)) + abs(lead(closeToLimits,2,default = 0)) >= 2)
        & f == lead(f,1)
        & f == lead(f,2)
        ~ 1
        ,TRUE ~ 0
      )
      ,partOfTwoInThree = case_when( # Identify if a point belongs to a 2 in 3 set
        twoInThree == 1 & abs(closeToLimits) == 1 ~ 1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      specialCauseFlag = case_when( # Identify a special cause variation for any of the four rules
        abs(outsideLimits) == 1
        | abs(partOfSevenPointTrend) == 1
        | abs(partOfSixPointGrowth) == 1
        | partOfTwoInThree == 1
        ~ 1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      specialCauseConcern = case_when( # Identify a special cause variation against the improvement direction
        specialCauseFlag == 1
        & relativeToMean == (improvementDirection * -1)
        ~ .data$y
      )
      ,specialCauseImprovement = case_when( # Identify a special cause variation towards the improvement direction
        specialCauseFlag == 1
        & relativeToMean == improvementDirection
        ~ .data$y
      )
    )

  return(df)

}
