#' SPC Standard Calculations (internal function)
#'
#' Returns a data frame containing SPC fields which are common to all methodologies, including 'plot the dots'
#'
#' This function is designed to produce consistent SPC charts
#' across Information Department reporting, according to the 'plot the dots'
#' logic produced by NHSI. The function can return either a plot or data frame.
#'
#'
#' @param data.frame A data frame containing a value field, a date field,
#' and a category field (if for faceting). There should be no gaps in the time series
#' for each category.
#' @param valueField Specify the field name which contains the value data, to be plotted on y axis.
#' @param dateField Specify the field name which contains the date data, to be plotted on x axis.
#' @param facetField Optional: Specify field name which contains a grouping/faceting variable. SPC logic will be applied to each group separately, with outputs combined. Currently accepts 1 variable only.
#' @param options Optional: A list object containing additional control and formatting properties. Preferably created using the spcOptions function.
#'
#' @noRd


#' @import dplyr
#' @importFrom rlang .data

spcStandard <- function(data.frame, valueField, dateField, facetField = NULL, options = NULL){

  # Identify a rebase, trajectory and target fields, if provided in SPC options object
  rebaseField <- options$rebase[[1]]
  trajectoryField <- options$trajectory[[1]]
  targetField <- options$target[[1]]

  # Declare improvement direction as integer
  if(!(is.null(options$improvementDirection))){
    if(options$improvementDirection == "increase" || options$improvementDirection == 1){
      improvementDirection <- 1
    } else if(options$improvementDirection == "decrease" || options$improvementDirection == -1){
      improvementDirection <- -1
    }
  } else {
    improvementDirection <- 1
  }

  #set trajectory field
  if(!(is.null(trajectoryField))){
    data.frame$trajectory <- data.frame[[trajectoryField]]
  } else {
    data.frame$trajectory <- rep(as.numeric(NA),nrow(data.frame))
  }

  # Set target field or create pseudo
  if(!(is.null(targetField))){
    data.frame$target <- data.frame[[targetField]]
  } else {
    data.frame$target <- rep(as.numeric(NA),nrow(data.frame))
  }

  # Set facet/grouping or create pseudo
  if(facetField == "pseudo_facet_col_name"){ # If no facet field specified, bind a pseudo-facet field for grouping/joining purposes
    f <- data.frame("pseudo_facet_col_name" = rep("no facet",nrow(data.frame)))
    data.frame <- cbind(data.frame,f)
  }

  # Set rebase field or create pseudo
  if(!(is.null(rebaseField))){
    data.frame$rebase <- data.frame[[rebaseField]]
  } else {
    data.frame$rebase <- rep(0,nrow(data.frame)) # If no rebase field supplied, create an empty rebase field (all NAs)
  }

  # Check validity of rebase field supplied - should be 1s and 0s only -
  # QUESTION: should this go into the validateParameters function? Or not as it's validating data?
  dferrors <- data.frame$rebase[data.frame$rebase != 1 & data.frame$rebase != 0]
  if(length(dferrors) > 0) stop("spc: options$rebase argument must define a field containing only 0 or 1 values.")

  ## Constants ----
  l <- nrow(data.frame)
  limit <- 2.66
  limitclose <- 2*(limit/3)

  # Restructure starting data frame
  data.frame <- data.frame %>%
    select(
      y = all_of(valueField)
      ,x = all_of(dateField)
      ,f = all_of(facetField)
      ,rebase = .data$rebase
      ,trajectory = .data$trajectory
      ,target = .data$target) %>%
    group_by(f) %>% # Group data frame by facet
    arrange(f,.data$x) %>% # Order data frame by facet, and x axis variable
    mutate(n = row_number()) %>% # Add an index to each facet group
    ungroup() %>% # Ungroup for tidiness
    mutate(
      movingrange = case_when( # Add moving range, as used for basis of sigma in 'plot the dots' logic
        n > 1 ~ abs(.data$y - lag(.data$y,1))
      )
    )

  # Identify facets/indices which have been flagged for rebasing
  rebaseTable <- data.frame %>%
    filter(.data$rebase == 1) %>%
    select(.data$f, .data$x)

  # Identify the earliest x axis point for each facet - start of rebase group 1
  rebaseTable2 <- data.frame %>%
    group_by(.data$f) %>%
    summarise(x = min(.data$x))

  # Identify the latest x axis point for each facet - end of rebase group n
  rebaseTable3 <- data.frame %>%
    group_by(.data$f) %>%
    summarise(x = max(.data$x))

  # Create a data frame of rebase intervals, with first and last x axis points and all intervening x axis points which will trigger a new rebase period
  rebaseTable4 <- rbind(rebaseTable, rebaseTable2, rebaseTable3) %>%
    arrange(.data$f,.data$x) %>% # order by facet and x axis
    group_by(.data$f) %>% # group by facet
    mutate(rn = row_number() # add a row number which will create an index for each rebase interval within the group
           ,start = lag(.data$x,1) # identify the starting x axis coordinate for each index, using lag function
    ) %>%
    ungroup() %>% # ungroup for tidiness
    filter(.data$rn != 1) %>% # remove the first rebase row from each facet, as this will have an end date but no start date
    select(.data$f, .data$start, end = .data$x) %>% # limit data frame to each facet, and it's rebase start/end points
    group_by(.data$f) %>% # regroup by facet
    mutate(rn = row_number()) %>% # recreate the row number index for each rebase interval within the group, now that the 1st row has been excluded
    arrange(.data$f,desc(.data$start)) %>% # reorder the data frame by facet and descending date order - to allow identification of last period in each facet
    mutate(rn2 = row_number()) %>% # add second row number to identify last rebase group in each facet
    ungroup()

  # Join data frame to rebase groupings, and filter to x axis between grouping start (inclusive) and end (exclusive)
  # - note that this will omit the last row in each facet because it belongs to the previous rebase group
  df2 <- data.frame %>%
    left_join(rebaseTable4, by = c("f" = "f")) %>%
    filter(.data$x >= .data$start, .data$x < .data$end) %>%
    mutate(movingrange = case_when(.data$x != .data$start ~ as.numeric(movingrange),TRUE ~ as.numeric(NA))) # set the first moving range value in each rebase group to NA, as this is not included in the new mean calculations

  # Identify the last row in each facet, to add to the previous rebase group
  df3 <- data.frame %>%
    left_join(rebaseTable4, by = c("f" = "f")) %>%
    filter(.data$rn2 == 1, .data$x == .data$end)

  # Bind together last two data frames, so that all data points are present with a link to the appropriate rebase group
  data.frame <- rbind(df2, df3) %>%
    arrange(f,.data$x) %>%
    select(
      .data$y
      ,.data$x
      ,f
      ,.data$rebase
      ,n
      ,.data$target
      ,.data$trajectory
      ,.data$movingrange
      ,rebaseGroup = .data$rn)

  # Identify the mean and moving range average within each facet and rebase group
  df_avg <- data.frame %>%
    group_by(f,.data$rebaseGroup) %>%
    summarise(mean = mean(.data$y,na.rm = TRUE),movingrangeaverage = mean(.data$movingrange,na.rm = TRUE))

  # Join data frame to moving range average and mean data, then perform standard logical tests
  data.frame <- data.frame %>%
    left_join(df_avg, by = c("f" = "f","rebaseGroup" = "rebaseGroup")) %>%
    mutate(
      lpl = mean - (limit * .data$movingrangeaverage) # identify lower process limits
      ,upl = mean + (limit * .data$movingrangeaverage) # identify upper process limits
      ,nlpl = mean - (limitclose * .data$movingrangeaverage) # identify near lower process limits
      ,nupl = mean + (limitclose * .data$movingrangeaverage) # identify near upper process limits
    ) %>%
    mutate(
      outsideLimits = case_when( # Identify any points which are outside the upper or lower process limits
        .data$y > upl | .data$y < lpl ~ 1
        ,TRUE ~ 0
      )
      ,relativeToMean = case_when( # Identify whether a point is above or below the mean
        .data$y < mean ~ -1
        ,.data$y > mean ~ 1
        ,TRUE ~ 0
      )
    ) %>%
    mutate(
      closeToLimits = case_when( # Identify if a point is between the near process limits and process limits
        .data$y > nupl & .data$y <= upl ~ 1
        ,.data$y < nlpl & .data$y >= lpl ~ 1
        ,TRUE ~ 0
      )
    )

  return(data.frame)
}
