#' SPC Plotting Function
#'
#' `spc` returns a plot object or data table with SPC values using NHSI 'plot the dots' logic.
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
#' Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param dateField Specify the field name which contains the date data, to be plotted on x axis.
#' Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param facetField Optional: Specify field name which contains a grouping/faceting variable. SPC logic will be applied to each group separately, with outputs combined. Currently accepts 1 variable only.
#' Field name can be specified using non-standard evaluation (i.e. no quotation marks).
#' @param options Optional: A list object containing additional control and formatting properties. Preferably created using the spcOptions function.
#'
#' @export spc


#' @import dplyr
#' @import ggplot2
#' @import scales
#' @importFrom rlang .data

spc <- function(
  data.frame
  ,valueField
  ,dateField
  ,facetField = NULL
  ,options = NULL ## options: target, trjaectory, rebase, data as percentages, title, x title, y title, x axis break frequency, pointSize, returnChart, display legend
) {

  ## Data/field preparation ----

  # Quote Expr
  df <- data.frame
  y_name <- enexpr(valueField)
  x_name <- enexpr(dateField)
  facet_name <- enexpr(facetField)

  # Identify a rebase, trajectory and target fields, if provided in SPC options object
  rebaseField <- options$rebase[[1]]
  trajectoryField <- options$trajectory[[1]]
  targetField <- options$target[[1]]

  # Check validity of inputs
  if(!(is.null(options)) && !(is.list(options))){ # If spcOptions supplied, check that options objecti s a list
    stop("Options argument should be a list.")
  }
  if(!(is.data.frame(data.frame))){ # Check that data.frame argument is a data frame
    stop("Data.frame argument is not a data.frame.")
  }
  if(!(is.null(options$yAxisBreaks))){ # Y axis breaks should be integer or decimal
    if(is.numeric(options$yAxisBreaks)){
      yaxis <- c(df$y,df$upl,df$lpl)
      start <- floor(min(yaxis,na.rm = TRUE)/options$yAxisBreaks) * options$yAxisBreaks
      end <- max(yaxis,na.rm = TRUE)
      yaxislabels <- seq(from = start, to = end, by = options$yAxisBreaks)
    } else {
      stop("Y Axis Break option must be numeric.")
    }
  }
  if(!(is.null(options$improvementDirection))){ # Check improvement direction supplied is either increase/decrease or 1/-1
    if(options$improvementDirection == "increase" || options$improvementDirection == 1){
      improvementDirection <- 1
    } else if(options$improvementDirection == "decrease" || options$improvementDirection == -1){
      improvementDirection <- -1
    } else {
      stop("Improvement direction option should be 'increase' or 'decrease'")
    }
  } else {
    improvementDirection <- 1
  }
  if(!(is.null(options$outputChart))){ # Check if chart required as output
    if(options$outputChart == TRUE){
      outputChart <- 1
    } else if(options$outputChart == FALSE){
      outputChart <- 0
    } else {
      stop("outputChart option must be true or false")
    }
  } else {
    outputChart <- 1
  }
  if(!(is.null(options$xAxisBreaks))){ # X axis breaks should be character string showing date seq intervals
    if(is.character(options$xAxisBreaks)){
      xaxis <- df[[x_name]]
      start <- min(xaxis,na.rm = TRUE)
      end <- max(xaxis,na.rm = TRUE)
      xaxislabels <- seq.Date(from = as.Date(start), to = as.Date(end), by = options$xAxisBreaks)
    } else {
      stop("X Axis Break option must be character vector of length 1. E.g. '3 months'.")
    }
  } else {
    xaxislabels <- df[[x_name]]
  }
  if(!(is.null(options$pointSize))){ # Check if custom plot point size provided - must be an integer
    if(is.numeric(options$pointSize)){
      pointSize <- options$pointSize
    } else {
      stop("pointSize option must be an integer")
    }
  } else {
    pointSize = 2
  }
  if(!(is.null(options$xAxisDateFormat))){ # Check if x axis data format supplied, if so must be character format
    if(is.character(options$xAxisDateFormat)){
      xAxisDateFormat <- options$xAxisDateFormat
    } else {
      stop("xAxisDateFormat option must be a character")
    }
  } else {
    xAxisDateFormat <- "%d/%m/%Y"
  }
  if(!(is.null(trajectoryField))){ # Check if trajectory field specified - if so, bind as trajectory field else bind as NAs
    if(!(as.character(trajectoryField) %in% colnames(df))){
      df$trajectory <- rep(as.numeric(NA),nrow(df))
    } else {
      df$trajectory <- df[[trajectoryField]]
    }
  } else {
    df$trajectory <- rep(as.numeric(NA),nrow(df))
  }
  if(!(is.null(targetField))){ # Check if target field specified - if so, bind as trajectory field else bind as NAs
    if(!(as.character(targetField) %in% colnames(df))){
      df$target <- rep(as.numeric(NA),nrow(df))
    } else {
      df$target <- df[[targetField]]
    }
  } else {
    df$target <- rep(as.numeric(NA),nrow(df))
  }
  if(!(is.null(options$mainTitle))){ # Check if custom plot title supplied
    plottitle <- options$mainTitle
  } else {
    plottitle <- "SPC Chart"
  }
  if(!(is.null(options$xAxisLabel))){ # Check if custom x axis title supplied
    xlabel <- options$xAxisLabel
  } else {
    xlabel <- "Date"
  }
  if(!(is.null(options$yAxisLabel))){ # Check if custom y axis title supplied
    ylabel <- options$yAxisLabel
  } else {
    ylabel <- "Value"
  }
  if(!(is.null(options$fixedXAxisMultiple))){ # For multiple facet chart, check whether options list specified fixed x axis
    scaleXFixed <- options$fixedXAxis
  } else {
    scaleXFixed <- TRUE
  }
  if(!(is.null(options$fixedYAxisMultiple))){ # For multiple facet chart, check whether options list specified fixed y axis
    scaleYFixed <- options$fixedYAxis
  } else {
    scaleYFixed <- TRUE
  }
  facetScales <- if(scaleYFixed == TRUE && scaleXFixed == TRUE){ # For multiple facet chart, derived fixed/free scales value from x and y axis properties
    "fixed"
  } else if (scaleYFixed == TRUE && scaleXFixed == FALSE){
    "free_x"
  } else if (scaleYFixed == FALSE && scaleXFixed == TRUE){
    "free_y"
  } else if (scaleYFixed == FALSE && scaleXFixed == FALSE){
    "free"
  }
  if(is.null(facet_name)){ # If no facet field specified, bind a pseudo-facet field for grouping/joining purposes
    facet_name <- "pseudo_facet_col_name"
    f <- data.frame("pseudo_facet_col_name" = rep("no facet",nrow(df)))
    df <- cbind(df,f)
    message("No facet detected - binding pseudo-facet column")
  }

  # Check validity of rebase field supplied - should be 1s and 0s only
  if(!(is.null(rebaseField))){ # If rebase field is not null
    if(!(as.character(rebaseField) %in% colnames(df))){ # Check if rebase field exists in supplied fields
      df$rebase <- rep(as.numeric(NA),nrow(df)) # If no, create an empty rebase field (all NAs)
    } else {
      df$rebase <- df[[rebaseField]] # If yes, create a rebase field using that field
    }
  } else {
    df$rebase <- rep(as.numeric(NA),nrow(df)) # If no rebase field supplied, create an empty rebase field (all NAs)
  }
  if(!(is.null(options$percentageYAxis))){ # Check if Y values are percentages
    if(is.numeric(options$percentageYAxis)){
      convertToPercentages <- options$percentageYAxis
    } else if (is.logical(options$percentageYAxis)){
      convertToPercentages <- 0.1 * as.numeric(options$percentageYAxis)
    } else {
      stop("percentageYAxis option should be TRUE or a decimal value < 1 to indicate axis break frequency.")
    }
  } else {
    convertToPercentages <- 0
  }

  ## Standard SPC Logic ----

  # Key Values
  l <- nrow(data.frame)

  # Constants
  limit <- 2.66
  limitclose <- 2*(limit/3)

  # Restructure starting data frame
  df <- df %>%
    select(
      y = all_of(y_name)
      ,x = all_of(x_name)
      ,f = all_of(facet_name)
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

  # # Find average of moving range for each facet
  # mra <- df %>%
  #   group_by(.data$f) %>%
  #   summarise(movingrangeaverage = mean(.data$movingrange, na.rm = TRUE))
  #
  # # Join moving range average back to primary data frame
  # df <- df %>%
  #   left_join(mra, by = c(f = "f"))

  # Identify facets/indices which have been flagged for rebasing
  rebaseTable <- df %>%
    filter(.data$rebase == 1) %>%
    select(.data$f, .data$x)

  # Identify the earliest x axis point for each facet - start of rebase group 1
  rebaseTable2 <- df %>%
    group_by(.data$f) %>%
    summarise(x = min(.data$x))

  # Identify the latest x axis point for each facet - end of rebase group n
  rebaseTable3 <- df %>%
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
  df2 <- df %>%
    left_join(rebaseTable4, by = c("f" = "f")) %>%
    filter(.data$x >= .data$start, .data$x < .data$end) %>%
    mutate(movingrange = case_when(.data$x != .data$start ~ movingrange,TRUE ~ as.numeric(NA))) # set the first moving range value in each rebase group to NA, as this is not included in the new mean calculations

  # Identify the last row in each facet, to add to the previous rebase group
  df3 <- df %>%
    left_join(rebaseTable4, by = c("f" = "f")) %>%
    filter(.data$rn2 == 1, .data$x == .data$end)

  # Bind together last two data frames, so that all data points are present with a link to the appropriate rebase group
  df <- rbind(df2, df3) %>%
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
  df_avg <- df %>%
    group_by(f,.data$rebaseGroup) %>%
    summarise(mean = mean(.data$y,na.rm = TRUE),movingrangeaverage = mean(.data$movingrange,na.rm = TRUE))

  # Join data frame to moving range average and mean data, then perform standard logical tests
  df <- df %>%
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


  ## Plot the dots SPC logic ----
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

  ## Create ggplot using plot the dots colours OR output data frame ----
  # Colour Palette for ggplot
  .darkgrey = "#7B7D7D"
  .orange = "#fab428"
  .skyblue = "#289de0"
  .purple = "#361475"
  .red = "#de1b1b"

  # Create chart if required
  if(outputChart == 1){
    plot <- ggplot(df,aes(x=.data$x,y=.data$y)) +
      theme_minimal() +
      geom_line(aes(y=.data$upl),linetype = "dashed",size=pointSize/2.666666,color=.darkgrey) +
      geom_line(aes(y=.data$lpl),linetype = "dashed",size=pointSize/2.666666,color=.darkgrey) +
      geom_line(aes(y=.data$target),linetype = "dashed",size=pointSize/2.666666,color=.purple) +
      geom_line(aes(y=.data$trajectory),linetype = "dashed",size=pointSize/2.666666,color=.red) +
      geom_line(aes(y=mean)) +
      geom_line(color=.darkgrey,size=pointSize/2.666666) +
      geom_point(color=.darkgrey,size=pointSize)

    if(facet_name != "pseudo_facet_col_name"){ # Apply facet wrap if a facet field is present
      plot <- plot +
        facet_wrap(vars(f), scales = facetScales)
    }

    plot <- plot +
      geom_point(aes(x=.data$x,y=.data$specialCauseImprovement),color=.skyblue,size=pointSize) +
      geom_point(aes(x=.data$x,y=.data$specialCauseConcern),color=.orange,size=pointSize) +
      ggtitle(label = plottitle) +
      xlab(label = xlabel) +
      ylab(label = ylabel) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_date(breaks=xaxislabels, labels = format(xaxislabels, format = xAxisDateFormat)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))

    if(is.null(facet_name)){
      if(convertToPercentages == FALSE){
        if(!(is.null(options$yAxisBreaks))){
          plot <- plot +
            scale_y_continuous(breaks = yaxislabels, labels = yaxislabels)
        }
      } else if(convertToPercentages != 0) {
        percentLimit <- max(df$upl,na.rm = TRUE)

        interval <- if(!(is.null(options$yAxisBreaks))){options$yAxisBreaks} else {convertToPercentages}

        plot <- plot +
          scale_y_continuous(labels = scales::percent,breaks = seq(from = 0, to = percentLimit, by = interval))
      }
    }

    if(!(is.null(facet_name))){
      if(convertToPercentages != 0) {
        percentLimit <- max(df$upl,na.rm = TRUE)

        plot <- plot +
          scale_y_continuous(labels = scales::percent)
      }
    }

    plot
  } else if(outputChart == 0){
    df
  }
}


