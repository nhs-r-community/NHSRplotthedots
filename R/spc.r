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
  ,options = NULL ## options: target, trajectory, rebase, data as percentages, title, x title, y title, x axis break frequency, pointSize, returnChart, display legend
) {

  #validate all inputs.  Validation problems will generate an error and stop code execution.
  validateParameters(data.frame, valueField, dateField, facetField, options)

  if(is.null(facetField)){ # If no facet field specified, bind a pseudo-facet field for grouping/joining purposes
    facetField <- "pseudo_facet_col_name"
  }

  df <- spcStandard(data.frame, valueField, dateField, facetField, options)


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

  #set output chart
  if(!(is.null(options$outputChart))){ # Check if chart required as output
    if(options$outputChart == TRUE){
      outputChart <- 1
    } else if(options$outputChart == FALSE){
      outputChart <- 0
    }
  } else {
    outputChart <- 1
  }

  #set x axis breaks
  if(!(is.null(options$xAxisBreaks))){
    xaxis <- df$x
    start <- min(xaxis,na.rm = TRUE)
    end <- max(xaxis,na.rm = TRUE)
    xaxislabels <- seq.Date(from = as.Date(start), to = as.Date(end), by = options$xAxisBreaks)
  } else {
    xaxislabels <- df$x
  }

  #set point size
  if(!(is.null(options$pointSize))){
    pointSize <- options$pointSize
  } else {
    pointSize = 2
  }

  #set x axis date format
  if(!(is.null(options$xAxisDateFormat))){
    xAxisDateFormat <- options$xAxisDateFormat
  } else {
    xAxisDateFormat <- "%d/%m/%Y"
  }

  #set main plot title
  if(!(is.null(options$mainTitle))){
    plottitle <- options$mainTitle
  } else {
    plottitle <- "SPC Chart"
  }

  #set x axis label
  if(!(is.null(options$xAxisLabel))){
    xlabel <- options$xAxisLabel
  } else {
    xlabel <- "Date"
  }

  #set y axis label
  if(!(is.null(options$yAxisLabel))){
    ylabel <- options$yAxisLabel
  } else {
    ylabel <- "Value"
  }
  
  #set y axis breaks
  if(!(is.null(options$yAxisBreaks))){
    yAxisBreaks <- options$yAxisBreaks
  } else {
    yAxisBreaks <- NULL
  }

  #set x axis fixed scaling for facet plots
  if(!(is.null(options$fixedXAxisMultiple))){
    scaleXFixed <- options$fixedXAxis
  } else {
    scaleXFixed <- TRUE
  }

  #set y axis fixed scaling for facet plots
  if(!(is.null(options$fixedYAxisMultiple))){
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



  #set percentage y axis
  if(!(is.null(options$percentageYAxis))){ # Check if Y values are percentages
    if(is.numeric(options$percentageYAxis)){
      convertToPercentages <- options$percentageYAxis
    } else if (is.logical(options$percentageYAxis)){
      convertToPercentages <- 0.1 * as.numeric(options$percentageYAxis)
    }
  } else {
    convertToPercentages <- 0
  }


  ## Plot the dots SPC logic ----
  df <- calculatePointHighlighting(df, improvementDirection)

  ## Create ggplot using plot the dots colours OR output data frame ----
  # Create chart if required
  if(outputChart == 1){

    #build a list of plotOptions to pass into the createGgplot() function
    plotOptions <- list(
      pointSize = pointSize,
      plottitle = plottitle,
      xlabel = xlabel,
      ylabel = ylabel,
      xaxislabels = xaxislabels,
      xAxisDateFormat = xAxisDateFormat,
      convertToPercentages = convertToPercentages,
      facetScales = facetScales,
      yAxisBreaks = yAxisBreaks
    )

    #make and return the plot
    plot <- createGgplot(df, facetField, plotOptions)
    return(plot)

  } else if(outputChart == 0){

    #or return the calculated dataframe
    return(df)

  }
}


