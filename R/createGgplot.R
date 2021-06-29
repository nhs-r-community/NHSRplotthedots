#' Create ggplot2 (internal function)
#'
#' Creates a ggplot2 object using the parameters passed in.
#'
#' @param df A data frame containing the information to be plotted.
#' @param facetField A character vector naming the column in the data frame containing the facet names
#' @param plotOptions A list containing the options needed for plot customisations
#' @return The ggplot2 object
#'
#' @noRd

createGgplot <- function(df, facetField, plotOptions){

  # Colour Palette for ggplot
  .darkgrey = "#7B7D7D"
  .orange = "#fab428"
  .skyblue = "#289de0"
  .purple = "#361475"
  .red = "#de1b1b"

  if(!(is.null(plotOptions$yAxisBreaks))){ 
      yaxis <- c(df$y,df$upl,df$lpl)
      start <- floor(min(yaxis,na.rm = TRUE)/plotOptions$yAxisBreaks) * plotOptions$yAxisBreaks
      end <- max(yaxis,na.rm = TRUE)
      yaxislabels <- seq(from = start, to = end, by = plotOptions$yAxisBreaks)
  }

  plot <- ggplot(df,aes(x=.data$x,y=.data$y)) +
    theme_minimal() +
    geom_line(aes(y=.data$upl),linetype = "dashed",size=plotOptions$pointSize/2.666666,color=.darkgrey) +
    geom_line(aes(y=.data$lpl),linetype = "dashed",size=plotOptions$pointSize/2.666666,color=.darkgrey) +
    geom_line(aes(y=.data$target),linetype = "dashed",size=plotOptions$pointSize/2.666666,color=.purple) +
    geom_line(aes(y=.data$trajectory),linetype = "dashed",size=plotOptions$pointSize/2.666666,color=.red) +
    geom_line(aes(y=mean)) +
    geom_line(color=.darkgrey,size=plotOptions$pointSize/2.666666) +
    geom_point(color=.darkgrey,size=plotOptions$pointSize)

  # Apply facet wrap if a facet field is present
  if(facetField != "pseudo_facet_col_name"){ 
    plot <- plot +
      facet_wrap(vars(.data$f), scales = plotOptions$facetScales)
  }

  plot <- plot +
    geom_point(aes(x=.data$x,y=.data$specialCauseImprovement),color=.skyblue,size=plotOptions$pointSize) +
    geom_point(aes(x=.data$x,y=.data$specialCauseConcern),color=.orange,size=plotOptions$pointSize) +
    ggtitle(label = plotOptions$plottitle) +
    xlab(label = plotOptions$xlabel) +
    ylab(label = plotOptions$ylabel) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_date(breaks=plotOptions$xaxislabels, labels = format(plotOptions$xaxislabels, format = plotOptions$xAxisDateFormat)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  #if the plot is not faceted (ie it's the default facet column name)
  if(facetField == "pseudo_facet_col_name"){
    if(plotOptions$convertToPercentages == FALSE){
      if(!(is.null(plotOptions$yAxisBreaks))){
        plot <- plot +
          scale_y_continuous(breaks = yaxislabels, labels = yaxislabels)
      }
    } else if(plotOptions$convertToPercentages != 0) {
      percentLimit <- max(df$upl,na.rm = TRUE)

      interval <- if(!(is.null(plotOptions$yAxisBreaks))){plotOptions$yAxisBreaks} else {plotOptions$convertToPercentages}

      plot <- plot +
        scale_y_continuous(labels = scales::percent,breaks = seq(from = 0, to = percentLimit, by = interval))
    }
  } else{
    #else if the plot is faceted
    if(plotOptions$convertToPercentages != 0) {
      percentLimit <- max(df$upl,na.rm = TRUE)

      plot <- plot +
        scale_y_continuous(labels = scales::percent)
    }
  }

  return(plot)

}
