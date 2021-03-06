
#' This is an example function
#'
#' @description The is to show you a little of what you can put in various sections for the package.
#' The function contains nothing important, but takes two input vectors, makes a data.frame then plots
#' it with `ggplot2`.  I chose this to show you how the 'import' comment works.  Use `roxygen2` package
#' to build the documentation and it will add this to both the description and the namespace.
#'
#' The tags below are transcribed, by `roxygen2` into the correct slots for the help metadata.
#
#' @param input_a A vector of values
#' @param input_b Another vector of values
#' @param colour A colour for the dots in the plot.  Has a default value of 'blue'.
#'
#' @details ggplot2 returns a list but also plots it by default.
#'
#' @returns  a ggplot2 object
#'
#' @export
#'
#' @import ggplot2
#' @import scales
#'
#' @examples
#' a <- c(10,15,17,18,22)
#' b <- c(150, 220, 160, 190, 210)
#' my_function(a, b)
#'
#' my_function(a, b, "red")
#'
my_function <- function(input_a, input_b, colour="blue") {

  mydat <- (data.frame(x=input_a, y=input_b))

  myplot <-
    ggplot(mydat, aes(x=x, y=y)) +
    geom_point(alpha=0.5, col=colour)+
    geom_line(alpha=0.5, col=colour)+
    theme_minimal()


  return(myplot)
}
