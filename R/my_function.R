
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

# Show the contour only
# ggplot(dat, aes(x=x, y=y) ) +
#  geom_density_2d()
 
# Show the area only
# ggplot(dat, aes(x=x, y=y) ) +
#  stat_density_2d(aes(fill = ..level..), geom = "polygon")
 
# Area + contour
# ggplot(dat, aes(x=x, y=y) ) +
#  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")
 
# Using raster
# ggplot(dat, aes(x=x, y=y) ) +
#  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
#  scale_x_continuous(expand = c(0, 0)) +
#  scale_y_continuous(expand = c(0, 0)) +
#  theme(
#    legend.position='none'
  )



ggplot(data, aes())+  
  geom_density(position = "stack", size = 0.8, show.legend = T, col = "gray25")+
  scale_x_continuous(limits = c(0,110), breaks = seq(0,110,15))+
  scale_fill_brewer(palette = "Set1")+
  labs(title = "", subtitle = "", y = "Density",
       x = "", fill = "", caption = "")+
  theme_fivethirtyeight()+
  theme(legend.position="bottom", legend.direction="horizontal", axis.text = element_text(size = 14), axis.title = element_text(size = 15), 
        legend.text = element_text(size = 14), axis.line = element_line(size = 0.4, colour = "grey10"), plot.caption = element_text(color = "gray20", face = "italic"),
        plot.background = element_rect(fill = "#d3dae6"), legend.background = element_rect(fill = "#d3dae6"))
