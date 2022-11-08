# Based on the design provided by an user on Twitter, this is an "approximated"
# version created with R
# generate sample data
fill_palette <-
  c("#EB7F3C", # orange
    "#425563", # gray
    "#005EB8") # NHS blue
fill_palette_breaks <-
  seq(.2, .8, length.out = length(fill_palette) - 1) * 6
line_size <- .75
line_colour <- "#425563"
point_size <- 3.5
point_shape <- 21
point_stroke <- 0
font_family <- "Arial"
font_family_bold <- "Arial"
font_family_bold_italic <- "Arial"
y <- c(1, 3, 4, 6, 4, 3)
demo_data_tb <- data.frame(
  x = 1:6,
  y = y,
  fill = cut(y, breaks = c(-Inf, fill_palette_breaks, Inf))
)
`%>%` <- magrittr::`%>%`
# create the subplot: points and line
subplot <- demo_data_tb %>%
  ggplot2::ggplot(ggplot2::aes(x, y)) +
  ggplot2::geom_line(size = line_size,
                     colour = line_colour) +
  ggplot2::geom_point(ggplot2::aes(fill = fill),
                      size = point_size,
                      shape = point_shape,
                      stroke = point_stroke
  ) +
  ggplot2::annotate(geom = "text",
                    x = -0.5,
                    y = 4,
                    label = "making\ndata\ncount\nin\nthe",
                    colour = line_colour,
                    size = 13,
                    fontface = "bold",
                    lineheight = .2,
                    # family = font_family_bold,
                    hjust = 0) +
  ggplot2::annotate(geom = "text",
                    x = -0.5,
                    y = 2.2,
                    label = "NHS",
                    colour = "#005EB8", # NHS Blue
                    size = 16,
                    fontface = "bold",
                    lineheight = .2,
                    # family = font_family_bold,
                    hjust = 0) +
  # ggplot2::annotate(geom = "label",
  #                   x = -0.5,
  #                   y = 2.2,
  #                   label = "NHS",
  #                   size = 16,
  #                   fontface = "bold.italic",
  #                   label.padding = ggplot2::unit(.5, "mm"),
  #                   label.r = ggplot2::unit(0, "lines"), # no round corners
  #                   colour = "#FFFFFF",
  #                   fill = "#005EB8",
  #                   # family = font_family,
  #                   hjust = 0) +
  ggplot2::scale_fill_manual(values = fill_palette) +
  ggplot2::theme_void() + # remove all the features, except the main plot
  ggplot2::theme( # remove the legend
    legend.position = "none",
    text = ggplot2::element_text(family = font_family)
  ) +
  hexSticker::theme_transparent() # make background of the plot transparent
hex_logo(subplot,
         main_colour = line_colour,
         url = "",
         p_family = font_family,
         out_filename = "inst/images/logo_debs_version.png")

