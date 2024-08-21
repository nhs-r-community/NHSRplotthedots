#' Generate hexagonal logo for the package
#'
#' @param subplot subplot (main image/plot)
#' @param seed numeric value for reproducibility
#' @param n_points numeric value with the number of points to generate
#' @param fill_palette vector of colours to fill the points
#' @param fill_palette_breaks numeric vector with the breaking points (to change
#'     the colours of the points)
#' @param line_size numeric value with the line size/thickness
#' @param line_colour string with the colour for the line
#' @param point_size numeric value with the size of the points
#' @param point_colour string with the colour for the contour of the points
#' @param point_shape numeric value with the shape of the points
#' @param point_stroke numeric value with the size of the stroke for the points
#' @param package string with the package name
#' @param main_colour string with the main colour (text, url and border)
#' @param background_colour string with the background colour
#' @param p_x x position for the package name
#' @param p_y y position for the package name
#' @param p_size font size for package name
#' @param p_family font family for package name
#' @param p_fontface font face for package name
#' @param s_x x position for subplot
#' @param s_y y position for subplot
#' @param s_height height for subplot
#' @param s_width width for subplot
#' @param asp aspect ratio, only works if subplot is an image file
#' @param url package repository URL (or other relevant URL)
#' @param u_angle angle for URL
#' @param u_x x position for URL
#' @param u_y y position for URL
#' @param u_size text size for URL
#' @param dpi plot resolution
#' @param out_filename filename to save sticker
#'
#' @returns A sticker object (`gg` class)
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # Simple plot with 10 points and 5 colours
#' hex_logo()
#'
#' # Multiple combination of parameters
#' plots <- tibble::tribble(
#'   ~n_points, ~point_stroke, ~fill_palette,
#'   10, 0, c("#ED6571", "#F69489", "#FFCF86", "#91CBD7", "#4E9BB9"),
#'   10, 0, c("#ED6571", rep("#FFCF86", 3), "#4E9BB9"),
#'   10, .5, c("#ED6571", "#F69489", "#FFCF86", "#91CBD7", "#4E9BB9"),
#'   10, .5, c("#ED6571", rep("#FFCF86", 3), "#4E9BB9"),
#'   15, 0, c("#ED6571", "#F69489", "#FFCF86", "#91CBD7", "#4E9BB9"),
#'   15, 0, c("#ED6571", rep("#FFCF86", 3), "#4E9BB9"),
#'   15, .5, c("#ED6571", "#F69489", "#FFCF86", "#91CBD7", "#4E9BB9"),
#'   15, .5, c("#ED6571", rep("#FFCF86", 3), "#4E9BB9"),
#'   20, 0, c("#ED6571", "#F69489", "#FFCF86", "#91CBD7", "#4E9BB9"),
#'   20, 0, c("#ED6571", rep("#FFCF86", 3), "#4E9BB9"),
#'   20, .5, c("#ED6571", "#F69489", "#FFCF86", "#91CBD7", "#4E9BB9"),
#'   20, .5, c("#ED6571", rep("#FFCF86", 3), "#4E9BB9")
#' ) %>%
#'   dplyr::rowwise() %>%
#'   dplyr::mutate(
#'     out_filename = stringr::str_c(
#'       "inst/images/logo_w_",
#'       n_points,
#'       "pt_",
#'       length(unique(fill_palette)),
#'       "col_",
#'       point_stroke,
#'       "st.png"
#'     )
#'   ) %>%
#'   purrr::pmap(hex_logo)
#' }
hex_logo <- function(
    subplot = NULL,
    seed = 123,
    n_points = 10,
    fill_palette = c("#ed6571", "#f69489", "#ffcf86", "#91cbd7", "#4e9bb9"),
    fill_palette_breaks = seq(.2, .8, length.out = length(fill_palette) - 1) * n_points,
    line_size = 0.5,
    line_colour = "#000000",
    point_size = 2.9,
    point_colour = line_colour,
    point_shape = 21,
    point_stroke = line_size,
    package = "NHSRplotthedots",
    main_colour = "#000000",
    background_colour = "#ffffff",
    p_x = 1.0,
    p_y = 1.62,
    p_size = 16,
    p_family = "Aller_Rg",
    p_fontface = "plain",
    s_x = 1.0,
    s_y = 0.98,
    s_height = 1.17,
    s_width = 1.5,
    asp = 0.9,
    url = "https://github.com/nhs-r-community/NHSRplotthedots",
    u_angle = 30,
    u_x = 1.02,
    u_y = 0.08,
    u_size = 4.85,
    dpi = 500,
    out_filename = "inst/images/logo.png") {

  # check if the subplot was provided
  if (missing(subplot)) {
    subplot <- simple_pointplot(
      seed = seed,
      n_points = n_points,
      fill_palette = fill_palette,
      fill_palette_breaks = fill_palette_breaks,
      line_size = line_size,
      line_colour = line_colour,
      point_size = point_size,
      point_colour = point_colour,
      point_shape = point_shape,
      point_stroke = point_stroke
    )
  }

  # generate and save sticker
  hexSticker::sticker(
    subplot = subplot,
    package = package,
    h_color = main_colour,
    h_fill = background_colour,
    dpi = dpi,
    s_x = s_x,
    s_y = s_y,
    s_height = s_height,
    s_width = s_width,
    asp = asp,
    p_x = p_x,
    p_y = p_y,
    p_size = p_size,
    p_color = main_colour,
    p_family = p_family,
    url = url,
    u_angle = u_angle,
    u_color = main_colour,
    u_family = p_family,
    u_size = u_size,
    u_x = u_x,
    u_y = u_y,
    filename = out_filename
  )
}

#' Create a simple point plot
#'
#' @param seed numeric value for reproducibility
#' @param n_points numeric value with the number of points to generate
#' @param fill_palette vector of colours to fill the points
#' @param fill_palette_breaks numeric vector with the breaking points (to change
#'  the colours of the points)
#' @param line_size numeric value with the line size/thickness
#' @param line_colour string with the colour for the line
#' @param point_size numeric value with the size of the points
#' @param point_colour string with the colour for the contour of the points
#' @param point_shape numeric value with the shape of the points
#' @param point_stroke numeric value with the size of the stroke for the points
#'
#' @returns A ggplot2 object
simple_pointplot <- function(
    seed = 123,
    n_points = 10,
    fill_palette = c("#ed6571", "#f69489", "#ffcf86", "#91cbd7", "#4e9bb9"),
    fill_palette_breaks = seq(.2, .8, length.out = length(fill_palette) - 1) * n_points,
    line_size = 0.5,
    line_colour = "#000000",
    point_size = 2.9,
    point_colour = line_colour,
    point_shape = 21,
    point_stroke = line_size) {

  # local bindings
  x <- y <- fill <- NULL
  set.seed(seed) # set seed for reproducibility

  # generate sample data
  x <- seq_len(n_points)
  y <- sample(x, n_points)
  demo_data_tb <- data.frame(
    x = x,
    y = y,
    fill = cut(y, breaks = c(-Inf, fill_palette_breaks, Inf))
  )
  # create the subplot: points and line
  subplot <- demo_data_tb %>%
    ggplot2::ggplot(ggplot2::aes(x, y)) +
    ggplot2::geom_line(size = line_size, colour = line_colour) +
    ggplot2::geom_point(
      aes(fill = fill),
      size = point_size,
      colour = point_colour,
      shape = point_shape,
      stroke = point_stroke
    ) +
    ggplot2::scale_fill_manual(values = fill_palette) +
    ggplot2::theme_void() + # remove all the features, except the main plot
    ggplot2::theme(legend.position = "none") + # remove the legend
    hexSticker::theme_transparent() # make background of the plot transparent
  subplot
}

# Community designed logo ----
# Based on the design provided by an user on Twitter, this is an "approximated"
# version created with R
fill_palette <- c(
  orange = "#eb7f3c", # orange
  nhs_dark_grey = "#425563", # NHS Dark grey
  nhs_dark_blue = "#005eb8" # NHS blue
)
fill_palette_breaks <- seq(.2, .8, length.out = length(fill_palette) - 1) * 6
line_size <- .75
line_colour <- fill_palette["nhs_dark_grey"]
point_size <- 3.5
point_shape <- 21
point_stroke <- 0
font_family <- "Arial"
font_family_bold <- "Arial"
font_family_bold_italic <- "Arial"
y <- c(1, 3, 4, 6, 4, 3)
# generate sample data
demo_data_tb <- data.frame(
  x = 1:6,
  y = y,
  fill = cut(y, breaks = c(-Inf, fill_palette_breaks, Inf))
)

# create the subplot: points and line
subplot <- demo_data_tb %>%
  ggplot2::ggplot(aes(x, y)) +
  ggplot2::geom_line(size = line_size, colour = line_colour) +
  ggplot2::geom_point(
    aes(fill = fill),
    size = point_size,
    shape = point_shape,
    stroke = point_stroke
  ) +
  ggplot2::annotate(
    geom = "text",
    x = -0.5,
    y = 4,
    label = "making\ndata\ncount\nin\nthe",
    colour = line_colour,
    size = 13,
    fontface = "bold",
    lineheight = .2,
    hjust = 0
  ) +
  ggplot2::annotate(
    geom = "text",
    x = -0.5,
    y = 2.2,
    label = "NHS",
    colour = fill_palette["nhs_dark_blue"], # NHS Blue
    size = 16,
    fontface = "bold",
    lineheight = .2,
    hjust = 0
  ) +
  ggplot2::scale_fill_manual(values = unname(fill_palette)) +
  ggplot2::theme_void() + # remove all the features, except the main plot
  # remove the legend
  ggplot2::theme(
    legend.position = "none",
    text = ggplot2::element_text(family = font_family)
  ) +
  hexSticker::theme_transparent() # make background of the plot transparent

hex_logo(
  subplot,
  main_colour = line_colour,
  url = "",
  p_family = font_family,
  out_filename = "inst/images/logo.png"
)
