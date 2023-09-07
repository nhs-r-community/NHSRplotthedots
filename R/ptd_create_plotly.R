#' Create plotly
#'
#' Creates a plotly object using the parameters passed in.
#'
#' @inheritParams ptd_create_ggplot
#' @param icons_size The size of the icons, defined in terms of font size.
#'  Defaults to 0.15.
#' @returns A plotly object
#' @export
ptd_create_plotly <- function(
    x,
    point_size = 4,
    percentage_y_axis = FALSE,
    main_title,
    x_axis_label,
    y_axis_label,
    fixed_x_axis_multiple = TRUE,
    fixed_y_axis_multiple = TRUE,
    x_axis_date_format = "%d/%m/%y",
    x_axis_breaks = NULL,
    y_axis_breaks = NULL,
    icons_size = 0.15,
    icons_position = c("top right", "bottom right", "bottom left", "top left", "none"),
    colours = ptd_spc_colours(),
    theme_override = NULL,
    break_lines = c("both", "limits", "process", "none"),
    ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    warning(
      "Unknown arguments provided by plot: ",
      paste(names(dots), collapse = ", "),
      ".\nCheck for common spelling mistakes in arguments."
    )
  }
  ggplot <- ptd_create_ggplot(
    x,
    point_size = point_size,
    percentage_y_axis = percentage_y_axis,
    main_title = main_title,
    x_axis_label = x_axis_label,
    y_axis_label = y_axis_label,
    fixed_x_axis_multiple = fixed_x_axis_multiple,
    fixed_y_axis_multiple = fixed_y_axis_multiple,
    x_axis_date_format = x_axis_date_format,
    x_axis_breaks = x_axis_breaks,
    y_axis_breaks = y_axis_breaks,
    icons_size = icons_size,
    icons_position = "none",
    colours = ptd_spc_colours(),
    theme_override = theme_override,
    break_lines = break_lines
  )

  icons_position <- match.arg(icons_position)

  options <- attr(x, "options")

  if (!is.null(options$facet_field)) {
    if (icons_position != "none") {
      warning("Facetted plots do not support showing the icons, setting `icons_position` to \"none\"")
    }
    icons_position <- "none"
  }

  plot <- plotly::ggplotly(ggplot)

  annotations <- if (any(ggplot$data$short_group_warning)) {
    list(
      x = 1,
      y = -0.3, # position of text adjust as needed
      text = paste(
        "Some trial limits created by groups of fewer than 12 points exist.",
        "These will become more reliable as more data is added.",
        sep = "\n"
      ),
      showarrow = FALSE,
      xref = "paper",
      yref = "paper",
      xanchor = "auto",
      yanchor = "auto",
      xshift = 0,
      yshift = 0,
      font = list(size = 12, color = "red")
    )
  }

  images <- if (icons_position != "none") {
    icons <- vapply(ptd_get_icons(x)$icon, read_svg_as_b64, character(1))

    size_x <- icons_size * 0.6
    size_y <- icons_size

    position_y <- ifelse(grepl("top", icons_position), 1 - size_y / 2, 0.1 + size_y / 2)
    position_x <- ifelse(grepl("right", icons_position), 1 - 2 * size_x, 0)

    lapply(
      seq_along(icons),
      \(i) {
        list(
          source = icons[[i]],
          xref = "paper",
          yref = "paper",
          x = position_x + (i - 1) * size_x,
          y = position_y,
          sizex = size_x,
          sizey = size_y,
          sizing = "stretch",
          opacity = 1,
          layer = "above"
        )
      }
    )
  }

  plotly::layout(
    plot,
    # put legend in center of x-axis
    legend = list(
      orientation = "h", # show entries horizontally
      xanchor = "center", # use center of legend as anchor
      x = 0.5,
      y = -0.6
    ),
    annotations = annotations,
    images = images
  )
}

read_svg_as_b64 <- function(filename) {
  img <- readBin(filename, "raw", file.size(filename))

  paste0(
    "data:image/svg+xml;base64,",
    base64enc::base64encode(img)
  )
}
