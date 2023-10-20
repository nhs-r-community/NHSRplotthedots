#' Create plotly
#'
#' Creates a plotly object using the parameters passed in.
#'
#' @param x an object created by [ptd_spc()]
#' @param point_size Specify the plotting point size for the ggplot output. Default is 2.5.
#' @param percentage_y_axis Specify whether the y axis values are percentages. Accepted values are TRUE for percentage y
#'     axis, FALSE for integer y axis. Defaults to FALSE.
#' @param main_title Specify a character string value for the ggplot title.
#' @param x_axis_label Specify a character string value for the x axis title.
#' @param y_axis_label Specify a character string value for the y axis title.
#' @param fixed_x_axis_multiple Specify whether, if producing a faceted spc, x axis should be fixed for all facet plots.
#'     Accepted values are TRUE for fixed x axes or FALSE for individual x axes.
#' @param fixed_y_axis_multiple Specify whether, if producing a faceted spc, y axis should be fixed for all facet plots.
#'     Accepted values are TRUE for fixed y axes or FALSE for individual y axes.
#' @param x_axis_date_format Specify how dates on the x axis should be displayed. Format should be provided
#'     as a character string using 'd m Y' etc syntax.
#' @param x_axis_breaks Specify an interval value for breaks on the x axis. Value should be a character string
#'     expressing interval length and type, e.g. "3 months", "7 days".
#' @param y_axis_breaks Specify an interval value for breaks on the y axis. Value should be a numeric vector of length
#'     1, either an integer for integer scales or a decimal value for percentage scales. This option is ignored if
#'     faceting is in use.
#' @param icons_size The size of the icons, defined in terms of font size. Defaults to 0.15.
#' @param icons_position Where to show the icons, either "top right" (default), "bottom right", "bottom left",
#'     "top left", or "none".
#' @param colours Specify the colours to use in the plot, use the [ptd_spc_colours()] function to change defaults.
#' @param theme_override Specify a list containing ggplot theme elements which can be used to override the default
#'     appearance of the plot.
#' @param break_lines whether to break lines when a rebase happens. Defaults to "both", but can break just "limits"
#'     lines, "process" lines, or "none".
#' @param ... currently ignored
#' @return The plotly object
#' @export
ptd_create_plotly <- function(x,
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

  plot <- ggplot |>
    plotly::ggplotly() |>
    ptd_plotly_fix_tooltips()

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
    hovermode = "x unified",
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

ptd_plotly_fix_tooltips <- function(plot) {
  # "fix" the tooltips. this could do with being improved upon, but for now it's better than nothing
  # each "layer" has it's own text which contains the x value, a new line, then the y value (and sometimes more stuff
  # on futher new lines)
  # what we want to do is remove the x value from all but the first item, then only keep the second line's value
  plot$x$data[[1]]$text <- plot$x$data[[1]]$text |>
    stringr::str_replace_all(
      "^(.*?)(<br />)(.*?)(?:<br />.*$)",
      "\\1\\2\\2\\3"
    )

  for (i in seq_along(plot$x$data)[-1]) {
    plot$x$data[[i]]$text <- plot$x$data[[i]]$text |>
      stringr::str_remove("^.*?(<br />)") |>
      stringr::str_remove("<br />.*$")
  }

  plot
}
