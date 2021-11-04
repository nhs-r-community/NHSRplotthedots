#' Create ggplot2
#'
#' Creates a ggplot2 object using the parameters passed in.
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
#' @param icons_size The size of the icons, defined in terms of font size. Defaults to 8.
#' @param icons_position Where to show the icons, either "top right" (default), "bottom right", "bottom left",
#'     "top left", or "none".
#' @param colours Specify the colours to use in the plot, use the [ptd_spc_colours()] function to change defaults.
#' @param theme_override Specify a list containing ggplot theme elements which can be used to override the default
#'     appearance of the plot.
#' @param break_lines whether to break lines when a rebase happens. Defaults to "both", but can break just "limits"
#'     lines, "process" lines, or "none".
#' @param ... currently ignored
#' @return The ggplot2 object
#' @export
ptd_create_ggplot <- function(x,
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
                              icons_size = 8L,
                              icons_position = c("top right", "bottom right", "bottom left", "top left", "none"),
                              colours = ptd_spc_colours(),
                              theme_override = NULL,
                              break_lines = c("both", "limits", "process", "none"),
                              ...) {
  dots <- list(...)
  if (length(dots) > 0) {
    warning("Unknown arguments provided by plot: ",
            paste(names(dots), collapse = ", "),
            "\nCheck for common spelling mistakes in arguments.")
  }

  assertthat::assert_that(
    inherits(x, "ptd_spc_df"),
    msg = "x argument must be an 'ptd_spc_df' object, created by ptd_spc()."
  )

  # argument needs to be called x for s3 plot method, but rename it to .data so it's more obvious through the rest of
  # the method
  .data <- x
  options <- attr(.data, "options")

  if (missing(main_title)) {
    main_title <- paste0(
      "SPC Chart of ",
      ptd_capitalise(options$value_field),
      ", starting ",
      format(min(.data[["x"]], na.rm = TRUE), format = "%d/%m/%Y")
    )
  }

  if (missing(x_axis_label)) {
    x_axis_label <- ptd_capitalise(options[["date_field"]])
  }
  if (missing(y_axis_label)) {
    y_axis_label <- ptd_capitalise(options[["value_field"]])
  }

  icons_position <- match.arg(icons_position)

  break_lines <- match.arg(break_lines)
  ptd_validate_plot_options(
    point_size,
    percentage_y_axis,
    main_title,
    x_axis_label,
    y_axis_label,
    fixed_x_axis_multiple,
    fixed_y_axis_multiple,
    x_axis_date_format,
    x_axis_breaks,
    y_axis_breaks,
    icons_size,
    icons_position,
    colours,
    theme_override,
    break_lines
  )

  colours_subset <- if (options[["improvement_direction"]] == "neutral") {
    colours[c("common_cause", "special_cause_neutral")]
  } else {
    colours[c("common_cause", "special_cause_improvement", "special_cause_concern")]
  }

  # apply a short groups warning caption if needed
  caption <- if (any(.data$short_group_warning)) {
    paste0(
      "Some trial limits created by groups of fewer than 12 points exist. \n",
      "These will become more reliable as more data is added."
    )
  }

  line_size <- point_size / 3

  break_limits <- break_lines %in% c("both", "limits")
  break_process <- break_lines %in% c("both", "process")

  plot <- ggplot(.data, aes(x = .data$x, y = .data$y)) +
    geom_line(aes(y = .data$upl, group = if (break_limits) .data$rebase_group else 0),
      linetype = "dashed", size = line_size, colour = colours$upl
    ) +
    geom_line(aes(y = .data$lpl, group = if (break_limits) .data$rebase_group else 0),
      linetype = "dashed", size = line_size, colour = colours$lpl
    ) +
    geom_line(aes(y = .data$target),
      linetype = "dashed", size = line_size, colour = colours$target, na.rm = TRUE
    ) +
    geom_line(aes(y = .data$trajectory),
      linetype = "dashed", size = line_size, colour = colours$trajectory, na.rm = TRUE
    ) +
    geom_line(aes(y = mean, group = if (break_limits) .data$rebase_group else 0),
      linetype = "solid", colour = colours$mean_line
    ) +
    geom_line(aes(group = if (break_process) .data$rebase_group else 0),
      linetype = "solid", size = line_size, colour = colours$value_line
    ) +
    geom_point(aes(colour = .data$point_type), size = point_size) +
    scale_colour_manual(
      values = colours_subset,
      labels = ptd_title_case
    ) +
    labs(
      title = main_title,
      x = x_axis_label,
      y = y_axis_label,
      caption = caption,
      group = NULL
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(color = "grey", size = 1), # border around whole plot
      plot.margin = unit(c(5, 5, 5, 5), "mm"), # 5mm of white space around plot edge
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_line(color = "grey70"), # gridline colour
      panel.grid.major.x = element_blank(), # remove major x gridlines
      panel.grid.minor.x = element_blank(), # remove minor x gridlines
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    theme_override

  plot <- plot + if (is.null(x_axis_breaks)) {
    scale_x_datetime(
      breaks = sort(unique(.data$x)),
      date_labels = x_axis_date_format
    )
  } else {
    scale_x_datetime(
      date_breaks = x_axis_breaks,
      date_labels = x_axis_date_format
    )
  }

  # Apply facet wrap if a facet field is present
  if (!is.null(options$facet_field)) {
    # For multiple facet chart, derived fixed/free scales value from x and y axis properties
    faces_scales <- if (fixed_x_axis_multiple) {
      ifelse(fixed_y_axis_multiple, "fixed", "free_y")
    } else {
      ifelse(fixed_y_axis_multiple, "free_x", "free")
    }

    plot <- plot +
      facet_wrap(vars(.data$f), scales = faces_scales)
  }

  if (percentage_y_axis %||% FALSE) {
    plot <- plot +
      scale_y_continuous(labels = scales::label_percent(y_axis_breaks))
  } else if (!is.null(y_axis_breaks)) {
    yaxis <- c(.data[["y"]], .data[["upl"]], .data[["lpl"]], .data[["target"]])
    start <- floor(min(yaxis, na.rm = TRUE) / y_axis_breaks) * y_axis_breaks
    end <- max(yaxis, na.rm = TRUE)

    y_axis_labels <- seq(from = start, to = end, by = y_axis_breaks)

    plot <- plot +
      scale_y_continuous(breaks = y_axis_labels, labels = y_axis_labels)
  }

  if (icons_position != "none") {
    plot <- plot +
      geom_ptd_icon(icons_size = icons_size, icons_position = icons_position)
  }

  plot
}

#' @rdname ptd_create_ggplot
#' @export
#' @noRd
plot.ptd_spc_df <- function(x,
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
                            icons_size = 8L,
                            icons_position = c("top right", "bottom right", "bottom left", "top left", "none"),
                            colours = ptd_spc_colours(),
                            theme_override = NULL,
                            break_lines = c("both", "limits", "process", "none"),
                            ...) {
  break_lines <- match.arg(break_lines)

  ptd_create_ggplot(
    x,
    point_size,
    percentage_y_axis,
    main_title,
    x_axis_label,
    y_axis_label,
    fixed_x_axis_multiple,
    fixed_y_axis_multiple,
    x_axis_date_format,
    x_axis_breaks,
    y_axis_breaks,
    icons_size,
    icons_position,
    colours,
    theme_override,
    break_lines,
    ...
  )
}
