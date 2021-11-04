GeomPTDIcon <- ggproto( # Exclude Linting
  "GeomPTDIcon",
  Geom,
  required_aes = c("type", "colour", "text"),
  default_aes = aes(),
  extra_params = c("na.rm", "icons_size", "icons_position"),
  draw_key = draw_key_point,
  draw_panel = function(self, data, panel_params, coord,
                        icons_size = 8,
                        icons_position = c("top right", "bottom right", "bottom left", "top left")) {
    icons_position <- match.arg(icons_position)
    # match the icons_position to x,y coordinates. either {0, 1}, but shift in by 0.01 so icons don't clip
    icons_position_x <- abs(as.numeric(grepl("right$", icons_position)) - 0.01)
    icons_position_y <- abs(as.numeric(grepl("^top", icons_position)) - 0.01)

    # figure out how to justify the icons viewport, this should be two strings like c("right", "top")
    just <- rev(strsplit(icons_position, " ")[[1]])

    # icons_size defines the font size, radius needs to be smaller than that
    radius <- icons_size / 32

    # use the coord transformation for the colours, but then set the x, y coordinates manually (inside the viewport)
    d <- coord$transform(data, panel_params) %>%
      mutate(
        x = ifelse(.data$type == "variation", radius, 3.5 * radius),
        y = radius / 2
      )

    # create the viewport for the icons
    v <- grid::viewport(
      x = grid::unit(icons_position_x, "npc"),
      y = grid::unit(icons_position_y, "npc"),
      width = grid::unit(5 * radius, "cm"),
      height = grid::unit(2 * radius, "cm"),
      just = just,
      gp = grid::gpar(col = "black")
    )

    # create the circles for the icons
    circles <- apply(d, 1, function(x) {
      grid::circleGrob(
        x = grid::unit(x[["x"]], "cm"),
        y = grid::unit(x[["y"]], "cm"),
        r = grid::unit(radius, "cm"),
        gp = grid::gpar(col = "black", fill = x[["colour"]]),
        vp = v
      )
    })

    # insert the text inside the icons
    text <- grid::textGrob(
      d$text, grid::unit(d$x, "cm"), grid::unit(d$y, "cm"),
      gp = grid::gpar(fontsize = icons_size),
      vp = v
    )

    # finally insert the icons into a gList
    do.call(grid::gList, c(circles, list(text)))
  }
)

geom_ptd_icon <- function(icons_size = 8L,
                          icons_position = c("top right", "bottom right", "bottom left", "top left"),
                          ...) {
  icons_position <- match.arg(icons_position)

  # function to transform the data: this takes the raw ptd spc data and returns two rows per facet:
  #  - one row for the variation icon
  #  - one row for the assurance icon (if applicable)
  data_transformer <- function(.x) {
    options <- attr(.x, "options")
    improvement_direction <- options$improvement_direction
    id_text <- c(
      common_cause = "C",
      special_cause_neutral = "N",
      special_cause_concern = if (improvement_direction == "increase") "L" else "H",
      special_cause_improvement = if (improvement_direction == "increase") "H" else "L",
      consistent_fail = "F",
      consistent_pass = "P",
      inconsistent = "?"
    )

    variation <- .x %>%
      group_by(.data$f) %>%
      filter(.data$x == max(.data$x)) %>%
      ungroup() %>%
      transmute(
        .data$f,
        type = "variation",
        colour = .data$point_type,
        text = unname(id_text[.data$point_type])
      )

    assurance <- .x %>%
      ptd_calculate_assurance_type() %>%
      filter(!is.na(.data$assurance_type)) %>%
      transmute(
        .data$f,
        type = "assurance",
        colour = case_when(
          .data$assurance_type == "consistent_pass" ~ "special_cause_improvement",
          .data$assurance_type == "consistent_fail" ~ "special_cause_concern",
          .data$assurance_type == "inconsistent" ~ "common_cause"
        ),
        text = unname(id_text[.data$assurance_type])
      )

    bind_rows(
      variation,
      assurance
    )
  }

  # set's up the layer: this is a little unusual for ggplot as we fix the mapping, data argument etc. As this geom is
  # not-exported it's not intended to be used in any other way
  layer(
    geom = GeomPTDIcon,
    mapping = aes(type = .data$type, colour = .data$colour, text = .data$text),
    data = data_transformer,
    stat = "identity",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(icons_size = icons_size, icons_position = icons_position, ...)
  )
}
