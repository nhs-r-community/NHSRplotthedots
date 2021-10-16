GeomPTDIcon <- ggproto(
  "GeomPTDIcon",
  Geom,
  required_aes = c("type", "colour", "text"),
  default_aes = aes(),
  draw_key = draw_key_point,
  draw_panel = function(data, panel_params, coord) {
    radius <- 0.3

    d <- coord$transform(data, panel_params) %>%
      mutate(x = ifelse(.data$type == "variation", radius, 4 * radius),
             y = radius)

    v <- grid::viewport(
      x = grid::unit(1, "npc"), y = grid::unit(1, "npc"),
      width = grid::unit(6 * radius, "cm"), height = grid::unit(3 * radius, "cm"),
      just = c("right", "top"),
      gp = grid::gpar(col = "black")
    )

    circles <- apply(d, 1, function(x) {
      grid::circleGrob(
        x = grid::unit(x[["x"]], "cm"), y = grid::unit(x[["y"]], "cm"), r = grid::unit(radius, "cm"),
        gp = grid::gpar(col = "black", fill = x[["colour"]]),
        vp = v
      )
    })

    text <- grid::textGrob(
      d$text, grid::unit(d$x, "cm"), grid::unit(d$y, "cm"),
      gp = grid::gpar(fontsize = 8),
      vp = v
    )

    do.call(grid::gList, c(circles, list(text)))
  }
)

geom_ptd_icon <- function(...) {
  data_transformer <- function(.x) {
    bind_rows(
      .x %>%
        group_by(f) %>%
        arrange(x) %>%
        slice_tail(n = 1) %>%
        ungroup() %>%
        transmute(f, type = "variation", colour = point_type),
      .x %>%
        ptd_calculate_assurance_type() %>%
        transmute(f, type = "assurance", colour = assurance_type)
    ) %>%
      filter(!is.na(colour)) %>%
      mutate(
        text = gsub("(.)[a-z]*(_|$)", "\\1", .data$colour),
        colour = case_when(
          .data$type == "variation" ~ .data$colour,
          .data$colour == "consistent_pass" ~ "special_cause_improvement",
          .data$colour == "consistent_fail" ~ "special_cause_concern",
          TRUE ~ "common_cause"
        )
      )
  }

  layer(
    geom = GeomPTDIcon,
    mapping = aes(type = type, colour = colour, text = text),
    data = data_transformer,
    stat = "identity",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(...)
  )
}
