GeomPTDIcon <- ggproto( # Exclude Linting
  "GeomPTDIcon",
  Geom,
  required_aes = c("type", "colour", "text"),
  default_aes = aes(),
  draw_key = draw_key_point,
  draw_panel = function(data, panel_params, coord) {
    radius <- 0.25

    d <- coord$transform(data, panel_params) %>%
      mutate(x = ifelse(.data$type == "variation", radius, 3.5 * radius),
             y = radius / 2)

    v <- grid::viewport(
      x = grid::unit(1, "npc"), y = grid::unit(1, "npc"),
      width = grid::unit(5 * radius, "cm"), height = grid::unit(2 * radius, "cm"),
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

  layer(
    geom = GeomPTDIcon,
    mapping = aes(type = .data$type, colour = .data$colour, text = .data$text),
    data = data_transformer,
    stat = "identity",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(...)
  )
}
