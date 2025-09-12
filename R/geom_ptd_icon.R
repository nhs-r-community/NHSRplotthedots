geom_ptd_icon_draw_panel <- function(
  self,
  data,
  panel_params,
  coord,
  icons_size = 8,
  icons_position = c("top right", "bottom right", "bottom left", "top left")
) {
  icons_position <- match.arg(icons_position)
  # match the icons_position to x,y coordinates. either {0, 1}, but shift in by
  # 0.01 so icons don't clip
  icons_position_x <- abs(as.numeric(grepl("right$", icons_position)) - 0.01)
  icons_position_y <- abs(as.numeric(grepl("^top", icons_position)) - 0.01)

  # figure out how to justify the icons viewport, this should be two strings
  # like c("right", "top")
  just <- rev(strsplit(icons_position, " ")[[1]])

  # icons_size defines the font size, radius needs to be smaller than that
  radius <- icons_size / 16

  # use the coord transformation for the colours, but then set the x, y
  # coordinates manually (inside the viewport)
  d <- coord$transform(data, panel_params) %>%
    dplyr::mutate(
      x = ifelse(.data$type == "variation", 3.5 * radius, radius),
      y = radius / 2
    )

  sz <- grid::unit(2 * radius, "cm")

  # create the viewport for the icons
  v <- grid::viewport(
    x = grid::unit(icons_position_x, "npc"),
    y = grid::unit(icons_position_y, "npc"),
    width = 2.5 * sz,
    height = sz,
    just = just,
    gp = grid::gpar(col = "black")
  )

  # create the icons
  icons <- apply(d, 1, function(x) {
    grid::rasterGrob(
      rsvg::rsvg_nativeraster(x[["icon"]], 100),
      x = grid::unit(x[["x"]], "cm"),
      y = grid::unit(x[["y"]], "cm"),
      height = sz,
      width = sz,
      just = c("center", "center"),
      vp = v
    )
  })

  # finally insert the icons into a gList
  do.call(grid::gList, c(icons))
}

#' GeomPTDIcon
#'
#' The Geom for the PTD icons. See `geom_ptd_icon()`.
#'
#' @export
GeomPTDIcon <- ggplot2::ggproto( # Exclude Linting
  "GeomPTDIcon",
  ggplot2::Geom,
  required_aes = c("type", "icon"),
  default_aes = ggplot2::aes(),
  extra_params = c("na.rm", "icons_size", "icons_position"),
  draw_key = ggplot2::draw_key_point,
  draw_panel = geom_ptd_icon_draw_panel
)

#' PTD Icons
#'
#' Inserts the Making Data Count icons for variation/assurance to a plot
#'
#' @param data the dataframe to plot. Must be of type ptd_spc_df
#'
#' @param icons_size the size of the icons
#' @param icons_position the position of the icons in the plot
#' @param ... currently unused
#'
#' @export
geom_ptd_icon <- function(
  data = NULL,
  icons_size = 8L,
  icons_position = c("top right", "bottom right", "bottom left", "top left"),
  ...
) {
  icons_position <- match.arg(icons_position)

  # sets up the layer: this is a little unusual for ggplot as we fix the
  # mapping, data argument etc. As this geom is not exported it's not intended
  # to be used in any other way
  ggplot2::layer(
    geom = GeomPTDIcon,
    mapping = aes(type = .data$type, icon = .data$icon),
    data = if (is.null(data)) ptd_get_icons else ptd_get_icons(data),
    stat = "identity",
    position = "identity",
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(icons_size = icons_size, icons_position = icons_position, ...)
  )
}

# function to transform the data: this takes the raw ptd spc data and returns
# two rows per facet:
#  - one row for the variation icon
#  - one row for the assurance icon (if applicable)
ptd_get_icons <- function(.x) {
  stopifnot(
    "Can only be used with objects reated with `ptd_spc()`" = inherits(.x, "ptd_spc_df")
  )

  options <- attr(.x, "options")
  improvement_direction <- options$improvement_direction # Exclude Linting

  variation_icon_file <- function(.x) {
    icon <- dplyr::case_match(
      .x,
      "common_cause" ~ "common_cause.svg",
      "special_cause_neutral_high" ~ "neutral_high.svg",
      "special_cause_neutral_low" ~ "neutral_low.svg",
      "special_cause_concern" ~ paste0(
        "concern_",
        if (improvement_direction == "increase") "low" else "high",
        ".svg"
      ),
      "special_cause_improvement" ~ paste0(
        "improvement_",
        if (improvement_direction == "increase") "high" else "low",
        ".svg"
      )
    )
    system.file("icons", "variation", icon, package = "NHSRplotthedots")
  }

  assurance_icon_file <- function(.x) {
    icon <- dplyr::case_match(
      .x,
      "consistent_fail" ~ "fail.svg",
      "consistent_pass" ~ "pass.svg",
      "inconsistent" ~ "inconsistent.svg"
    )
    system.file("icons", "assurance", icon, package = "NHSRplotthedots")
  }

  variation <- .x %>%
    dplyr::group_by(.data$f) %>%
    dplyr::filter(.data$x == max(.data$x)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      .data$f,
      type = "variation",
      icon = variation_icon_file(.data$point_type)
    )

  if (is.null(options$target)) {
    return(variation)
  }

  assurance <- .x %>%
    ptd_calculate_assurance_type() %>%
    dplyr::filter(!is.na(.data$assurance_type)) %>%
    dplyr::transmute(
      .data$f,
      type = "assurance",
      icon = assurance_icon_file(.data$assurance_type)
    )

  dplyr::bind_rows(
    variation,
    assurance
  )
}
