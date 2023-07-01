ptd_plotly_ptd_icons <- function(spc_data = NULL) {
  # function to transform the data: this takes the raw ptd spc data and returns two rows per facet:
  #  - one row for the variation icon
  #  - one row for the assurance icon (if applicable)

  options <- attr(spc_data, "options")
  improvement_direction <- options$improvement_direction # Exclude Linting

  variation_icon_file <- function(.x) {
    icon <- dplyr::case_match(
      .x,
      "common_cause" ~ "common_cause.svg",
      "special_cause_neutral_high" ~ "neutral_high.svg",
      "special_cause_neutral_low" ~ "neutral_low.svg",
      "special_cause_concern" ~ ifelse(
        improvement_direction == "increase",
        "concern_low.svg",
        "concern_high.svg"
      ),
      "special_cause_improvement" ~ ifelse(
        improvement_direction == "increase",
        "improvement_high.svg",
        "improvement_low.svg"
      )
    )

    paste(
      "https://raw.githubusercontent.com",
      "nhs-r-community",
      "NHSRplotthedots",
      "bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71",
      "inst",
      "icons",
      "variation",
      icon,
      sep = "/"
    )

  }

  assurance_icon_file <- function(.x) {
    icon <- dplyr::case_match(
      .x,
      "consistent_fail" ~ "fail.svg",
      "consistent_pass" ~ "pass.svg",
      "inconsistent" ~ "inconsistent.svg"
    )

    paste(
      "https://raw.githubusercontent.com",
      "nhs-r-community",
      "NHSRplotthedots",
      "bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71",
      "inst",
      "icons",
      "assurance",
      icon,
      sep = "/"
    )
  }

  variation <- spc_data %>%
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

  assurance <- spc_data %>%
    ptd_calculate_assurance_type() %>%
    tidyr::drop_na(.data$assurance_type) %>%
    dplyr::transmute(
      .data$f,
      type = "assurance",
      icon = assurance_icon_file(.data$assurance_type)
    )

  icons <- dplyr::bind_rows(
    variation,
    assurance
  )

  return(icons)
}
