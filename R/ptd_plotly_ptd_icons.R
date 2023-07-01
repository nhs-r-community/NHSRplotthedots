ptd_plotly_ptd_icons <- function(spc_data = NULL) {
  # function to transform the data: this takes the raw ptd spc data and returns two rows per facet:
  #  - one row for the variation icon
  #  - one row for the assurance icon (if applicable)

  options <- attr(spc_data, "options")
  improvement_direction <- options$improvement_direction # Exclude Linting

  variation_icon_file <- function(.x) {
    icon <- case_when(
      .x == "common_cause" ~ "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/variation/common_cause.svg",
      .x == "special_cause_neutral_high" ~ "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/variation/neutral_high.svg",
      .x == "special_cause_neutral_low" ~ "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/variation/neutral_low.svg",
      .x == "special_cause_concern" ~ paste0(if (improvement_direction == "increase") "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/variation/concern_low.svg" else "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/variation/concern_high.svg"),
      .x == "special_cause_improvement" ~ paste0(if (improvement_direction == "increase") "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/variation/improvement_high.svg" else "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/variation/improvement_low.svg")
    )
    # system.file("icons", "variation", icon, package = "NHSRplotthedots")
  }

  assurance_icon_file <- function(.x) {
    icon <- case_when(
      .x == "consistent_fail" ~ "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/assurance/fail.svg",
      .x == "consistent_pass" ~ "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/assurance/pass.svg",
      .x == "inconsistent" ~ "https://raw.githubusercontent.com/nhs-r-community/NHSRplotthedots/bcf5f8cdb9fc74f3c2b30fb489973fce1fa58e71/inst/icons/assurance/inconsistent.svg"
    )
    # system.file("icons", "assurance", icon, package = "NHSRplotthedots")
  }

  variation <- spc_data %>%
    group_by(.data$f) %>%
    filter(.data$x == max(.data$x)) %>%
    ungroup() %>%
    transmute(
      .data$f,
      type = "variation",
      icon = variation_icon_file(.data$point_type)
    )

  if (is.null(options$target)) {
    return(variation)
  }

  assurance <- spc_data %>%
    ptd_calculate_assurance_type() %>%
    filter(!is.na(.data$assurance_type)) %>%
    transmute(
      .data$f,
      type = "assurance",
      icon = assurance_icon_file(.data$assurance_type)
    )

  icons <- bind_rows(
    variation,
    assurance
  )

  return(icons)
}
