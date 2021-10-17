#' Calculate assurance type (internal function)
#'
#' Performs calculations to identify whether the SPC calculations indicate whether we will consistently pass the
#' target, consistently fail the target, or inconsistently pass/fail the target.
#'
#' @param .data A data frame containing the information to be plotted.
#' @return The calculated data frame
#'
#' @noRd
#'
ptd_calculate_assurance_type <- function(.data) {
  d <- .data %>%
    group_by(.data$f) %>%
    slice_tail(n = 1)

  options <- attr(.data, "options")

  if (is.null(options$target) || options$improvement_direction == "neutral") {
    return(summarise(d, assurance_type = as.character(NA), .groups = "drop"))
  }

  # linting reports this is assigned by not used, so excluding line from linting as it is used
  is_increasing <- options$improvement_direction == "increase" # Exclude Linting

  d %>%
    summarise(assurance_type = case_when(
      target > upl ~ ifelse(is_increasing, "consistent_fail", "consistent_pass"),
      target < lpl ~ ifelse(is_increasing, "consistent_pass", "consistent_fail"),
      TRUE ~ "inconsistent"
    ), .groups = "drop")
}
