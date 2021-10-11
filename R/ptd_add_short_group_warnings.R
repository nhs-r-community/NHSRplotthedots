#' Add Short Group Warnings (internal function)
#'
#' Adds a column to the spc data frame warning of any groups of points that are
#' shorter than the warning_threshold, and which as a result do not follow best-
#' practice for the number of points on which to base SPC calculations.
#' Value is FALSE for any group (facet and rebase combined) that
#' is >= warning_threshold.
#' Value is TRUE for any group (facet and rebase combined) that
#' is < warning_threshold.
#'
#' @param .data A data frame containing the information to be plotted.
#' @param warning_threshold An integer signifying the number of points that will
#' trigger a warning.
#' @return The original data frame with added column
#'
#' @noRd
#'
ptd_add_short_group_warnings <- function(.data, warning_threshold = 12){
  .data %>%
    group_by(across(c(.data$f, .data$rebase_group))) %>%
    mutate(short_group_warning = n() < warning_threshold, .after = .data$rebase_group) %>%
    ungroup()
}
