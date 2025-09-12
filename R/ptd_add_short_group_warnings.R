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
#'  trigger a warning.
#' @returns The original data frame with added column
#'
#' @details To override the `warning_threshold` you can set the option
#'  `ptd_spc.warning_threshold`, e.g. `options(ptd_spc.warning_threshold = 10)`.
#'  The default, if the option is not set, is 12.
#' @noRd
ptd_add_short_group_warnings <- function(
  .data,
  warning_threshold = getOption("ptd_spc.warning_threshold", 13)
) {
  .data <- .data %>%
    dplyr::group_by(
      dplyr::across(
        c("f", "rebase_group")
      )
    ) %>%
    dplyr::mutate(
      short_group_warning = dplyr::n() < warning_threshold,
      .after = "rebase_group"
    ) %>%
    dplyr::ungroup()

  if (any(.data$short_group_warning)) {
    warning(
      "Some groups have 'n < 12' observations. ",
      "These have trial limits, which will be revised with each additional observation ",
      "until 'n = fix_after_n_points' has been reached."
    )
  }

  .data
}
