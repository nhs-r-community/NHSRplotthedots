#' SPC Colours
#'
#' Produces a list of colours that controls the geoms in the plot
#'
#' @param common_cause,special_cause_improvement,special_cause_neutral,special_cause_concern the colour of the points
#' @param value_line the colour of the line joining the points
#' @param mean_line the colour of the "mean" (average) line
#' @param lpl,upl the colour the the lower and upper process limit lines
#' @param target the colour of the target line
#' @param trajectory the colour of the trajectory line
#'
#' @returns A list of colours
#'
#' @export
ptd_spc_colours <- function(
  common_cause = "#a6a6a6",
  special_cause_improvement = "#00b0f0", # blue
  special_cause_neutral = "#490092", # purple
  special_cause_concern = "#e46c0a", # orange
  value_line = "#a6a6a6",
  mean_line = "#000000",
  lpl = "#a6a6a6",
  upl = "#a6a6a6",
  target = "#de1b1b",
  trajectory = "#490092"
) {
  structure(
    list(
      common_cause = common_cause,
      special_cause_improvement = special_cause_improvement,
      special_cause_neutral = special_cause_neutral,
      special_cause_concern = special_cause_concern,
      value_line = value_line,
      mean_line = mean_line,
      lpl = lpl,
      upl = upl,
      target = target,
      trajectory = trajectory
    ),
    class = "ptd_spc_colours_class"
  )
}
