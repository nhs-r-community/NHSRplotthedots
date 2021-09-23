#' SPC Colours
#'
#' Produces a list of colours that controls the geoms in the plot
#'
#' @param common_cause,special_cause_improvement,special_cause_concern the colour of the points
#' @param value_line the colour of the line joining the points
#' @param mean_line the colour of the "mean" (average) line
#' @param lpl,upl the colour the the lower and upper process limit lines
#' @param target the colour of the target line
#' @param trajectory the colour of the trajectory line
#'
#' @return a list of colours
#'
#' @export
ptd_spc_colours <- function(common_cause = "#7B7D7D",
                            special_cause_improvement = "#289de0",
                            special_cause_concern = "#fab428",
                            value_line = "#7B7D7D",
                            mean_line = "#000000",
                            lpl = "#7B7D7D",
                            upl = "#7B7D7D",
                            target = "#de1b1b",
                            trajectory = "#361475") {
  structure(
    list(
      common_cause              = common_cause,
      special_cause_improvement = special_cause_improvement,
      special_cause_concern     = special_cause_concern,
      value_line                = value_line,
      mean_line                 = mean_line,
      lpl                       = lpl,
      upl                       = upl,
      target                    = target,
      trajectory                = trajectory
    ),
    class = "ptd_spc_colours_class"
  )
}
