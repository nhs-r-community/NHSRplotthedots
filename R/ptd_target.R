#' Target
#'
#' Produces an object that can be used for adding Targets to an SPC chart. This method provides two different ways to
#' add a target:
#'   1. You can either provide a single value, which will apply the same target to every facet of an SPC
#'   2. You can provide named values of targets, where the names correspond to the names of the facets, in order to
#'      have different targets for each facet
#'
#' @param ... either a single values, or, named values of targets. See examples.
#'
#' @return returns either:
#'   * a single numeric value, in this case all facets in the plot will use this target value
#'   * a named list of single numeric values, where each item is named as for one of the facet's in the plot. If a facet
#'   isn't specified then it will not have a target
#'
#' @details This function is a helper to provide data in the correct format for use with `ptd_spc()`. See **Value**
#' section for details of return type. If you are trying to do something like `ptd_spc(list_of_values)` then you can
#' skip using the function and just use `list_of_values`, so long as the list meets the requirements as listed above.
#'
#' @export
#'
#' @examples
#' # if you aren't using a facetted chart, or you want to use the same target for each facet, you
#' # can simply call this method with a single value. For example, to use a target of 90%:
#'
#' ptd_target(0.9)
#'
#' # if you are using a facetted chart, and wish to use a different target for each facet, then you
#' # can call this method, naming each value with the name of the facet. Any facet that isn't listed
#' # will not have a target applied to it.
#' #
#' # For example, to apply a target of 25 to the "a" facet and 10 to the "b" facet:
#'
#' ptd_rebase(
#'   "a" = 25,
#'   "b" = 10
#' )
ptd_target <- function(...) {
  target <- list(...)

  if (length(target) == 0) {
    return(NULL)
  }

  if (length(target) == 1 && is.null(names(target))) {
    assertthat::assert_that(
      is.numeric(target[[1]]),
      assertthat::is.scalar(target[[1]]),
      msg = "ptd_target(): all items must be scalar numerics."
    )
    return(target[[1]])
  }

  assertthat::assert_that(
    !is.null(names(target)),
    all(names(target) != ""),
    msg = "ptd_target(): some items are not named."
  )

  assertthat::assert_that(
    all(vapply(target, is.numeric, logical(1))),
    all(vapply(target, assertthat::is.scalar, logical(1))),
    msg = "ptd_target(): all items must be scalar numerics."
  )

  target
}
