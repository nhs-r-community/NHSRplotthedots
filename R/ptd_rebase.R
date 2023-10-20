#' Rebase
#'
#' Produces an object that can be used for rebasing an SPC chart. This method
#' provides two different ways to rebase:
#'   1. You can either provide a single vector of dates, which will rebase every
#'    facet of an SPC with the same dates
#'   2. You can provide named vectors of dates, where the names correspond to the
#'    names of the facets, in order to rebase a faceted chart.
#'
#' @param ... Either a single vector of dates, or named vectors of dates.
#'  See examples.
#' @returns A list
#' @export
#'
#' @examples
#' # If you aren't using a faceted chart, or you want to rebase each facet at
#' # the same dates, then you can simply call this method with a vector of dates.
#' # For example, to rebase on the 1^st^ January 2020 and 22^nd^ March 2020:
#'
#' ptd_rebase(as.Date(c("2020-01-01", "2020-03-22")))
#'
#' # If you are using a faceted chart, and wish to rebase each facet with
#' # different dates, then you can call this method, naming each vector of dates
#' # with the name of the facet. If there are facets that you don't need to rebase
#' # you can simply ignore them. For example, if you had a chart with facets "a",
#' # "b", and "c", and you wanted to rebase "a" on the 1^st^ January 2020, and
#' # "b" on the 22^nd^ March 2020:
#'
#' ptd_rebase("a" = as.Date("2020-01-01"), "b" = as.Date("2020-03-22"))
ptd_rebase <- function(...) {
  rebase <- list(...)

  if (length(rebase) == 0) {
    return(NULL)
  }

  if (length(rebase) == 1 && is.null(names(rebase))) {
    assertthat::assert_that(
      is_date(rebase[[1]]),
      msg = "ptd_rebase(): all items must be date vectors."
    )
    return(rebase[[1]])
  }

  assertthat::assert_that(
    !is.null(names(rebase)),
    all(names(rebase) != ""),
    msg = "ptd_rebase(): some items are not named."
  )

  assertthat::assert_that(
    all(vapply(rebase, is_date, logical(1))),
    msg = "ptd_rebase(): all items must be date vectors."
  )

  rebase
}
