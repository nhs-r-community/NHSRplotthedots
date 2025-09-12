#' SPC Options Function
#'
#' `ptd_spc_options` returns a list object containing properties which adjust
#'  the output of the `ptd_spc` function.
#'
#' This function is designed to allow greater control over SPC charts created
#'  using this package.  `ptd_spc_options` is a list with named slots for known
#'  parameters within the `ptd_spc` function. It should be supplied to the
#'  options argument within the `ptd_spc` function, with the options listed
#'  within `ptd_spc_options`. See examples below.
#'
#' @inheritParams spc
#' @noRd
ptd_spc_options <- function(
  value_field,
  date_field,
  facet_field = NULL,
  rebase = NULL,
  fix_after_n_points = NULL,
  improvement_direction = c("increase", "neutral", "decrease"),
  target = NULL,
  trajectory = NULL,
  screen_outliers = TRUE
) {
  assertthat::assert_that(
    is.character(value_field),
    assertthat::is.scalar(value_field),
    msg = "value_field argument must be a 'character' of length 1."
  )

  assertthat::assert_that(
    is.character(date_field),
    assertthat::is.scalar(date_field),
    msg = "date_field argument must be a 'character' of length 1."
  )

  if (!is.null(facet_field)) {
    assertthat::assert_that(
      is.character(facet_field),
      assertthat::is.scalar(facet_field),
      msg = "facet_field argument must be a 'character' of length 1."
    )
  }

  if (!is.null(rebase)) {
    assertthat::assert_that(
      (
        is_date(rebase) || (
          all(vapply(rebase, is_date, logical(1))) && !is.null(names(rebase))
        )
      ),
      msg = "rebase argument must be a date vector, or a named list of date vectors."
    )

    assertthat::assert_that(
      !(is.list(rebase) && is.null(facet_field)),
      msg = "rebase must be a date vector if facet_field is not set"
    )
  }

  if (!is.null(fix_after_n_points)) {
    assertthat::assert_that(
      is.numeric(fix_after_n_points),
      assertthat::is.scalar(fix_after_n_points),
      fix_after_n_points >= 12,
      msg = paste0(
        "fix_after_n_points must be a single numeric that is greater than ",
        "or equal to 12."
      )
    )
  }

  improvement_direction <- match.arg(improvement_direction)

  if (!is.null(target)) {
    assertthat::assert_that(
      (
        (is.numeric(target) && assertthat::is.scalar(target)) || (
          all(vapply(target, is.numeric, logical(1))) &&
            all(vapply(target, assertthat::is.scalar, logical(1))) &&
            !is.null(names(target))
        )
      ),
      msg = "target argument must be a single numeric, or a named list of numerics."
    )

    assertthat::assert_that(
      !(is.list(target) && is.null(target)),
      msg = "target must be a single numeric if facet_field is not set"
    )
  }

  if (!is.null(trajectory)) {
    assertthat::assert_that(
      is.character(trajectory),
      assertthat::is.scalar(trajectory),
      msg = "trajectory argument must be a 'character' of length 1."
    )
  }

  assertthat::assert_that(
    is.logical(screen_outliers),
    assertthat::is.scalar(screen_outliers),
    msg = "screen_outliers must either `TRUE` or `FALSE`."
  )

  structure(
    list(
      value_field = value_field,
      date_field = date_field,
      facet_field = facet_field,
      rebase = rebase,
      fix_after_n_points = fix_after_n_points,
      improvement_direction = improvement_direction,
      target = target,
      trajectory = trajectory,
      screen_outliers = screen_outliers
    ),
    class = "ptd_spc_options"
  )
}

#' @export
print.ptd_spc_options <- function(x, ...) {
  f <- function(s, surround = "'") {
    a <- crayon::bold(s, ":", sep = "")
    p <- paste(rep(" ", 21 - nchar(s)), collapse = "")

    b <- if (is.null(x[[s]])) {
      crayon::blue("not set")
    } else {
      crayon::red(paste0(surround, x[[s]], surround))
    }
    paste0(a, p, b)
  }

  l <- min(max(unlist(sapply(x, nchar))) + 24, 120)
  f("value_field")

  lines <- c(
    crayon::bold("Plot the Dots SPC options:"),
    paste(rep("=", l), collapse = ""),
    f("value_field"),
    f("date_field"),
    f("facet_field"),
    f("rebase"),
    f("fix_after_n_points", ""),
    f("improvement_direction"),
    f("target"),
    f("trajectory"),
    f("screen_outliers"),
    paste(rep("-", l), collapse = "")
  )

  cat(lines, sep = "\n")
}
