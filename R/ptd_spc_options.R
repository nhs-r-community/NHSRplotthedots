#' SPC Options Function
#'
#' `ptd_spc_options` returns a list object containing properties which adjust the output of the `ptd_spc` function.
#'
#' This function is designed to allow greater control over SPC charts created using this package.  `ptd_spc_options` is
#' a list with named slots for known parameters within the `ptd_spc` function. It should be supplied to the options
#' argument within the `ptd_spc` function, with the options listed within `ptd_spc_options`.  See examples below.
#'
#' @inheritParams spc
#'
#' @noRd
ptd_spc_options <- function(value_field,
                            date_field,
                            facet_field = NULL,
                            rebase = NULL,
                            fix_after_n_points = NULL,
                            improvement_direction = c("increase", "decrease"),
                            target = NULL,
                            trajectory = NULL) {
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
      is.character(rebase),
      assertthat::is.scalar(rebase),
      msg = "rebase argument must be a 'character' of length 1."
    )
  }

  if (!is.null(fix_after_n_points)) {
    assertthat::assert_that(
      is.numeric(fix_after_n_points),
      assertthat::is.scalar(fix_after_n_points),
      fix_after_n_points >= 12,
      msg = "fix_after_n_points must be a single numeric that is greater than or equal to 12."
    )
  }

  improvement_direction <- match.arg(improvement_direction)

  if (!is.null(target)) {
    assertthat::assert_that(
      is.character(target),
      assertthat::is.scalar(target),
      msg = "target argument must be a 'character' of length 1."
    )
  }

  if (!is.null(trajectory)) {
    assertthat::assert_that(
      is.character(trajectory),
      assertthat::is.scalar(trajectory),
      msg = "trajectory argument must be a 'character' of length 1."
    )
  }

  # TODO: check that this is correct
  assertthat::assert_that(
    is.null(fix_after_n_points) || is.null(rebase),
    msg = "cannot rebase and fix_after_n_points"
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
      trajectory = trajectory
    ),
    class = "ptd_spc_options"
  )
}

ptd_validate_spc_options <- function(options, .data) {
  assertthat::assert_that(
    inherits(options, "ptd_spc_options"),
    msg = "options must be created by ptd_spc_options()"
  )
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = ".data must be a data.frame"
  )

  check <- function(op) {
    if (is.null(options[[op]])) {
      return(TRUE)
    }
    if (options[[op]] %in% colnames(.data)) {
      return(TRUE)
    }
    stop(op, ": '", options[[op]], "' must be a valid column name in the data frame.")
  }
  check("value_field")
  check("date_field")
  check("facet_field")
  check("rebase")
  check("target")
  check("trajectory")

  assertthat::assert_that(
    all(count(group_by_at(.data, all_of(c(options[["date_field"]], options[["facet_field"]]))))$n == 1),
    msg = paste0("duplicate rows found in '", options[["date_field"]], "'")
  )

  assertthat::assert_that(
    inherits(.data[[options[["date_field"]]]], c("Date", "POSIXt")),
    msg = paste0(
      "date_field must be a Date or POSIXt vector ('",
      options["date_field"], "' is a '", class(.data[[options[["date_field"]]]]), "')."
    )
  )

  assertthat::assert_that(
    is.numeric(.data[[options[["value_field"]]]]),
    msg = paste0(
      "value_field must be a numeric vector ('",
      options["value_field"], "' is a '", class(.data[[options[["value_field"]]]]), "')."
    )
  )

  if (!is.null(options[["rebase"]])) {
    assertthat::assert_that(
      (is.numeric(.data[[options[["rebase"]]]]) || is.logical(.data[[options[["rebase"]]]])),
      all(.data[[options[["rebase"]]]] %in% c(0, 1)),
      msg = "values in the rebase column must either be 0 or 1."
    )
  }

  invisible(TRUE)
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
    paste(rep("-", l), collapse = "")
  )

  cat(lines, sep = "\n")
}
