#' SPC Options Function
#'
#' `spcOptions` returns a list object containing properties which adjust the output of the 'spc' function.
#'
#' This function is designed to allow greater control over SPC charts created using this package.  spcOptions is a list
#' with named slots for known parameters within the spc function. It should be supplied to the options argument within
#' the spc function, with the options listed within spcOptions.  See examples below.
#'
#' @inheritParams spc
#'
#' @noRd
spcOptions <- function(valueField,
                       dateField,
                       facetField = NULL,
                       rebase = NULL,
                       fixAfterNPoints = NULL,
                       improvementDirection = c("increase", "decrease"),
                       target = NULL,
                       trajectory = NULL) {

  assertthat::assert_that(
    is.character(valueField),
    assertthat::is.scalar(valueField),
    msg = "valueField argument must be a 'character' of length 1."
  )

  assertthat::assert_that(
    is.character(dateField),
    assertthat::is.scalar(dateField),
    msg = "dateField argument must be a 'character' of length 1."
  )

  if (!is.null(facetField)) {
    assertthat::assert_that(
      is.character(facetField),
      assertthat::is.scalar(facetField),
      msg = "facetField argument must be a 'character' of length 1."
    )
  }

  if (!is.null(rebase)) {
    assertthat::assert_that(
      is.character(rebase),
      assertthat::is.scalar(rebase),
      msg = "rebase argument must be a 'character' of length 1."
    )
  }

  if (!is.null(fixAfterNPoints)) {
    assertthat::assert_that(
      is.numeric(fixAfterNPoints),
      assertthat::is.scalar(fixAfterNPoints),
      fixAfterNPoints >= 12,
      msg = "fixAfterNPoints must be a single numeric that is greater than or equal to 12."
    )
  }

  improvementDirection <- match.arg(improvementDirection)

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

  structure(
    list(
      valueField = valueField,
      dateField = dateField,
      facetField = facetField,
      rebase = rebase,
      fixAfterNPoints = fixAfterNPoints,
      improvementDirection = improvementDirection,
      target = target,
      trajectory = trajectory
    ),
    class = "ptd_spc_options"
  )
}

validateSpcOptions <- function(options, .data) {
  assertthat::assert_that(
    inherits(options, "ptd_spc_options"),
    msg = "options must be created by spcOptions()"
  )
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = ".data must be a data.frame"
  )

  check <- function(op) {
    if (is.null(options[[op]])) return(TRUE)
    if (options[[op]] %in% colnames(.data)) return(TRUE)
    stop(op, ": '", options[[op]], "' must be a valid column name in the data frame.")
  }
  check("valueField")
  check("dateField")
  check("facetField")
  check("rebase")
  check("target")
  check("trajectory")

  assertthat::assert_that(
    all(count(group_by_at(.data, all_of(c(options[["dateField"]], options[["facetField"]]))))$n == 1),
    msg = paste0("duplicate rows found in '", options[["dateField"]], "'")
  )

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
  f("valueField")

  lines <- c(
    crayon::bold("Plot the Dots SPC options:"),
    paste(rep("=", l), collapse = ""),
    f("valueField"),
    f("dateField"),
    f("facetField"),
    f("rebase"),
    f("fixAfterNPoints", ""),
    f("improvementDirection"),
    f("target"),
    f("trajectory"),
    paste(rep("-", l), collapse = "")
  )

  cat(lines, sep = "\n")
}
