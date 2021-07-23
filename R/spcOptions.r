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
                       improvementDirection = "increase",
                       target = NULL,
                       trajectory = NULL) {

  if (!(is.character(valueField) && length(valueField) == 1)) {
    stop("valueField argument must be a 'character' of length 1.")
  }

  if (!(is.character(dateField) && length(dateField) == 1)) {
    stop("dateField argument must be a 'character' of length 1.")
  }

  if (!is.null(facetField) && !(is.character(facetField) && length(facetField) == 1)) {
    stop("facetField argument must be a 'character' of length 1.")
  }

  if (!is.null(rebase) && !(is.character(rebase) && length(rebase) == 1)) {
    stop("rebase argument must be a 'character' of length 1.")
  }

  if (!is.null(fixAfterNPoints) && !(
    is.numeric(fixAfterNPoints) &&
    length(fixAfterNPoints) == 1 &&
    fixAfterNPoints >= 12
  )) {
    stop("fixAfterNPoints must be a single numeric that is greater than or equal to 12.")
  }

  if (!improvementDirection %in% c("increase", "decrease")) {
    stop("Improvement direction should be a either 'increase' or 'decrease'.")
  }

  if (!is.null(target) && !(is.character(target) && length(target) == 1)) {
    stop("target argument must be a 'character' of length 1.")
  }

  if (!is.null(trajectory) && !(is.character(trajectory) && length(trajectory) == 1)) {
    stop("trajectory argument must be a 'character' of length 1.")
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

#' @export
validate.ptd_spc_options <- function(options, .data) {
  stopifnot(".data must be a data.frame" = inherits(.data, "data.frame"))

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
