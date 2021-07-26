#' null replacement
#'
#' if `a` is null, choose `b`.
#'
#' @param a a possibly null value
#' @param b a value to replace `a` with if `a` is null
#'
#' @examples
#' \dontrun{
#' 1 %||% 2 == 1
#' NULL %||% 2 == 2
#' }
#'
#' @name null-replacement
#' @noRd

# Begin Exclude Linting
`%||%` <- function(a, b) {
  if (is.null(a)) {
    b
  } else {
    a
  }
}
# End Exclude Linting
