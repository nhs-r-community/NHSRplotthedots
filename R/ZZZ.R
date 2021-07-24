#' null replacement
#'
#' if a is null, choose b
#'
`%||%` <- function(a, b) {
  if (is.null(a))
    b
  else
    a
}
