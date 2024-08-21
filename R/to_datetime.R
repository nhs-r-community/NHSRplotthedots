# Internal function to help ensure we convert dates correctly
# If we are given a Date object, promote it to a POSIXct object
# If we are given a POSIXt object, return it (identity)
# If we are given a character then try to promote it to a POSIXct object

to_datetime <- function(x) {
  UseMethod("to_datetime")
}

#' @export
to_datetime.Date <- function(x) {
  as.POSIXct(x)
}

#' @export
to_datetime.POSIXt <- identity

#' @export
to_datetime.character <- function(x) {
  as.POSIXct(x)
}
