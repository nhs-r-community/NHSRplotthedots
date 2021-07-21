capitalise <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}