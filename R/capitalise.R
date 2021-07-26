capitalise <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

titleCase <- function(x) {
  capitalise(gsub("_(.)", " \\U\\1", x, perl = TRUE))
}
