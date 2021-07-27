ptd_capitalise <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

ptd_title_case <- function(x) {
  ptd_capitalise(gsub("_(.)", " \\U\\1", x, perl = TRUE))
}
