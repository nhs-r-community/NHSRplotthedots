isDate <- function(x, format = "%Y-%m-%d") {
  formatted <- try(as.Date(x, format), silent = TRUE)
  isDate <- as.character(formatted) == x & !is.na(formatted)  # valid and identical to input
  isDate[is.na(x)] <- NA  # Insert NA for NA in x
  return(isDate)
}
