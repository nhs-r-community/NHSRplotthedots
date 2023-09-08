ptd_add_target_column <- function(.data, target) {
  if (is.null(target)) {
    .data$target <- as.double(NA)
  } else if (is.numeric(target)) {
    .data$target <- target
  } else {
    t <- data.frame(f = names(target), target = as.numeric(target))

    .data <- dplyr::left_join(.data, t, by = "f")
  }

  .data
}
