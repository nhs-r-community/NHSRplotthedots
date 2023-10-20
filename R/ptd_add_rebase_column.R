# adds a column named rebase
# with 1's in any row corresponding to a rebase date
# and 0's everywhere else

ptd_add_rebase_column <- function(.data, date_field, facet_field, rebase) {
  if (is.list(rebase)) {
    rebase_table <- dplyr::bind_rows(
      lapply(
        seq_along(rebase),
        function(i) {
          data.frame(
            d = to_datetime(rebase[[i]]),
            f = names(rebase)[[i]], rebase = 1
          )
        }
      )
    )
    colnames(rebase_table) <- c(date_field, facet_field, "rebase")

    .data <- .data %>%
      dplyr::left_join(rebase_table, by = c(date_field, facet_field)) %>%
      dplyr::mutate(
        dplyr::across(rebase, ~ ifelse(is.na(.x), 0, 1))
      )
  } else if (!is.null(rebase)) {
    # in with NULL returns FALSE, so this is suitable even if rebase isn't provided
    .data$rebase <- as.numeric(.data[[date_field]] %in% to_datetime(rebase))
  } else {
    .data$rebase <- 0
  }

  .data
}
