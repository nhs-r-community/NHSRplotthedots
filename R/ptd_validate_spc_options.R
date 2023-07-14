ptd_validate_spc_options <- function(options, .data) {
  assertthat::assert_that(
    inherits(options, "ptd_spc_options"),
    msg = "options must be created by ptd_spc_options()"
  )
  assertthat::assert_that(
    inherits(.data, "data.frame"),
    msg = ".data must be a data.frame"
  )

  check <- function(op) {
    if (is.null(options[[op]])) {
      return(TRUE)
    }
    if (options[[op]] %in% colnames(.data)) {
      return(TRUE)
    }
    stop(op, ": '", options[[op]], "' must be a valid column name in the data frame.")
  }
  check("value_field")
  check("date_field")
  check("facet_field")
  check("trajectory")

  assertthat::assert_that(
    all(
      dplyr::count(
        dplyr::group_by_at(
          .data,
          c(
            options[["date_field"]],
            options[["facet_field"]]
          )
        )
      )$n == 1
    ),
    msg = paste0("duplicate rows found in '", options[["date_field"]], "'")
  )

  assertthat::assert_that(
    is_date(.data[[options[["date_field"]]]]),
    msg = paste0(
      "date_field must be a Date or POSIXt vector ('",
      options["date_field"], "' is a '", class(.data[[options[["date_field"]]]]), "')."
    )
  )

  assertthat::assert_that(
    is.numeric(.data[[options[["value_field"]]]]),
    msg = paste0(
      "value_field must be a numeric vector ('",
      options["value_field"], "' is a '", class(.data[[options[["value_field"]]]]), "')."
    )
  )

  if (!is.null(options[["rebase"]]) && !is_date(options[["rebase"]])) {
    # if rebase is a date vector, no need to do anything
    assertthat::assert_that(
      all(names(options[["rebase"]]) %in% unique(.data[[options[["facet_field"]]]])),
      msg = "options provided to rebase are not in the facet_field column."
    )
  }

  if (!is.null(options[["target"]]) && !is.numeric(options[["target"]])) {
    # if target is a numeric vector, no need to do anything
    assertthat::assert_that(
      all(names(options[["target"]]) %in% unique(.data[[options[["facet_field"]]]])),
      msg = "options provided to target are not in the facet_field column."
    )
  }

  invisible(TRUE)
}
