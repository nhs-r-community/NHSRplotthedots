library(testthat)
library(mockery)

# ptd_validate_spc_options() ----

test_that("options must be created by ptd_spc_options()", {
  ptd_validate_spc_options(list(), NULL) |>
    expect_error("options must be created by ptd_spc_options()")
})

test_that(".data must be a data.frame", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a")

  expect_error(ptd_validate_spc_options(o, NULL), ".data must be a data.frame")
  expect_error(ptd_validate_spc_options(o, 1), ".data must be a data.frame")
  expect_error(ptd_validate_spc_options(o, "a"), ".data must be a data.frame")
  ptd_validate_spc_options(o, d)
})

test_that("it returns an error if value_field does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("x", "b")
  ptd_validate_spc_options(o, d) |>
    expect_error("value_field: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if date_field does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "x")
  ptd_validate_spc_options(o, d) |>
    expect_error("date_field: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if facet_field does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", facet_field = "c")
  ptd_validate_spc_options(o, d) |>
    expect_error("facet_field: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if rebase is a named list, and the names don't appear in the facet column", { # nolint
  d <- data.frame(a = Sys.Date() + 1:4, b = 1:4, f = c("a", "b", "c", "d"))

  o1 <- ptd_spc_options("b", "a", facet_field = "f", rebase = list("a" = d[[1]]))
  ptd_validate_spc_options(o1, d)

  o2 <- ptd_spc_options("b", "a", facet_field = "f", rebase = list("A" = d[[1]]))
  ptd_validate_spc_options(o2, d) |>
    expect_error("options provided to rebase are not in the facet_field column.")
})

test_that("it returns an error if target is a named list, and the names don't appear in the facet column", { # nolint
  d <- data.frame(a = Sys.Date() + 1:4, b = 1:4, f = c("a", "b", "c", "d"))

  o1 <- ptd_spc_options("b", "a", facet_field = "f", target = list("a" = 1))
  ptd_validate_spc_options(o1, d)

  o2 <- ptd_spc_options("b", "a", facet_field = "f", target = list("A" = 1))
  ptd_validate_spc_options(o2, d) |>
    expect_error("options provided to target are not in the facet_field column.")
})

test_that("it returns an error if trajectory does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", trajectory = "c")
  ptd_validate_spc_options(o, d) |>
    expect_error("trajectory: 'c' must be a valid column name in the data frame.")
})

test_that("date_field can only appear once per facet", {
  d <- data.frame(a = rep(Sys.Date(), 2), b = 1:2, g = 1:2)

  o1 <- ptd_spc_options("b", "a")
  expect_error(ptd_validate_spc_options(o1, d), "duplicate rows found in 'a'")

  o2 <- ptd_spc_options("b", "a", facet_field = "g")
  expect_true(ptd_validate_spc_options(o2, d))
})

test_that("date_field must be either a Date or POSIXt vector", {
  o <- ptd_spc_options("b", "a")
  ptd_validate_spc_options(o, data.frame(a = Sys.Date(), b = 1))
  ptd_validate_spc_options(o, data.frame(a = Sys.time(), b = 1))

  expect_error(ptd_validate_spc_options(o, data.frame(a = 1, b = 1)),
    "date_field must be a Date or POSIXt vector ('a' is a 'numeric').",
    fixed = TRUE
  )
  expect_error(ptd_validate_spc_options(o, data.frame(a = "a", b = 1)),
    "date_field must be a Date or POSIXt vector ('a' is a 'character').",
    fixed = TRUE
  )
})

test_that("value_field must be a numeric", {
  o <- ptd_spc_options("b", "a")
  ptd_validate_spc_options(o, data.frame(a = Sys.Date(), b = 1))

  expect_error(ptd_validate_spc_options(o, data.frame(a = Sys.Date(), b = "a")),
    "value_field must be a numeric vector ('b' is a 'character').",
    fixed = TRUE
  )
})
