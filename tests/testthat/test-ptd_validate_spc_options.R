library(testthat)
library(mockery)

# ptd_validate_spc_options() ----

test_that("options must be created by ptd_spc_options()", {
  expect_error(ptd_validate_spc_options(list(), NULL), "options must be created by ptd_spc_options()")
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
  expect_error(ptd_validate_spc_options(o, d), "value_field: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if date_field does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "x")
  expect_error(ptd_validate_spc_options(o, d), "date_field: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if facet_field does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", facet_field = "c")
  expect_error(ptd_validate_spc_options(o, d), "facet_field: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if rebase is a named list, and the names don't appear in the facet column", {
  d <- data.frame(a = Sys.Date() + 1:4, b = 1:4, f = c("a", "b", "c", "d"))

  o1 <- ptd_spc_options("b", "a", facet_field = "f", rebase = list("a" = d[[1]]))
  ptd_validate_spc_options(o1, d)

  o2 <- ptd_spc_options("b", "a", facet_field = "f", rebase = list("A" = d[[1]]))
  expect_error(ptd_validate_spc_options(o2, d), "options provided to rebase are not in the facet_field column.")
})

test_that("it returns an error if target does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", target = "c")
  expect_error(ptd_validate_spc_options(o, d), "target: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if trajectory does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", trajectory = "c")
  expect_error(ptd_validate_spc_options(o, d), "trajectory: 'c' must be a valid column name in the data frame.")
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
