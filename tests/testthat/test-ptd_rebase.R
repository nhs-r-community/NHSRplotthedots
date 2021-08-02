library(testthat)
library(mockery)

test_that("it returns NULL if no arguments are passed", {
  expect_null(ptd_rebase())
})

test_that("it returns an error if a single vector is passed which is not a date vector", {
  em <- "ptd_rebase(): all items must be date vectors."
  expect_error(ptd_rebase("a"), em, fixed = TRUE)
})

test_that("it returns a date vector if a single date vector is passed", {
  d <- Sys.Date() + 0:3
  expect_equal(ptd_rebase(d), d)
})

test_that("it returns an error if multiple arguments are passed which aren't named", {
  d <- Sys.Date() + 0:3
  em <- "ptd_rebase(): some items are not named."
  expect_error(ptd_rebase(d, d), em, fixed = TRUE)
  expect_error(ptd_rebase("a" = d, d), em, fixed = TRUE)
})

test_that("it returns a named list if a named list of dates is passed", {
  d1 <- Sys.Date() + 0:3
  d2 <- Sys.Date() + 4:7

  expect_equal(ptd_rebase("a" = d1, "b" = d2), list("a" = d1, "b" = d2))
})

test_that("it returns an error if some items aren't dates", {
  d1 <- Sys.Date() + 0:3
  d2 <- 1

  em <- "ptd_rebase(): all items must be date vectors."
  expect_error(ptd_rebase("a" = d1, "b" = d2), em, fixed = TRUE)
})
