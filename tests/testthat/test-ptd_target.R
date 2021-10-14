library(testthat)
library(mockery)

test_that("it returns NULL if no arguments are passed", {
  expect_null(ptd_target())
})

test_that("it returns an error if a single value is passed which is not a numeric", {
  em <- "ptd_target(): all items must be scalar numerics."
  expect_error(ptd_target("a"), em, fixed = TRUE)
})

test_that("it returns a scalar numeric if a single numeric is passed", {
  v <- 0.9
  expect_equal(ptd_target(v), v)
})

test_that("it returns an error if multiple arguments are passed which aren't named", {
  v1 <- 0.8
  v2 <- 0.9
  em <- "ptd_target(): some items are not named."
  expect_error(ptd_target(v1, v2), em, fixed = TRUE)
  expect_error(ptd_target("a" = v1, v2), em, fixed = TRUE)
})

test_that("it returns a named list if a named list of numerics is passed", {
  v1 <- 0.8
  v2 <- 0.9

  expect_equal(ptd_target("a" = v1, "b" = v2), list("a" = v1, "b" = v2))
})

test_that("it returns an error if some items aren't numerics", {
  v1 <- 0.8
  v2 <- "0.9"

  em <- "ptd_target(): all items must be scalar numerics."
  expect_error(ptd_target("a" = v1, "b" = v2), em, fixed = TRUE)
})

test_that("it returns an error if some items aren't scalar numerics", {
  v1 <- 0.8
  v2 <- c(v1, v1)

  em <- "ptd_target(): all items must be scalar numerics."
  expect_error(ptd_target("a" = v1, "b" = v2), em, fixed = TRUE)
})
