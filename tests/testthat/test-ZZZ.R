library(testthat)
library(mockery)

test_that("null replacement works", {
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% 2, 2)
  expect_equal(NULL %||% 1 %||% 2, 1)
  expect_equal(NULL %||% NULL %||% 2, 2)
})

test_that("is_date works", {
  expect_true(is_date(as.Date("2020-01-01")))
  expect_true(is_date(Sys.time()))
  expect_true(is_date(as.POSIXlt(Sys.time())))
  expect_false(is_date("a"))
  expect_false(is_date(1))
})
