library(testthat)
library(mockery)

set.seed(123)
data <- data.frame(
  x = as.Date("2020-01-01") + 1:20,
  y = rnorm(20)
)

options <- list(
  valueField = "a",
  dateField = "b",
  facetField = "c",
  rebase = "d",
  fixAfterNPoints = "e",
  improvementDirection = "f",
  target = "g",
  trajectory = "h"
)

# spc() ----

test_that("it throws an error if .data is not a data.frame", {
  expect_error(spc("x", "a", "b"), "spc: .data must be a data.frame")
})

test_that("it returns a ptd_spc_df object", {
  stub(spc, "spcOptions", options)
  stub(spc, "validateSpcOptions", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)
  stub(spc, "as.POSIXct", function(x, ...) x)

  s <- spc(data, "y", "x")

  expect_s3_class(s, c("ptd_spc_df", "data.frame"))
})

test_that("it has options as an attribute, created by spcOptions", {
  m <- mock(options)

  stub(spc, "spcOptions", m)
  stub(spc, "validateSpcOptions", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)
  stub(spc, "as.POSIXct", function(x, ...) x)

  s <- spc(data, "a", "b", "c", "d", "e", "f", "g", "h")
  options <- attr(s, "options")

  expect_equal(options, options)
  expect_called(m, 1)
  expect_args(m, 1, "a", "b", "c", "d", "e", "f", "g", "h")
})

test_that("it validates the options", {
  m <- mock(TRUE)

  stub(spc, "spcOptions", options)
  stub(spc, "validateSpcOptions", m)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)
  stub(spc, "as.POSIXct", function(x, ...) x)
  stub(spc, "as.POSIXct", function(x, ...) x)

  s <- spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, options, data)
})

test_that("it calls spcStandard", {
  m <- mock("spcStandard")

  stub(spc, "spcOptions", options)
  stub(spc, "validateSpcOptions", TRUE)
  stub(spc, "spcStandard", m)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)
  stub(spc, "as.POSIXct", function(x, ...) x)

  s <- spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, options)
})

test_that("it calls calculatePointHighlighting (increase)", {
  m <- mock("calculatePointHighlighting")

  options$improvementDirection <- "increase"
  stub(spc, "spcOptions", options)
  stub(spc, "validateSpcOptions", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", m)
  stub(spc, "as.POSIXct", function(x, ...) x)

  spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data,  1)
})

test_that("it calls calculatePointHighlighting (decrease)", {
  m <- mock("calculatePointHighlighting")

  options$improvementDirection <- "decrease"
  stub(spc, "spcOptions", options)
  stub(spc, "validateSpcOptions", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", m)
  stub(spc, "as.POSIXct", function(x, ...) x)

  spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, -1)
})

test_that("it converts dateField to POSIXct", {
  m <- mock("as.POSIXct")

  options$improvementDirection <- "decrease"
  stub(spc, "spcOptions", options)
  stub(spc, "validateSpcOptions", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)
  stub(spc, "as.POSIXct", m)

  spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data$x, tz = "utc")
})

# print() ----

test_that("it calls plot", {
  m <- mock("plot")
  stub(print.ptd_spc_df, "plot", m)

  s <- spc(data, "y", "x")
  o <- capture_output(print(s))

  expect_called(m, 1)
  expect_args(m, 1, s)

  # check that print is called on the return of plot: this is a mocked output
  expect_equal(o, "[1] \"plot\"")
})

test_that("it calls print", {
  m <- mock("print")
  stub(print.ptd_spc_df, "plot", "plot")
  stub(print.ptd_spc_df, "print", m)

  s <- spc(data, "y", "x")
  o <- capture_output(print(s))

  expect_called(m, 1)
  expect_args(m, 1, "plot")
})
