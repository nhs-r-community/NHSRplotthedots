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

# ptd_spc() ----

test_that("it throws an error if .data is not a data.frame", {
  expect_error(ptd_spc("x", "a", "b"), "ptd_spc: .data must be a data.frame")
})

test_that("it returns a ptd_spc_df object", {
  stub(ptd_spc, "spcOptions", options)
  stub(ptd_spc, "validateSpcOptions", TRUE)
  stub(ptd_spc, "spcStandard", function(x, ...) x)
  stub(ptd_spc, "calculatePointHighlighting", function(x, ...) x)
  stub(ptd_spc, "as.POSIXct", function(x, ...) x)

  s <- ptd_spc(data, "y", "x")

  expect_s3_class(s, c("ptd_spc_df", "data.frame"))
})

test_that("it has options as an attribute, created by spcOptions", {
  m <- mock(options)

  stub(ptd_spc, "ptd_spcOptions", m)
  stub(ptd_spc, "ptd_validateSpcOptions", TRUE)
  stub(ptd_spc, "ptd_spcStandard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculatePointHighlighting", function(x, ...) x)
  stub(ptd_spc, "as.POSIXct", function(x, ...) x)

  s <- ptd_spc(data, "a", "b", "c", "d", "e", "f", "g", "h")
  options <- attr(s, "options")

  expect_equal(options, options)
  expect_called(m, 1)
  expect_args(m, 1, "a", "b", "c", "d", "e", "f", "g", "h")
})

test_that("it validates the options", {
  m <- mock(TRUE)

  stub(ptd_spc, "ptd_spcOptions", options)
  stub(ptd_spc, "ptd_validateSpcOptions", m)
  stub(ptd_spc, "ptd_spcStandard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculatePointHighlighting", function(x, ...) x)
  stub(ptd_spc, "as.POSIXct", function(x, ...) x)
  stub(ptd_spc, "as.POSIXct", function(x, ...) x)

  s <- ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, options, data)
})

test_that("it calls spcStandard", {
  m <- mock("spcStandard")

  stub(ptd_spc, "ptd_spcOptions", options)
  stub(ptd_spc, "ptd_validateSpcOptions", TRUE)
  stub(ptd_spc, "ptd_spcStandard", m)
  stub(ptd_spc, "ptd_calculatePointHighlighting", function(x, ...) x)
  stub(ptd_spc, "as.POSIXct", function(x, ...) x)

  s <- ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, options)
})

test_that("it calls calculatePointHighlighting (increase)", {
  m <- mock("calculatePointHighlighting")

  options$improvementDirection <- "increase"
  stub(ptd_spc, "ptd_spcOptions", options)
  stub(ptd_spc, "ptd_validateSpcOptions", TRUE)
  stub(ptd_spc, "ptd_spcStandard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculatePointHighlighting", m)
  stub(ptd_spc, "as.POSIXct", function(x, ...) x)

  ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, 1)
})

test_that("it calls calculatePointHighlighting (decrease)", {
  m <- mock("calculatePointHighlighting")

  options$improvementDirection <- "decrease"
  stub(ptd_spc, "ptd_spcOptions", options)
  stub(ptd_spc, "ptd_validateSpcOptions", TRUE)
  stub(ptd_spc, "ptd_spcStandard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculatePointHighlighting", m)
  stub(ptd_spc, "as.POSIXct", function(x, ...) x)

  ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, -1)
})

test_that("it converts dateField to POSIXct", {
  m <- mock("as.POSIXct")

  options$improvementDirection <- "decrease"
  stub(ptd_spc, "ptd_spcOptions", options)
  stub(ptd_spc, "ptd_validateSpcOptions", TRUE)
  stub(ptd_spc, "ptd_spcStandard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculatePointHighlighting", function(x, ...) x)
  stub(ptd_spc, "as.POSIXct", m)

  ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data$x, tz = "utc")
})

# print() ----

test_that("it calls plot", {
  m <- mock("plot")
  stub(print.ptd_spc_df, "plot", m)

  s <- ptd_spc(data, "y", "x")
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

  s <- ptd_spc(data, "y", "x")
  o <- capture_output(print(s))

  expect_called(m, 1)
  expect_args(m, 1, "plot")
})

# summary() ----
test_that("it outputs expected content", {
  d <- data

  d$rebase <- 0
  d$rebase[10] <- 1

  d$facet <- rep(c(0, 1), each = 10)

  s1 <- ptd_spc(d, "y", "x")
  expect_snapshot_output(summary(s1))

  s2 <- ptd_spc(d, "y", "x", rebase = "rebase")
  expect_snapshot_output(summary(s2))

  s3 <- ptd_spc(d, "y", "x", facetField = "facet")
  expect_snapshot_output(summary(s3))

  s4 <- ptd_spc(d, "y", "x", rebase = "rebase", facetField = "facet")
  expect_snapshot_output(summary(s4))
})
