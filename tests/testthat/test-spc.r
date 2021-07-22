library(testthat)
library(mockery)

data <- data.frame(
  x = 1:20,
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

test_that("it returns a ptd_spc_df object", {
  stub(spc, "spcOptions", options)
  stub(spc, "validate", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)

  data <- data.frame(
    x = 1:20,
    y = rnorm(20)
  )

  s <- spc(data, "y", "x")

  expect_s3_class(s, c("ptd_spc_df", "data.frame"))

})

test_that("it has options as an attribute, created by spcOptions", {
  m <- mock(options)

  stub(spc, "spcOptions", m)
  stub(spc, "validate", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)

  s <- spc(data, "a", "b", "c", "d", "e", "f", "g", "h")
  options <- attr(s, "options")

  expect_equal(options, options)
  expect_called(m, 1)
  expect_args(m, 1, "a", "b", "c", "d", "e", "f", "g", "h")
})

test_that("it validates the options", {
  m <- mock(TRUE)

  stub(spc, "spcOptions", options)
  stub(spc, "validate", m)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)

  s <- spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, options, data)
})

test_that("it calls spcStandard", {
  m <- mock("spcStandard")

  stub(spc, "spcOptions", options)
  stub(spc, "validate", TRUE)
  stub(spc, "spcStandard", m)
  stub(spc, "calculatePointHighlighting", function(x, ...) x)

  s <- spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, options)
})

test_that("it calls calculatePointHighlighting (increase)", {
  m <- mock("calculatePointHighlighting")

  options$improvementDirection <- "increase"
  stub(spc, "spcOptions", options)
  stub(spc, "validate", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", m)

  spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data,  1)
})

test_that("it calls calculatePointHighlighting (decrease)", {
  m <- mock("calculatePointHighlighting")

  options$improvementDirection <- "decrease"
  stub(spc, "spcOptions", options)
  stub(spc, "validate", TRUE)
  stub(spc, "spcStandard", function(x, ...) x)
  stub(spc, "calculatePointHighlighting", m)

  spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, -1)
})
