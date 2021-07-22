library(testthat)

test_that("it returns correct data", {
  r <- spcOptions(
    valueField = "valueField",
    dateField = "dateField",
    facetField = "facetField",
    rebase = "rebase",
    fixAfterNPoints = 12,
    improvementDirection = "increase",
    target = "target",
    trajectory = "trajectory"
  )

  expect_equal(r$valueField, "valueField")
  expect_equal(r$dateField, "dateField")
  expect_equal(r$facetField, "facetField")
  expect_equal(r$rebase, "rebase")
  expect_equal(r$fixAfterNPoints, 12)
  expect_equal(r$improvementDirection, "increase")
  expect_equal(r$target, "target")
  expect_equal(r$trajectory, "trajectory")

  expect_s3_class(r, "ptd_spc_options")
})

test_that("valueField can only be a scalar character", {
  expect_error(spcOptions(1), "valueField argument must be a 'character' of length 1.")
  expect_error(spcOptions(c("a", "b")), "valueField argument must be a 'character' of length 1.")
})

test_that("dateField can only be a scalar character", {
  expect_error(spcOptions("a", 1), "dateField argument must be a 'character' of length 1.")
  expect_error(spcOptions("a", c("a", "b")), "dateField argument must be a 'character' of length 1.")
})

test_that("facetField is either null, or a scalar character", {
  # this should run without an error
  spcOptions("a", "b", facetField = NULL)
  expect_error(spcOptions("a", "b", facetField = 1), "facetField argument must be a 'character' of length 1.")
  expect_error(spcOptions("a", "b", facetField = c("a", "b")), "facetField argument must be a 'character' of length 1.")
})

test_that("fixAfterNPoints must be a single numeric that is greater than or equal to 12.", {
  expect_error(spcOptions("a", "b", fixAfterNPoints = "a"),
               "fixAfterNPoints must be a single numeric that is greater than or equal to 12.")
  expect_error(spcOptions("a", "b", fixAfterNPoints = c(15, 20)),
               "fixAfterNPoints must be a single numeric that is greater than or equal to 12.")
  expect_error(spcOptions("a", "b", fixAfterNPoints = 11),
               "fixAfterNPoints must be a single numeric that is greater than or equal to 12.")
  spcOptions("a", "b", fixAfterNPoints = 12)
})

test_that("improvementDirection defaults to increase", {
  o <- spcOptions("a", "b")
  expect_equal(o$improvementDirection, "increase")
})

test_that("improvementDirection must be one of increase or decrease", {
  spcOptions("a", "b", improvementDirection = "increase")
  spcOptions("a", "b", improvementDirection = "decrease")

  expect_error(spcOptions("a", "b", improvementDirection = "a"),
               "Improvement direction should be a either 'increase' or 'decrease'.")
})

test_that("target is either null, or a scalar character", {
  # this should run without an error
  spcOptions("a", "b", target = NULL)
  expect_error(spcOptions("a", "b", target = 1), "target argument must be a 'character' of length 1.")
  expect_error(spcOptions("a", "b", target = c("a", "b")), "target argument must be a 'character' of length 1.")
})

test_that("trajectory is either null, or a scalar character", {
  # this should run without an error
  spcOptions("a", "b", trajectory = NULL)
  expect_error(spcOptions("a", "b", trajectory = 1), "trajectory argument must be a 'character' of length 1.")
  expect_error(spcOptions("a", "b", trajectory = c("a", "b")), "trajectory argument must be a 'character' of length 1.")
})

test_that("printing output", {
  r <- spcOptions("hello", "world")
  expect_output(print(r), "Plot the Dots SPC options:")
  expect_output(print(r), "================================")
  expect_output(print(r), "valueField:.*'hello'")
  expect_output(print(r), "dateField:.*'world'")
  expect_output(print(r), "facetField:.*not set")
  expect_output(print(r), "rebase:.*not set")
  expect_output(print(r), "fixAfterNPoints:.*not set")
  expect_output(print(r), "improvementDirection:.*not set")
  expect_output(print(r), "target:.*not set")
  expect_output(print(r), "trajectory:.*not set")
  expect_output(print(r), "--------------------------------")
})
