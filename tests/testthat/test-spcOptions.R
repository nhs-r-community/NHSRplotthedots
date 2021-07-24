library(testthat)

# spcOptions() ----
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

test_that("rebase is either null, or a scalar character", {
  # this should run without an error
  spcOptions("a", "b", rebase = NULL)
  expect_error(spcOptions("a", "b", rebase = 1), "rebase argument must be a 'character' of length 1.")
  expect_error(spcOptions("a", "b", rebase = c("a", "b")), "rebase argument must be a 'character' of length 1.")
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
               "'arg' should be one of \"increase\", \"decrease\"")
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

# print() ----

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

# validateSpcOptions() ----

test_that("options must be created by spcOptions()", {
  expect_error(validateSpcOptions(list(), NULL), "options must be created by spcOptions()")
})

test_that(".data must be a data.frame", {
  d <- data.frame(a = 1, b = 2)
  o <- spcOptions("a", "b")

  expect_error(validateSpcOptions(o, NULL), ".data must be a data.frame")
  expect_error(validateSpcOptions(o, 1),    ".data must be a data.frame")
  expect_error(validateSpcOptions(o, "a"),  ".data must be a data.frame")
  validateSpcOptions(o, d)
})

test_that("it returns an error if valueField does not exist in .data", {
  d <- data.frame(a = 1, b = 2)
  o <- spcOptions("x", "b")
  expect_error(validateSpcOptions(o, d), "valueField: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if dateField does not exist in .data", {
  d <- data.frame(a = 1, b = 2)
  o <- spcOptions("a", "x")
  expect_error(validateSpcOptions(o, d), "dateField: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if facetField does not exist in .data", {
  d <- data.frame(a = 1, b = 2)
  o <- spcOptions("a", "b", facetField = "c")
  expect_error(validateSpcOptions(o, d), "facetField: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if rebase does not exist in .data", {
  d <- data.frame(a = 1, b = 2)
  o <- spcOptions("a", "b", rebase = "c")
  expect_error(validateSpcOptions(o, d), "rebase: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if target does not exist in .data", {
  d <- data.frame(a = 1, b = 2)
  o <- spcOptions("a", "b", target = "c")
  expect_error(validateSpcOptions(o, d), "target: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if trajectory does not exist in .data", {
  d <- data.frame(a = 1, b = 2)
  o <- spcOptions("a", "b", trajectory = "c")
  expect_error(validateSpcOptions(o, d), "trajectory: 'c' must be a valid column name in the data frame.")
})

test_that("dateField can only appear once per facet", {
  d <- data.frame(a = c(1, 1), b = 1:2, g = 1:2)

  o1 <- spcOptions("b", "a")
  expect_error(validateSpcOptions(o1, d), "duplicate rows found in 'a'")

  o2 <- spcOptions("b", "a", facetField = "g")
  expect_true(validateSpcOptions(o2, d))
})
