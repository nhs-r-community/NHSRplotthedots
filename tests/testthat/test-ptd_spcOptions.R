library(testthat)

# ptd_spcOptions() ----
test_that("it returns correct data", {
  r <- ptd_spcOptions(
    valueField = "valueField",
    dateField = "dateField",
    facetField = "facetField",
    rebase = "rebase",
    fixAfterNPoints = NULL,
    improvementDirection = "increase",
    target = "target",
    trajectory = "trajectory"
  )

  expect_equal(r$valueField, "valueField")
  expect_equal(r$dateField, "dateField")
  expect_equal(r$facetField, "facetField")
  expect_equal(r$rebase, "rebase")
  expect_equal(r$fixAfterNPoints, NULL)
  expect_equal(r$improvementDirection, "increase")
  expect_equal(r$target, "target")
  expect_equal(r$trajectory, "trajectory")

  expect_s3_class(r, "ptd_spc_options")
})

test_that("valueField can only be a scalar character", {
  expect_error(ptd_spcOptions(1), "valueField argument must be a 'character' of length 1.")
  expect_error(ptd_spcOptions(c("a", "b")), "valueField argument must be a 'character' of length 1.")
})

test_that("dateField can only be a scalar character", {
  expect_error(ptd_spcOptions("a", 1), "dateField argument must be a 'character' of length 1.")
  expect_error(ptd_spcOptions("a", c("a", "b")), "dateField argument must be a 'character' of length 1.")
})

test_that("facetField is either null, or a scalar character", {
  # this should run without an error
  ptd_spcOptions("a", "b", facetField = NULL)
  expect_error(ptd_spcOptions("a", "b", facetField = 1), "facetField argument must be a 'character' of length 1.")
  expect_error(ptd_spcOptions("a", "b", facetField = c("a", "b")), "facetField argument must be a 'character' of length 1.")
})

test_that("rebase is either null, or a scalar character", {
  # this should run without an error
  ptd_spcOptions("a", "b", rebase = NULL)
  expect_error(ptd_spcOptions("a", "b", rebase = 1), "rebase argument must be a 'character' of length 1.")
  expect_error(ptd_spcOptions("a", "b", rebase = c("a", "b")), "rebase argument must be a 'character' of length 1.")
})

test_that("fixAfterNPoints must be a single numeric that is greater than or equal to 12.", {
  expect_error(ptd_spcOptions("a", "b", fixAfterNPoints = "a"),
               "fixAfterNPoints must be a single numeric that is greater than or equal to 12.")
  expect_error(ptd_spcOptions("a", "b", fixAfterNPoints = c(15, 20)),
               "fixAfterNPoints must be a single numeric that is greater than or equal to 12.")
  expect_error(ptd_spcOptions("a", "b", fixAfterNPoints = 11),
               "fixAfterNPoints must be a single numeric that is greater than or equal to 12.")
  ptd_spcOptions("a", "b", fixAfterNPoints = 12)
})

test_that("improvementDirection defaults to increase", {
  o <- ptd_spcOptions("a", "b")
  expect_equal(o$improvementDirection, "increase")
})

test_that("improvementDirection must be one of increase or decrease", {
  ptd_spcOptions("a", "b", improvementDirection = "increase")
  ptd_spcOptions("a", "b", improvementDirection = "decrease")

  expect_error(ptd_spcOptions("a", "b", improvementDirection = "a"),
               "'arg' should be one of \"increase\", \"decrease\"")
})

test_that("target is either null, or a scalar character", {
  # this should run without an error
  ptd_spcOptions("a", "b", target = NULL)
  expect_error(ptd_spcOptions("a", "b", target = 1), "target argument must be a 'character' of length 1.")
  expect_error(ptd_spcOptions("a", "b", target = c("a", "b")), "target argument must be a 'character' of length 1.")
})

test_that("trajectory is either null, or a scalar character", {
  # this should run without an error
  ptd_spcOptions("a", "b", trajectory = NULL)
  expect_error(ptd_spcOptions("a", "b", trajectory = 1), "trajectory argument must be a 'character' of length 1.")
  expect_error(ptd_spcOptions("a", "b", trajectory = c("a", "b")), "trajectory argument must be a 'character' of length 1.")
})

test_that("you cannot rebase and fixAfterNPoints", {
  expect_error(ptd_spcOptions("b", "a", rebase = "c", fixAfterNPoints = 12),
               "cannot rebase and fixAfterNPoints")
})

# print() ----

test_that("printing output", {
  r <- ptd_spcOptions("hello", "world")
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

# ptd_validateSpcOptions() ----

test_that("options must be created by ptd_spcOptions()", {
  expect_error(ptd_validateSpcOptions(list(), NULL), "options must be created by ptd_spcOptions()")
})

test_that(".data must be a data.frame", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spcOptions("b", "a")

  expect_error(ptd_validateSpcOptions(o, NULL), ".data must be a data.frame")
  expect_error(ptd_validateSpcOptions(o, 1),    ".data must be a data.frame")
  expect_error(ptd_validateSpcOptions(o, "a"),  ".data must be a data.frame")
  ptd_validateSpcOptions(o, d)
})

test_that("it returns an error if valueField does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spcOptions("x", "b")
  expect_error(ptd_validateSpcOptions(o, d), "valueField: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if dateField does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spcOptions("b", "x")
  expect_error(ptd_validateSpcOptions(o, d), "dateField: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if facetField does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spcOptions("b", "a", facetField = "c")
  expect_error(ptd_validateSpcOptions(o, d), "facetField: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if rebase does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spcOptions("b", "a", rebase = "c")
  expect_error(ptd_validateSpcOptions(o, d), "rebase: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if target does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spcOptions("b", "a", target = "c")
  expect_error(ptd_validateSpcOptions(o, d), "target: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if trajectory does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spcOptions("b", "a", trajectory = "c")
  expect_error(ptd_validateSpcOptions(o, d), "trajectory: 'c' must be a valid column name in the data frame.")
})

test_that("dateField can only appear once per facet", {
  d <- data.frame(a = rep(Sys.Date(), 2), b = 1:2, g = 1:2)

  o1 <- ptd_spcOptions("b", "a")
  expect_error(ptd_validateSpcOptions(o1, d), "duplicate rows found in 'a'")

  o2 <- ptd_spcOptions("b", "a", facetField = "g")
  expect_true(ptd_validateSpcOptions(o2, d))
})

test_that("dateField must be either a Date or POSIXt vector", {
  o <- ptd_spcOptions("b", "a")
  ptd_validateSpcOptions(o, data.frame(a = Sys.Date(), b = 1))
  ptd_validateSpcOptions(o, data.frame(a = Sys.time(), b = 1))

  expect_error(ptd_validateSpcOptions(o, data.frame(a = 1, b = 1)),
               "dateField must be a Date or POSIXt vector ('a' is a 'numeric').",
               fixed = TRUE)
  expect_error(ptd_validateSpcOptions(o, data.frame(a = "a", b = 1)),
               "dateField must be a Date or POSIXt vector ('a' is a 'character').",
               fixed = TRUE)
})

test_that("valueField must be a numeric", {
  o <- ptd_spcOptions("b", "a")
  ptd_validateSpcOptions(o, data.frame(a = Sys.Date(), b = 1))

  expect_error(ptd_validateSpcOptions(o, data.frame(a = Sys.Date(), b = "a")),
               "valueField must be a numeric vector ('b' is a 'character').",
               fixed = TRUE)
})

test_that("rebase values must be either 0 or 1", {
  # this should work
  o <- ptd_spcOptions("b", "a", rebase = "r")
  ptd_validateSpcOptions(o, data.frame(a = Sys.Date() + 1:2, b = 1:2, r = c(0, 1)))
  ptd_validateSpcOptions(o, data.frame(a = Sys.Date() + 1:2, b = 1:2, r = c(TRUE, FALSE)))

  # this should error
  expect_error(ptd_validateSpcOptions(o, data.frame(a = Sys.Date() + 1:2, b = 1:2, r = c("a", "b"))),
               "values in the rebase column must either be 0 or 1.")
  expect_error(ptd_validateSpcOptions(o, data.frame(a = Sys.Date() + 1:2, b = 1:2, r = c(2, 3))),
               "values in the rebase column must either be 0 or 1.")
})

