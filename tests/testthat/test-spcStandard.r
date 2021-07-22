library(testthat)
library(mockery)

test_that("it returns a data frame", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  r <- spcStandard(data, list(valueField = "y", dateField = "x"))

  expect_s3_class(r, "data.frame")
})

test_that("it returns expected values", {
  set.seed(123)
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  r <- spcStandard(data, list(valueField = "y", dateField = "x"))

  expect_snapshot(r)
})

test_that("it sets the trajectory field", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  # when options$trajectory is set
  r1 <- spcStandard(data, list(valueField = "y", dateField = "x", trajectory = "t"))
  expect_equal(r1$trajectory, 1:20)

  # when options$trajectory is not set
  r2 <- spcStandard(data, list(valueField = "y", dateField = "x"))
  expect_equal(r2$trajectory, rep(as.double(NA), 20))
})

test_that("it sets the target field", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  # when options$target is set
  r1 <- spcStandard(data, list(valueField = "y", dateField = "x", target = "t"))
  expect_equal(r1$target, 1:20)

  # when options$target is not set
  r2 <- spcStandard(data, list(valueField = "y", dateField = "x"))
  expect_equal(r2$target, rep(as.double(NA), 20))
})

test_that("it creates the pseudo facet column if no facetField is set", {
  data <- data.frame(x = 1:20, y = rnorm(20))

  # when options$target is not set
  r2 <- spcStandard(data, list(valueField = "y", dateField = "x"))
  expect_equal(r2$f, rep("no facet", 20))
})

test_that("it sets the rebase field", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = c(rep(0, 10), 1, rep(0, 9)))

  # when options$target is set
  r1 <- spcStandard(data, list(valueField = "y", dateField = "x", rebase = "t"))
  expect_equal(r1$rebase, c(rep(0, 10), 1, rep(0, 9)))

  # when options$target is not set
  r2 <- spcStandard(data, list(valueField = "y", dateField = "x"))
  expect_equal(r2$rebase, rep(0, 20))
})

test_that("it raises an error if the rebase field is not 0 or 1", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  # when options$target is set
  expect_error(spcStandard(data, list(valueField = "y", dateField = "x", rebase = "t")),
               "spc: rebase column must define a field containing only 0 or 1 values.")
})

# cannot think of useful tests for lines 66 through 183
