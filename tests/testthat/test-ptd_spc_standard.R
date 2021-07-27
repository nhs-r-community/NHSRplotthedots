library(testthat)
library(mockery)

test_that("it returns a data frame", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  r <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))

  expect_s3_class(r, "data.frame")
})

test_that("it returns expected values", {
  set.seed(123)
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  r <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))

  expect_snapshot(r)
})

test_that("it sets the trajectory field", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  # when options$trajectory is set
  r1 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x", trajectory = "t"))
  expect_equal(r1$trajectory, 1:20)

  # when options$trajectory is not set
  r2 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r2$trajectory, rep(as.double(NA), 20))
})

test_that("it sets the target field", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = 1:20)

  # when options$target is set
  r1 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x", target = "t"))
  expect_equal(r1$target, 1:20)

  # when options$target is not set
  r2 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r2$target, rep(as.double(NA), 20))
})

test_that("it creates the pseudo facet column if no facet_field is set", {
  data <- data.frame(x = 1:20, y = rnorm(20))

  # when options$target is not set
  r2 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r2$f, rep("no facet", 20))
})

test_that("it sets the rebase_group field", {
  data <- data.frame(x = 1:20, y = rnorm(20), t = c(rep(0, 10), 1, rep(0, 9)))

  # when options$target is set
  r1 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x", rebase = "t"))
  expect_equal(r1$rebase_group, c(rep(0, 10), rep(1, 10)))

  # when options$target is not set
  r2 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r2$rebase_group, rep(0, 20))
})

test_that("setting fix_after_n_points changes the calculations", {
  set.seed(123)
  data <- data.frame(x = 1:20, y = rnorm(20))

  s0 <- ptd_spc_standard(data, ptd_spc_options("x", "y"))
  s1 <- ptd_spc_standard(data, ptd_spc_options("x", "y", fix_after_n_points = 12))

  expect_lt(s1$lpl[[1]], s0$lpl[[1]])
  expect_gt(s1$upl[[1]], s0$upl[[1]])
})

# cannot think of useful tests for lines 66 through 183
