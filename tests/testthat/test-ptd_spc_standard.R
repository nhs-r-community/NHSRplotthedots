library(testthat)
library(mockery)

set.seed(123)
data <- data.frame(
  x = as.Date("2020-01-01") + 1:20,
  y = rnorm(20),
  rebase = 0,
  t = 1:20
)

test_that("it returns a data frame", {
  r <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))

  expect_s3_class(r, "data.frame")
})

test_that("it returns expected values", {
  r <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))

  expect_snapshot(r)
})

test_that("it sets the trajectory field", {
  # when options$trajectory is set
  r1 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x", trajectory = "t"))
  expect_equal(r1$trajectory, 1:20)

  # when options$trajectory is not set
  r2 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r2$trajectory, rep(as.double(NA), 20))
})

test_that("it sets the target field", {
  # when options$target is set
  r1 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x", target = "t"))
  expect_equal(r1$target, 1:20)

  # when options$target is not set
  r2 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r2$target, rep(as.double(NA), 20))
})

test_that("it creates the pseudo facet column if no facet_field is set", {
  # when options$target is not set
  r2 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r2$f, rep("no facet", 20))
})

test_that("it sets the rebase_group field", {
  # when rebase is not set
  r1 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r1$rebase_group, rep(0, 20))

  # when rebase is set
  data <- mutate(data, rebase = c(rep(0, 10), 1, rep(0, 9)))
  r2 <- ptd_spc_standard(data, list(value_field = "y", date_field = "x"))
  expect_equal(r2$rebase_group, c(rep(0, 10), rep(1, 10)))
})

test_that("setting fix_after_n_points changes the calculations", {
  s0 <- ptd_spc_standard(data, ptd_spc_options("y", "x"))
  s1 <- ptd_spc_standard(data, ptd_spc_options("y", "x", fix_after_n_points = 12))

  expect_true(s1$lpl[[1]] != s0$lpl[[1]])
  expect_true(s1$upl[[1]] != s0$upl[[1]])
})
