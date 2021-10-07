library(testthat)
library(mockery)

set.seed(123)
data <- data.frame(
  x = as.Date("2020-01-01") + 1:20,
  y = rnorm(20),
  rebase = 0,
  t = 1:20
)
spc_options <- list(value_field = "y", date_field = "x", screen_outliers = TRUE)

test_that("it returns a data frame", {
  r <- ptd_spc_standard(data, spc_options)

  expect_s3_class(r, "data.frame")
})

test_that("it returns expected values", {
  r1 <- ptd_spc_standard(data, spc_options)

  expect_snapshot(dplyr::glimpse(r1))

  # testing the screen_outliers
  o <- spc_options
  data$y[15] <- 10
  r2 <- ptd_spc_standard(data, o)
  expect_snapshot(dplyr::glimpse(r2))

  o$screen_outliers <- FALSE
  r3 <- ptd_spc_standard(data, o)
  expect_snapshot(dplyr::glimpse(r3))

  # check that we have different limits for all of the results
  expect_true(r1$lpl[[1]] != r2$lpl[[1]])
  expect_true(r1$lpl[[1]] != r3$lpl[[1]])
  expect_true(r2$lpl[[1]] != r3$lpl[[1]])
  expect_true(r1$upl[[1]] != r2$upl[[1]])
  expect_true(r1$upl[[1]] != r3$upl[[1]])
  expect_true(r2$upl[[1]] != r3$upl[[1]])

  # check that the range in the 3rd results (when screen_outliers = FALSE) is larger than the range in the 2nd results
  # (when screen_outliers = TRUE)
  expect_true(r3$upl[[1]] - r3$lpl[[1]] > r2$upl[[1]] - r2$lpl[[1]])
})

test_that("it sets the trajectory field", {
  o <- spc_options
  o$trajectory <- "t"
  # when options$trajectory is set
  r1 <- ptd_spc_standard(data, o)
  expect_equal(r1$trajectory, 1:20)

  # when options$trajectory is not set
  o$trajectory <- NULL
  r2 <- ptd_spc_standard(data, o)
  expect_equal(r2$trajectory, rep(as.double(NA), 20))
})

test_that("it sets the target field", {
  o <- spc_options
  o$target <- "t"
  # when options$target is set
  r1 <- ptd_spc_standard(data, o)
  expect_equal(r1$target, 1:20)

  # when options$target is not set
  o$target <- NULL
  r2 <- ptd_spc_standard(data, o)
  expect_equal(r2$target, rep(as.double(NA), 20))
})

test_that("it creates the pseudo facet column if no facet_field is set", {
  # when options$target is not set
  r <- ptd_spc_standard(data, spc_options)
  expect_equal(r$f, rep("no facet", 20))
})

test_that("it sets the rebase_group field", {
  o <- spc_options
  # when rebase is not set
  r1 <- ptd_spc_standard(data, o)
  expect_equal(r1$rebase_group, rep(0, 20))

  # when rebase is set
  o$rebase <- "rebase"
  data <- mutate(data, rebase = c(rep(0, 10), 1, rep(0, 9)))
  r2 <- ptd_spc_standard(data, o)
  expect_equal(r2$rebase_group, c(rep(0, 10), rep(1, 10)))
})

test_that("setting fix_after_n_points changes the calculations", {
  o <- spc_options
  s0 <- ptd_spc_standard(data, o)

  o$fix_after_n_points <- 12
  s1 <- ptd_spc_standard(data, o)

  expect_true(s1$lpl[[1]] != s0$lpl[[1]])
  expect_true(s1$upl[[1]] != s0$upl[[1]])
})
