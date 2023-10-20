library(testthat)
library(mockery)

set.seed(123)
data <- data.frame(
  x = as.Date("2020-01-01") + 1:20,
  y = rnorm(20),
  rebase = 0,
  target = as.double(NA)
)
spc_options <- list(value_field = "y", date_field = "x", screen_outliers = TRUE)

test_that("it returns a data frame", {
  r <- ptd_spc_standard(data, spc_options)

  expect_s3_class(r, "data.frame")
})

test_that("it returns expected values", {
  r1 <- ptd_spc_standard(data, spc_options)

  expect_snapshot(pillar::glimpse(r1))

  # testing the screen_outliers
  o <- spc_options
  data$y[15] <- 10
  r2 <- ptd_spc_standard(data, o)
  expect_snapshot(pillar::glimpse(r2))

  o$screen_outliers <- FALSE
  r3 <- ptd_spc_standard(data, o)
  expect_snapshot(pillar::glimpse(r3))

  # check that we have different limits for all of the results
  expect_true(r1$lpl[[1]] != r2$lpl[[1]])
  expect_true(r1$lpl[[1]] != r3$lpl[[1]])
  expect_true(r2$lpl[[1]] != r3$lpl[[1]])
  expect_true(r1$upl[[1]] != r2$upl[[1]])
  expect_true(r1$upl[[1]] != r3$upl[[1]])
  expect_true(r2$upl[[1]] != r3$upl[[1]])

  # check that the range in the 3rd results (when screen_outliers = FALSE)
  # is larger than the range in the 2nd results (when screen_outliers = TRUE)
  expect_true(r3$upl[[1]] - r3$lpl[[1]] > r2$upl[[1]] - r2$lpl[[1]])
})

test_that("it sets the trajectory field", {
  o <- spc_options
  o$trajectory <- "t"

  # when trajectory is set, but the column doesn't exist
  msg <- paste0("Trajectory column (", o$trajectory, ") does not exist in .data")
  expect_error(ptd_spc_standard(data, o), msg, fixed = TRUE)

  # when options$trajectory is set
  d <- data
  d$t <- 1:20
  r1 <- ptd_spc_standard(d, o)
  expect_equal(r1$trajectory, 1:20)

  # when options$trajectory is not set
  o$trajectory <- NULL
  r2 <- ptd_spc_standard(data, o)
  expect_equal(r2$trajectory, rep(as.double(NA), 20))
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
  data <- dplyr::mutate(data, rebase = c(rep(0, 10), 1, rep(0, 9)))
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

test_that("it only reports special cause for 2of3 points close to limits when they are on the same side of the mean", { # nolint
  # dataframe of symmetrical data
  dtf <- data.frame(
    data = c(rep(c(1, -1), times = 8)),
    date = as.Date("2020-01-01") + 1:16
  )

  # create 2 successive points close to the limits on opposite sides of the mean
  dtf$data[15] <- 5.5
  dtf$data[16] <- -5.5

  s <- ptd_spc(dtf, value_field = data, date_field = date)

  expect_equal(
    tail(s, 2)$point_type,
    c("common_cause", "common_cause")
  )
})
