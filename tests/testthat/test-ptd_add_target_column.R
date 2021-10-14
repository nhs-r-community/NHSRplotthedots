library(testthat)
library(mockery)

set.seed(123)
data <- data.frame(
  x = 1:20,
  f = rep(c("a", "b"), each = 10)
)

test_that("it adds a column of NA's if no target is provided", {
  o <- ptd_add_target_column(data, NULL)
  expect_equal(o$target, rep(as.double(NA), 20))
})

test_that("it correctly sets target column when a numeric vector is provided", {
  o <- ptd_add_target_column(data, 0.9)
  expect_equal(o$target, rep(0.9, 20))
})

test_that("it correctly sets target column when a list is provided", {
  o <- ptd_add_target_column(data, list("a" = 0.9, "b" = 0.8))

  expect_equal(o$target, rep(c(0.9, 0.8), each = 10))
})
