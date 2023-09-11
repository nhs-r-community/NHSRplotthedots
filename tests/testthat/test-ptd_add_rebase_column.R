test_that("it adds a column of zeros if no rebase is provided", {
  set.seed(123)
  data <- data.frame(
    x = to_datetime(as.Date("2020-01-01") + rep(1:10, 2)),
    f = rep(c("a", "b"), each = 10)
  )
  o <- ptd_add_rebase_column(data, "x", NULL, NULL)
  expect_equal(o$rebase, rep(0, 20))
})

test_that("it correctly sets rebase column when a date vector is passed", {
  set.seed(123)
  data <- data.frame(
    x = to_datetime(as.Date("2020-01-01") + rep(1:10, 2)),
    f = rep(c("a", "b"), each = 10)
  )
  o <- ptd_add_rebase_column(data, "x", NULL, as.Date(c("2020-01-03", "2020-01-08")))
  expect_equal(o$rebase, rep(c(0, 1, rep(0, 4), 1, rep(0, 3)), 2))
})

test_that("it correctly sets rebase column when a list is passed", {
  set.seed(123)
  data <- data.frame(
    x = to_datetime(as.Date("2020-01-01") + rep(1:10, 2)),
    f = rep(c("a", "b"), each = 10)
  )
  o <- ptd_add_rebase_column(data, "x", "f", list(
    "a" = as.Date("2020-01-03"),
    "b" = as.Date("2020-01-08")
  ))
  expect_equal(o$rebase, c(0, 1, rep(0, 14), 1, rep(0, 3)))
})
