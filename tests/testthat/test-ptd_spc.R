library(testthat)
library(mockery)

set.seed(123)
data <- data.frame(
  x = as.Date("2020-01-01") + 1:20,
  y = rnorm(20)
)

spc_options <- list(
  value_field = "a",
  date_field = "b",
  facet_field = "c",
  rebase = "d",
  fix_after_n_points = "e",
  improvement_direction = "f",
  target = "g",
  trajectory = "h"
)

# ptd_spc() ----

test_that("it throws an error if .data is not a data.frame", {
  expect_error(ptd_spc("x", "a", "b"), "ptd_spc: .data must be a data.frame")
})

test_that("it returns a ptd_spc_df object", {
  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) {
    x$rebase <- 0
    x
  })
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) {
    x$short_group_warning <- FALSE
    x
  })
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) {
    x$target <- as.double(NA)
    x
  })

  s <- ptd_spc(data, "y", "x")

  expect_s3_class(s, c("ptd_spc_df", "data.frame"))
})

test_that("it has options as an attribute, created by ptd_spc_options", {
  m <- mock(spc_options)

  stub(ptd_spc, "ptd_spc_options", m)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) {
    x$rebase <- 0
    x
  })
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) {
    x$short_group_warning <- FALSE
    x
  })
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) {
    x$target <- as.double(NA)
    x
  })

  s <- ptd_spc(data, "a", "b", "c", "d", "e", "f", "g", "h", "i")
  o <- attr(s, "options")

  expect_equal(o, spc_options)
  expect_called(m, 1)
  expect_args(m, 1, "a", "b", "c", "d", "e", "f", "g", "h", "i")
})

test_that("it validates the options", {
  m <- mock(TRUE)

  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", m)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) {
    x$rebase <- 0
    x
  })
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) {
    x$short_group_warning <- FALSE
    x
  })
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) {
    x$target <- as.double(NA)
    x
  })

  s <- ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, spc_options, data)
})

test_that("it calls ptd_spc_standard", {
  m <- mock("ptd_spc_standard")

  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", m)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) x)
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) x)
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) x)

  s <- ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, spc_options)
})

test_that("it calls ptd_calculate_point_type (increase)", {
  m <- mock("ptd_calculate_point_type")

  spc_options$improvement_direction <- "increase"
  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", m)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) x)
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) x)
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) x)

  ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, 1)
})

test_that("it calls ptd_calculate_point_type (decrease)", {
  m <- mock("ptd_calculate_point_type")

  spc_options$improvement_direction <- "decrease"
  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", m)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) x)
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) x)
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) x)

  ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data, -1)
})

test_that("it converts date_field to POSIXct", {
  m <- mock("to_datetime")

  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", m)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) x)
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) x)
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) x)

  ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data$x)
})

test_that("it calls ptd_add_rebase_column", {
  m <- mock("ptd_add_rebase_column")

  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", m)
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) x)
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) x)

  ptd_spc(data, "y", "x", facet_field = "f", rebase = "r")

  expect_called(m, 1)
  expect_args(m, 1, data, "x", "f", "r")
})

test_that("it calls ptd_add_short_group_warnings", {
  m <- mock("ptd_add_short_group_warnings")

  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) x)
  stub(ptd_spc, "ptd_add_short_group_warnings", m)
  stub(ptd_spc, "ptd_add_target_column", function(x, ...) x)

  ptd_spc(data, "y", "x")

  expect_called(m, 1)
  expect_args(m, 1, data)
})

test_that("it calls ptd_add_target_column", {
  m <- mock("ptd_add_target_column")

  stub(ptd_spc, "ptd_spc_options", spc_options)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_rebase_column", function(x, ...) x)
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) x)
  stub(ptd_spc, "ptd_add_target_column", m)

  ptd_spc(data, "y", "x", target = "t")

  expect_called(m, 1)
  expect_args(m, 1, data, "t")
})

test_that("it accepts nse arguments as well as string", {
  m <- mock(spc_options, spc_options)

  stub(ptd_spc, "ptd_spc_options", m)
  stub(ptd_spc, "ptd_validate_spc_options", TRUE)
  stub(ptd_spc, "ptd_spc_standard", function(x, ...) x)
  stub(ptd_spc, "ptd_calculate_point_type", function(x, ...) x)
  stub(ptd_spc, "to_datetime", function(x, ...) x)
  stub(ptd_spc, "ptd_add_short_group_warnings", function(x, ...) x)

  r <- ptd_rebase()
  t <- ptd_target()

  s1 <- ptd_spc(data, vf, df)
  s2 <- ptd_spc(
    data,
    value_field = vf,
    date_field = df,
    facet_field = ff,
    rebase = r,
    target = t,
    trajectory = tr
  )

  expect_called(m, 2)
  expect_args(m, 1, "vf", "df", NULL, NULL, NULL, "increase", NULL, NULL, TRUE)
  expect_args(m, 2, "vf", "df", "ff", r, NULL, "increase", t, "tr", TRUE)
})

# print() ----

test_that("it calls plot", {
  m <- mock("plot")
  stub(print.ptd_spc_df, "plot", m)

  s <- ptd_spc(data, "y", "x")
  o <- capture_output(print(s))

  expect_called(m, 1)
  expect_args(m, 1, s)

  # check that print is called on the return of plot: this is a mocked output
  expect_equal(o, "[1] \"plot\"")
})

test_that("it calls print", {
  m <- mock("print")
  stub(print.ptd_spc_df, "plot", "plot")
  stub(print.ptd_spc_df, "print", m)

  s <- ptd_spc(data, "y", "x")
  o <- capture_output(print(s))

  expect_called(m, 1)
  expect_args(m, 1, "plot")
})

# summary() ----
test_that("it outputs expected content", {
  d <- data

  d$facet <- rep(c(0, 1), each = 10)

  d$target <- 1

  stub(ptd_spc, "ptd_assurance_type", "assurance_type")

  s1 <- ptd_spc(d, "y", "x")
  expect_snapshot_output(summary(s1))

  s2 <- ptd_spc(d, "y", "x", rebase = as.Date("2020-01-01"))
  expect_snapshot_output(summary(s2))

  suppressWarnings(
    s3 <- ptd_spc(d, "y", "x", facet_field = "facet")
  )
  expect_snapshot_output(summary(s3))

  suppressWarnings(
    s4 <- ptd_spc(d, "y", "x", rebase = as.Date("2020-01-01"), facet_field = "facet")
  )
  expect_snapshot_output(summary(s4))

  s5 <- ptd_spc(d, "y", "x", target = "target")
  expect_snapshot_output(summary(s5))
})
