library(testthat)

# ptd_spc_options() ----
test_that("it returns correct data", {
  r <- ptd_spc_options(
    value_field = "value_field",
    date_field = "date_field",
    facet_field = "facet_field",
    rebase = as.Date("2020-01-01"),
    fix_after_n_points = NULL,
    improvement_direction = "increase",
    target = "target",
    trajectory = "trajectory",
    screen_outliers = TRUE
  )

  expect_equal(r$value_field, "value_field")
  expect_equal(r$date_field, "date_field")
  expect_equal(r$facet_field, "facet_field")
  expect_equal(r$rebase, as.Date("2020-01-01"))
  expect_equal(r$fix_after_n_points, NULL)
  expect_equal(r$improvement_direction, "increase")
  expect_equal(r$target, "target")
  expect_equal(r$trajectory, "trajectory")
  expect_equal(r$screen_outliers, TRUE)

  expect_s3_class(r, "ptd_spc_options")
})

test_that("value_field can only be a scalar character", {
  expect_error(ptd_spc_options(1), "value_field argument must be a 'character' of length 1.")
  expect_error(ptd_spc_options(c("a", "b")), "value_field argument must be a 'character' of length 1.")
})

test_that("date_field can only be a scalar character", {
  expect_error(ptd_spc_options("a", 1), "date_field argument must be a 'character' of length 1.")
  expect_error(ptd_spc_options("a", c("a", "b")), "date_field argument must be a 'character' of length 1.")
})

test_that("facet_field is either null, or a scalar character", {
  # this should run without an error
  ptd_spc_options("a", "b", facet_field = NULL)
  expect_error(
    ptd_spc_options("a", "b",
      facet_field = 1
    ),
    "facet_field argument must be a 'character' of length 1."
  )
  expect_error(
    ptd_spc_options("a", "b",
      facet_field = c("a", "b")
    ),
    "facet_field argument must be a 'character' of length 1."
  )
})

test_that("rebase is either null, a date, or a named list of dates", {
  # this should run without an error
  ptd_spc_options("a", "b", rebase = NULL)
  ptd_spc_options("a", "b", rebase = as.Date("2020-01-01"))
  ptd_spc_options("a", "b", rebase = list("a" = as.Date("2020-01-01")), facet_field = "a")

  # these will cause an error
  em <- "rebase argument must be a date vector, or a named list of date vectors."
  expect_error(ptd_spc_options("a", "b", rebase = 1), em)
  expect_error(ptd_spc_options("a", "b", rebase = c("a", "b")), em)
  expect_error(ptd_spc_options("a", "b", rebase = list("a" = as.Date("2020-01-01"), b = "a")), em)
  expect_error(ptd_spc_options("a", "b", rebase = list(as.Date("2020-01-01"))), em)
})

test_that("rebase must be a date vector if facet_field is not set", {
  em <- "rebase must be a date vector if facet_field is not set"
  expect_error(ptd_spc_options("a", "b", rebase = list("a" = Sys.Date())), em)
})

test_that("fix_after_n_points must be a single numeric that is greater than or equal to 12.", {
  expect_error(
    ptd_spc_options("a", "b", fix_after_n_points = "a"),
    "fix_after_n_points must be a single numeric that is greater than or equal to 12."
  )
  expect_error(
    ptd_spc_options("a", "b", fix_after_n_points = c(15, 20)),
    "fix_after_n_points must be a single numeric that is greater than or equal to 12."
  )
  expect_error(
    ptd_spc_options("a", "b", fix_after_n_points = 11),
    "fix_after_n_points must be a single numeric that is greater than or equal to 12."
  )
  ptd_spc_options("a", "b", fix_after_n_points = 12)
})

test_that("improvement_direction defaults to increase", {
  o <- ptd_spc_options("a", "b")
  expect_equal(o$improvement_direction, "increase")
})

test_that("improvement_direction must be one of increase, neutral, or decrease", {
  ptd_spc_options("a", "b", improvement_direction = "increase")
  ptd_spc_options("a", "b", improvement_direction = "neutral")
  ptd_spc_options("a", "b", improvement_direction = "decrease")

  expect_error(
    ptd_spc_options("a", "b", improvement_direction = "a"),
    "'arg' should be one of \"increase\", \"neutral\", \"decrease\""
  )
})

test_that("target is either null, or a scalar character", {
  # this should run without an error
  ptd_spc_options("a", "b", target = NULL)
  expect_error(ptd_spc_options("a", "b", target = 1), "target argument must be a 'character' of length 1.")
  expect_error(ptd_spc_options("a", "b", target = c("a", "b")), "target argument must be a 'character' of length 1.")
})

test_that("trajectory is either null, or a scalar character", {
  # this should run without an error
  ptd_spc_options("a", "b", trajectory = NULL)
  expect_error(
    ptd_spc_options("a", "b", trajectory = 1),
    "trajectory argument must be a 'character' of length 1."
  )
  expect_error(
    ptd_spc_options("a", "b", trajectory = c("a", "b")),
    "trajectory argument must be a 'character' of length 1."
  )
})

test_that("screen_outliers must be a scalar logical", {
  # this should run without an error
  ptd_spc_options("a", "b", screen_outliers = TRUE)
  ptd_spc_options("a", "b", screen_outliers = FALSE)

  # this should error
  em <- "screen_outliers must either `TRUE` or `FALSE`."
  expect_error(
    ptd_spc_options("a", "b", screen_outliers = c(TRUE, FALSE)),
    em
  )
  expect_error(
    ptd_spc_options("a", "b", screen_outliers = "TRUE"),
    em
  )
})

test_that("you cannot rebase and fix_after_n_points", {
  expect_error(
    ptd_spc_options("b", "a", rebase = as.Date("2020-04-01"), fix_after_n_points = 12),
    "cannot rebase and fix_after_n_points"
  )
})

# print() ----

test_that("printing output", {
  r <- ptd_spc_options("hello", "world")
  expect_output(print(r), "Plot the Dots SPC options:")
  expect_output(print(r), "================================")
  expect_output(print(r), "value_field:.*'hello'")
  expect_output(print(r), "date_field:.*'world'")
  expect_output(print(r), "facet_field:.*not set")
  expect_output(print(r), "rebase:.*not set")
  expect_output(print(r), "fix_after_n_points:.*not set")
  expect_output(print(r), "improvement_direction:.*not set")
  expect_output(print(r), "target:.*not set")
  expect_output(print(r), "trajectory:.*not set")
  expect_output(print(r), "screen_outliers:.*'TRUE'")
  expect_output(print(r), "--------------------------------")
})
