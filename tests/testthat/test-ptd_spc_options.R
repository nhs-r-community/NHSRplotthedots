library(testthat)

# ptd_spc_options() ----
test_that("it returns correct data", {
  r <- ptd_spc_options(
    value_field = "value_field",
    date_field = "date_field",
    facet_field = "facet_field",
    rebase = "rebase",
    fix_after_n_points = NULL,
    improvement_direction = "increase",
    target = "target",
    trajectory = "trajectory"
  )

  expect_equal(r$value_field, "value_field")
  expect_equal(r$date_field, "date_field")
  expect_equal(r$facet_field, "facet_field")
  expect_equal(r$rebase, "rebase")
  expect_equal(r$fix_after_n_points, NULL)
  expect_equal(r$improvement_direction, "increase")
  expect_equal(r$target, "target")
  expect_equal(r$trajectory, "trajectory")

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

test_that("rebase is either null, or a scalar character", {
  # this should run without an error
  ptd_spc_options("a", "b", rebase = NULL)
  expect_error(ptd_spc_options("a", "b", rebase = 1), "rebase argument must be a 'character' of length 1.")
  expect_error(ptd_spc_options("a", "b", rebase = c("a", "b")), "rebase argument must be a 'character' of length 1.")
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

test_that("improvement_direction must be one of increase or decrease", {
  ptd_spc_options("a", "b", improvement_direction = "increase")
  ptd_spc_options("a", "b", improvement_direction = "decrease")

  expect_error(
    ptd_spc_options("a", "b", improvement_direction = "a"),
    "'arg' should be one of \"increase\", \"decrease\""
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

test_that("you cannot rebase and fix_after_n_points", {
  expect_error(
    ptd_spc_options("b", "a", rebase = "c", fix_after_n_points = 12),
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
  expect_output(print(r), "--------------------------------")
})

# ptd_validate_spc_options() ----

test_that("options must be created by ptd_spc_options()", {
  expect_error(ptd_validate_spc_options(list(), NULL), "options must be created by ptd_spc_options()")
})

test_that(".data must be a data.frame", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a")

  expect_error(ptd_validate_spc_options(o, NULL), ".data must be a data.frame")
  expect_error(ptd_validate_spc_options(o, 1), ".data must be a data.frame")
  expect_error(ptd_validate_spc_options(o, "a"), ".data must be a data.frame")
  ptd_validate_spc_options(o, d)
})

test_that("it returns an error if value_field does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("x", "b")
  expect_error(ptd_validate_spc_options(o, d), "value_field: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if date_field does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "x")
  expect_error(ptd_validate_spc_options(o, d), "date_field: 'x' must be a valid column name in the data frame.")
})

test_that("it returns an error if facet_field does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", facet_field = "c")
  expect_error(ptd_validate_spc_options(o, d), "facet_field: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if rebase does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", rebase = "c")
  expect_error(ptd_validate_spc_options(o, d), "rebase: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if target does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", target = "c")
  expect_error(ptd_validate_spc_options(o, d), "target: 'c' must be a valid column name in the data frame.")
})

test_that("it returns an error if trajectory does not exist in .data", {
  d <- data.frame(a = Sys.Date(), b = 2)
  o <- ptd_spc_options("b", "a", trajectory = "c")
  expect_error(ptd_validate_spc_options(o, d), "trajectory: 'c' must be a valid column name in the data frame.")
})

test_that("date_field can only appear once per facet", {
  d <- data.frame(a = rep(Sys.Date(), 2), b = 1:2, g = 1:2)

  o1 <- ptd_spc_options("b", "a")
  expect_error(ptd_validate_spc_options(o1, d), "duplicate rows found in 'a'")

  o2 <- ptd_spc_options("b", "a", facet_field = "g")
  expect_true(ptd_validate_spc_options(o2, d))
})

test_that("date_field must be either a Date or POSIXt vector", {
  o <- ptd_spc_options("b", "a")
  ptd_validate_spc_options(o, data.frame(a = Sys.Date(), b = 1))
  ptd_validate_spc_options(o, data.frame(a = Sys.time(), b = 1))

  expect_error(ptd_validate_spc_options(o, data.frame(a = 1, b = 1)),
    "date_field must be a Date or POSIXt vector ('a' is a 'numeric').",
    fixed = TRUE
  )
  expect_error(ptd_validate_spc_options(o, data.frame(a = "a", b = 1)),
    "date_field must be a Date or POSIXt vector ('a' is a 'character').",
    fixed = TRUE
  )
})

test_that("value_field must be a numeric", {
  o <- ptd_spc_options("b", "a")
  ptd_validate_spc_options(o, data.frame(a = Sys.Date(), b = 1))

  expect_error(ptd_validate_spc_options(o, data.frame(a = Sys.Date(), b = "a")),
    "value_field must be a numeric vector ('b' is a 'character').",
    fixed = TRUE
  )
})

test_that("rebase values must be either 0 or 1", {
  # this should work
  o <- ptd_spc_options("b", "a", rebase = "r")
  ptd_validate_spc_options(o, data.frame(a = Sys.Date() + 1:2, b = 1:2, r = c(0, 1)))
  ptd_validate_spc_options(o, data.frame(a = Sys.Date() + 1:2, b = 1:2, r = c(TRUE, FALSE)))

  # this should error
  expect_error(
    ptd_validate_spc_options(o, data.frame(a = Sys.Date() + 1:2, b = 1:2, r = c("a", "b"))),
    "values in the rebase column must either be 0 or 1."
  )
  expect_error(
    ptd_validate_spc_options(o, data.frame(a = Sys.Date() + 1:2, b = 1:2, r = c(2, 3))),
    "values in the rebase column must either be 0 or 1."
  )
})
