library(testthat)
library(mockery)

warning_threshold <- 12
warning_message <- paste0(
  "Some groups have 'n < 12' observations. ",
  "These have trial limits, which will be revised with each additional observation ",
  "until 'n = fix_after_n_points' has been reached."
)

test_that("it chooses warning_threshold from options if no value provided", {
  m <- mock(10, 12)
  stub(ptd_add_short_group_warnings, "getOption", m)

  data <- data.frame(
    f = rep("no facet", times = 10), # no facets
    rebase_group = 0 # no rebase
    # other columns not needed by this function
  )
  o1 <- ptd_add_short_group_warnings(data)
  expect_warning(
    o2 <- ptd_add_short_group_warnings(data),
    warning_message,
    fixed = TRUE
  )

  expect_called(m, 2)
})

test_that("it groups, then ungroups data", {
  m1 <- mock()
  m2 <- mock(data.frame(short_group_warning = FALSE))

  stub(ptd_add_short_group_warnings, "dplyr::group_by", m1)
  stub(ptd_add_short_group_warnings, "dplyr::across", function(x, ...) x)
  stub(ptd_add_short_group_warnings, "dplyr::mutate", function(x, ...) x)
  stub(ptd_add_short_group_warnings, "dplyr::ungroup", m2)

  ptd_add_short_group_warnings(
    data.frame(f = character(), rebase_group = character()),
    warning_threshold
  )

  expect_called(m1, 1)
  expect_call(m1, 1, dplyr::group_by(., dplyr::across(c("f", "rebase_group"))))

  expect_called(m2, 1)
  expect_call(m2, 1, dplyr::ungroup(.))
})

test_that("it adds a column called short_group_warning", {
  data <- data.frame(
    f = rep("no facet", times = 20), # no facets
    rebase_group = 0 # no rebase
    # other columns not needed by this function
  )
  o <- ptd_add_short_group_warnings(data, warning_threshold)
  expect_false(is.null(o$short_group_warning))
})

test_that("it warns when a group is shorter than the warning_threshold", {
  data <- data.frame(
    f = rep("no facet", times = 11), # no facets
    rebase_group = 0 # no rebase
  )
  expect_warning(
    o <- ptd_add_short_group_warnings(data, warning_threshold),
    warning_message,
    fixed = TRUE
  )
  expect_equal(o$short_group_warning, rep(TRUE, 11))
})

test_that("it does not warn when a group is equal to the warning_threshold", {
  data <- data.frame(
    f = rep("no facet", times = 12), # no facets
    rebase_group = 0 # no rebase
  )
  o <- ptd_add_short_group_warnings(data, warning_threshold)
  expect_equal(o$short_group_warning, rep(FALSE, 12))
})

test_that("it handles facets and rebase groups - no warnings", {
  data <- data.frame(
    f = rep(c("a", "b"), each = 40), # facets
    rebase_group = rep(c(0, 1, 2, 3), each = 20) # rebase after 20 points
  )
  o <- ptd_add_short_group_warnings(data, warning_threshold)
  expect_equal(o$short_group_warning, rep(FALSE, 80))
})

test_that("it handles facets and rebase groups - warning on one facet", {
  data <- data.frame(
    f = rep(c("a", "b"), each = 40), # facets
    # facet a has a short group of 5 points
    rebase_group = rep(c(0, 1, 2, 3), times = c(35, 5, 20, 20))
  )
  expect_warning(
    o <- ptd_add_short_group_warnings(data, warning_threshold),
    warning_message,
    fixed = TRUE
  )
  expect_equal(
    o$short_group_warning,
    rep(c(FALSE, TRUE, FALSE, FALSE), times = c(35, 5, 20, 20))
  )
})
