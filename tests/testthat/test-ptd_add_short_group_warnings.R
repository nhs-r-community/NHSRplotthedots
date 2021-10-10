warning_threshold <- 12

test_that("it adds a column called short_group_warning", {
  data <- data.frame(
    f = rep("no facet", times = 20), # no facets
    rebase_group = 0 # no rebase
    # other columns not needed by this function
  )
  o <- ptd_add_short_group_warnings(data, warning_threshold)
  expect_equal("short_group_warning" %in% colnames(o), TRUE)
})

test_that("it warns when a group is shorter than the warning_threshold", {
  data <- data.frame(
    f = rep("no facet", times = 11), # no facets
    rebase_group = 0 # no rebase
  )
  o <- ptd_add_short_group_warnings(data, warning_threshold)
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
    rebase_group = rep(c(0,1,2,3), each = 20) # rebase after 20 points
  )
  o <- ptd_add_short_group_warnings(data, warning_threshold)
  expect_equal(o$short_group_warning, rep(FALSE, 80))
})

test_that("it handles facets and rebase groups - warning on one facet", {
  data <- data.frame(
  f = rep(c("a", "b"), each = 40), # facets
  rebase_group = rep(c(0,1,2,3), times = c(35,5,20,20)) # facet a has a short group of 5 points
)
  o <- ptd_add_short_group_warnings(data, warning_threshold)
  expect_equal(
      o$short_group_warning,
      rep(c(FALSE, TRUE, FALSE, FALSE), times = c(35, 5, 20, 20))
    )
})