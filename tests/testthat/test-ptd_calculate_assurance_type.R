library(testthat)
library(mockery)

test_that("it returns a column of NA's if there is no target", {
  s <- data.frame(f = "no facet")
  attr(s, "options") <- list(
    target = NULL,
    improvement_direction = "increase"
  )

  r <- ptd_calculate_assurance_type(s)

  expect_equal(r$assurance_type, as.character(NA))
})

test_that("it returns correct values for assurance when improvement_direction is increase", { # nolint
  s <- data.frame(
    f = c("a", "b", "c"),
    lpl = c(1, 5, 1),
    mean = c(2, 6, 4),
    upl = c(3, 7, 5),
    target = 4
  )
  attr(s, "options") <- list(
    target = "target",
    improvement_direction = "increase"
  )

  r <- ptd_calculate_assurance_type(s)

  expect_equal(r$assurance_type, c("consistent_fail", "consistent_pass", "inconsistent"))
})

test_that("it returns correct values for assurance when improvement_direction is decrease", { # nolint
  s <- data.frame(
    f = c("a", "b", "c"),
    lpl = c(1, 5, 1),
    mean = c(2, 6, 4),
    upl = c(3, 7, 5),
    target = 4
  )
  attr(s, "options") <- list(
    target = "target",
    improvement_direction = "decrease"
  )

  r <- ptd_calculate_assurance_type(s)

  expect_equal(r$assurance_type, c("consistent_pass", "consistent_fail", "inconsistent"))
})

test_that("it returns correct values for assurance when improvement_direction is neutral", { # nolint
  s <- data.frame(
    f = c("a", "b", "c"),
    lpl = c(1, 5, 1),
    mean = c(2, 6, 4),
    upl = c(3, 7, 5),
    target = 4
  )
  attr(s, "options") <- list(
    target = "target",
    improvement_direction = "neutral"
  )

  r <- ptd_calculate_assurance_type(s)

  expect_equal(r$assurance_type, as.character(rep(NA, 3)))
})

test_that("it returns correct values for assurance when rebase", {
  s <- data.frame(
    f = "a",
    rebase = c(1, 2, 3),
    lpl = c(1, 5, 1),
    mean = c(2, 6, 4),
    upl = c(3, 7, 5),
    target = 4
  )
  attr(s, "options") <- list(
    target = "target",
    improvement_direction = "decrease"
  )

  r1 <- ptd_calculate_assurance_type(s[c(1, 2, 3), ])
  expect_equal(r1$assurance_type, "inconsistent")

  r2 <- ptd_calculate_assurance_type(s[c(2, 3, 1), ])
  expect_equal(r2$assurance_type, "consistent_pass")

  r3 <- ptd_calculate_assurance_type(s[c(3, 1, 2), ])
  expect_equal(r3$assurance_type, "consistent_fail")
})
