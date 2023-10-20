library(testthat)
library(mockery)

# ptd_calculate_point_type() ----
test_that("it calls functions as expected (no facet groups)", {
  a <- data.frame(
    f = rep(1, 4),
    rebase_group = rep(0, 4),
    relative_to_mean = rep(1, 4),
    close_to_limits = rep(2, 4),
    y = rep(3, 4),
    outside_limits = rep(4, 4)
  )

  m1 <- mock("ptd_special_cause_type")
  m2 <- mock("point_type")

  stub(ptd_calculate_point_type, "ptd_special_cause_type", m1)
  stub(ptd_calculate_point_type, "ptd_point_type", m2)

  ptd_calculate_point_type(a, "improvement_direction")

  expect_called(m1, 1)
  expect_called(m2, 1)

  expect_args(m1, 1, a$y, a$relative_to_mean, a$close_to_limits, a$outside_limits)
  expect_args(
    m2, 1,
    rep("ptd_special_cause_type", 4),
    "improvement_direction"
  )
})

test_that("it calls functions as expected (with facet groups)", {
  a <- data.frame(
    f = 1:4,
    rebase_group = rep(0, 4),
    relative_to_mean = rep(1, 4),
    close_to_limits = rep(2, 4),
    y = rep(3, 4),
    outside_limits = rep(4, 4)
  )

  m1 <- mock("ptd_special_cause_flag", cycle = TRUE)
  m2 <- mock("ptd_point_type", cycle = TRUE)

  stub(ptd_calculate_point_type, "ptd_special_cause_type", m1)
  stub(ptd_calculate_point_type, "ptd_point_type", m2)

  ptd_calculate_point_type(a, 1)

  expect_called(m1, 4)
  expect_called(m2, 4)
})

test_that("it returns the mutated data", {
  d <- data.frame(f = 1, rebase_group = 2)
  stub(ptd_calculate_point_type, "dplyr::mutate", "mutate")
  stub(ptd_calculate_point_type, "dplyr::ungroup", "ungroup")
  stub(ptd_calculate_point_type, "ptd_special_cause_type", "ptd_special_cause_type")
  a <- ptd_calculate_point_type(d, 1)

  expect_equal(a, "ungroup")
})

test_that("it groups and ungroups the data", {
  d <- data.frame(f = 1, rebase_group = 0)

  stub(ptd_calculate_point_type, "dplyr::mutate", function(x, ...) x)
  stub(ptd_calculate_point_type, "dplyr::ungroup", identity)
  stub(ptd_calculate_point_type, "ptd_special_cause_type", "ptd_special_cause_type")

  a <- ptd_calculate_point_type(d, 1)

  expect_equal(dplyr::groups(a), list(as.symbol("f"), as.symbol("rebase_group")))
})

# ptd_seven_point_one_side_mean() ----
test_that("ptd_seven_point_one_side_mean works as expected", {
  expect_equal(ptd_seven_point_one_side_mean(numeric()), numeric())
  expect_equal(
    ptd_seven_point_one_side_mean(rep(1, 6)),
    c(0, 0, 0, 0, 0, 0)
  )
  expect_equal(
    ptd_seven_point_one_side_mean(c(rep(1, 6), -1)),
    c(0, 0, 0, 0, 0, 0, 0)
  )
  expect_equal(
    ptd_seven_point_one_side_mean(c(rep(1, 8), -1)),
    c(0, 0, 0, 0, 0, 0, 1, 1, 0)
  )
  expect_equal(
    ptd_seven_point_one_side_mean(c(rep(-1, 8), 1)),
    c(0, 0, 0, 0, 0, 0, 1, 1, 0)
  )
})

# ptd_part_of_seven_trend() ----
test_that("ptd_part_of_seven_trend works as expected", {
  expect_equal(ptd_part_of_seven_trend(numeric()), numeric())
  expect_equal(
    ptd_part_of_seven_trend(rep(0, 6)),
    c(0, 0, 0, 0, 0, 0)
  )

  expect_equal(
    ptd_part_of_seven_trend(c(rep(0, 6), 1, 0)),
    c(rep(1, 7), 0)
  )

  expect_equal(
    ptd_part_of_seven_trend(c(rep(0, 7), 1, 0)),
    c(0, rep(1, 7), 0)
  )

  expect_equal(
    ptd_part_of_seven_trend(c(rep(0, 6), -1, 0)),
    c(rep(1, 7), 0)
  )

  expect_equal(
    ptd_part_of_seven_trend(c(rep(0, 7), -1, 0)),
    c(0, rep(1, 7), 0)
  )
})

# ptd_seven_point_trend() ----
test_that("ptd_seven_point_trend works as expected", {
  expect_equal(ptd_seven_point_trend(numeric()), numeric())
  expect_equal(
    ptd_seven_point_trend(1:6),
    c(0, 0, 0, 0, 0, 0)
  )

  a <- ptd_seven_point_trend(c(1:3, 3:6))
  expect_equal(a, c(0, 0, 0, 0, 0, 0, 0))

  b <- ptd_seven_point_trend(1:7)
  expect_equal(b, c(0, 0, 0, 0, 0, 0, 1))

  d <- ptd_seven_point_trend(c(2, 1:7, 3))
  expect_equal(d, c(0, 0, 0, 0, 0, 0, 0, 1, 0))

  e <- ptd_seven_point_trend(7:1)
  expect_equal(e, c(0, 0, 0, 0, 0, 0, -1))

  f <- ptd_seven_point_trend(c(2, 7:1, 3))
  expect_equal(f, c(0, 0, 0, 0, 0, 0, 0, -1, 0))
})

# ptd_two_in_three() ----
test_that("ptd_two_in_three works as expected", {
  a <- ptd_two_in_three(numeric())
  expect_equal(a, numeric())

  b <- ptd_two_in_three(numeric(3), numeric(1))
  expect_equal(b, numeric(3))

  d <- ptd_two_in_three(c(1, 0, 1), c(1, 1, 1))
  e <- ptd_two_in_three(c(0, 1, 1), c(1, 1, 1))
  f <- ptd_two_in_three(c(1, 1, 0), c(1, 1, 1))
  expect_equal(d, e)
  expect_equal(d, f)
  expect_equal(d, rep(1, 3))

  g <- ptd_two_in_three(c(0, 0, 1, 0, 1, 0, 0), c(1, 1, 1, 1, 1, 1, 1))
  expect_equal(g, c(0, 0, 1, 1, 1, 0, 0))

  h <- ptd_two_in_three(c(0, 0, 1, 1, 0, 0, 0), c(1, 1, 1, 1, 1, 1, 1))
  expect_equal(h, c(0, 1, 1, 1, 1, 0, 0))
})

test_that("ptd_two_in_three works as expected when points are on opposite sides of the mean", { # nolint
  d <- ptd_two_in_three(c(1, 0, 1), c(1, -1, 1))
  e <- ptd_two_in_three(c(0, 1, 1), c(1, -1, 1))
  f <- ptd_two_in_three(c(1, 1, 0), c(1, -1, 1))
  expect_equal(d, rep(0, 3))
  expect_equal(e, rep(0, 3))
  expect_equal(f, rep(0, 3))

  g <- ptd_two_in_three(c(0, 0, 1, 0, 1, 0, 0), c(1, -1, 1, -1, 1, -1, 1))
  expect_equal(g, rep(0, 7))

  h <- ptd_two_in_three(c(0, 0, 1, 1, 0, 0, 0), c(-1, 1, -1, 1, -1, 1, -1))
  expect_equal(h, rep(0, 7))
})

# ptd_part_of_two_in_three() ----
test_that("ptd_part_of_two_in_three works as expected", {
  av <- numeric()
  artm <- numeric() # relative to mean column
  at <- ptd_two_in_three(av, artm)
  aa <- ptd_part_of_two_in_three(at, av)
  expect_equal(aa, av)

  bv <- numeric(3)
  brtm <- numeric(1) # relative to mean column
  bt <- ptd_two_in_three(bv, brtm)
  ba <- ptd_part_of_two_in_three(bv, bt)
  expect_equal(ba, bv)

  cv <- c(1, 0, 1)
  crtm <- c(1, 1, 1) # relative to mean column
  ct <- ptd_two_in_three(cv, crtm)
  ca <- ptd_part_of_two_in_three(ct, cv)
  expect_equal(ca, cv)

  dv <- c(0, 1, 1)
  drtm <- c(1, 1, 1) # relative to mean column
  dt <- ptd_two_in_three(dv, drtm)
  da <- ptd_part_of_two_in_three(dt, dv)
  expect_equal(da, dv)

  ev <- c(1, 1, 0)
  ertm <- c(1, 1, 1) # relative to mean column
  et <- ptd_two_in_three(ev, ertm)
  ea <- ptd_part_of_two_in_three(et, ev)
  expect_equal(ea, ev)

  fv <- c(0, 0, 1, 0, 1, 0, 0)
  frtm <- c(1, 1, 1, 1, 1, 1, 1) # relative to mean column
  ft <- ptd_two_in_three(fv, frtm)
  fa <- ptd_part_of_two_in_three(ft, fv)
  expect_equal(fa, fv)

  gv <- c(0, 0, 1, 1, 0, 0, 0)
  grtm <- c(1, 1, 1, 1, 1, 1, 1) # relative to mean column
  gt <- ptd_two_in_three(gv, grtm)
  ga <- ptd_part_of_two_in_three(gv, gv)
  expect_equal(ga, gv)

  hv <- c(1, 0, 0, 1)
  hrtm <- c(1, 1, 1, 1) # relative to mean column
  ht <- ptd_two_in_three(hv, hrtm)
  ha <- ptd_part_of_two_in_three(ht, hv)
  expect_equal(ha, c(0, 0, 0, 0))

  iv <- c(1, 0, 0, 1, 1)
  irtm <- c(1, 1, 1, 1, 1) # relative to mean column
  it <- ptd_two_in_three(iv, irtm)
  ia <- ptd_part_of_two_in_three(it, iv)
  expect_equal(ia, c(0, 0, 0, 1, 1))
})

# ptd_special_cause_type() ----
test_that("ptd_special_cause_type works as expected", {
  # there are 7 possible inputs that result in a 1 result, and 1 input that results in a 0.
  # we can mock the functions that are called and return results that can test these cases
  m1 <- mock("sevenPointOneSideOfMean")
  m2 <- mock("sevenPointTrend")
  # partOfSevenTrend: this is called twice
  m3 <- mock(
    c(1, -1, 0, 0, 0, 0, 0, 0),
    c(0, 0, 1, -1, 0, 0, 0, 0)
  )
  m4 <- mock("twoInThree")
  m5 <- mock(c(0, 0, 0, 0, 1, 0, 0, 0)) # part_of_two_in_three

  # tie these areas up with the variable names in function that you want to stub.
  stub(ptd_special_cause_type, "ptd_seven_point_one_side_mean", m1) # -names
  stub(ptd_special_cause_type, "ptd_seven_point_trend", m2) # - names
  stub(ptd_special_cause_type, "ptd_part_of_seven_trend", m3) #- values
  stub(ptd_special_cause_type, "ptd_two_in_three", m4) # -value
  stub(ptd_special_cause_type, "ptd_part_of_two_in_three", m5) # -values

  a <- ptd_special_cause_type(
    1:8,
    c(rep(1, 4), rep(-1, 4)),
    "relative_to_mean",
    c(0, 0, 0, 0, 0, 1, -1, 0)
  )

  expect_equal(
    a,
    c(
      "7 Point Trend (Increasing)",
      "7 Point Trend (Increasing)",
      "7 Points Above CL",
      "7 Points Above CL",
      "2 in 3 Below CL",
      "Below LCL",
      "Below LCL",
      "Common Cause"
    )
  )
})
