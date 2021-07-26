library(testthat)
library(mockery)

# ptd_calculatePointHighlighting() ----
test_that("it calls functions as expected (no facet groups)", {
  a <- data.frame(
    f = rep(1, 1),
    relativeToMean = rep(1, 4),
    closeToLimits = rep(2, 4),
    y = rep(3, 4),
    outsideLimits = rep(4, 4)
  )

  m1 <- mock("ptd_specialCauseFlag")
  m2 <- mock("pointType")

  stub(ptd_calculatePointHighlighting, "ptd_specialCauseFlag", m1)
  stub(ptd_calculatePointHighlighting, "case_when", m2)

  ptd_calculatePointHighlighting(a, "improvementDirection")

  expect_called(m1, 1)
  expect_called(m2, 1)

  expect_args(m1, 1, a$y, a$relativeToMean, a$closeToLimits, a$outsideLimits)
  expect_call(m2, 1, case_when(
    !specialCauseFlag ~ "common_cause",
    relativeToMean == improvementDirection ~ "special_cause_improvement",
    TRUE ~ "special_cause_concern"
  ))
})

test_that("it calls functions as expected (with facet groups)", {
  a <- data.frame(
    f = 1:4,
    relativeToMean = rep(1, 4),
    closeToLimits = rep(2, 4),
    y = rep(3, 4),
    outsideLimits = rep(4, 4)
  )

  m1 <- mock("ptd_specialCauseFlag", cycle = TRUE)
  m2 <- mock("ptd_pointType", cycle = TRUE)

  stub(ptd_calculatePointHighlighting, "ptd_specialCauseFlag", m1)
  stub(ptd_calculatePointHighlighting, "case_when", m2)

  ptd_calculatePointHighlighting(a, 1)

  expect_called(m1, 4)
  expect_called(m2, 4)
})

test_that("it returns the mutated data", {
  d <- data.frame(f = 1)
  stub(ptd_calculatePointHighlighting, "mutate", "mutate")
  stub(ptd_calculatePointHighlighting, "ungroup", "ungroup")

  a <- ptd_calculatePointHighlighting(d, 1)

  expect_equal(a, "ungroup")
})

test_that("it groups and ungroups the data", {
  d <- data.frame(f = 1)

  stub(ptd_calculatePointHighlighting, "mutate", function(x, ...) x)
  stub(ptd_calculatePointHighlighting, "ungroup", identity)

  a <- ptd_calculatePointHighlighting(d, 1)

  expect_equal(groups(a), list(as.symbol("f")))
})

# ptd_sevenPointOneSideOfMean() ----
test_that("sevenPointOneSideOfMean works as expected", {
  expect_equal(ptd_sevenPointOneSideOfMean(numeric()), numeric())
  expect_equal(
    ptd_sevenPointOneSideOfMean(rep(1, 6)),
    c(0, 0, 0, 0, 0, 0)
  )
  expect_equal(
    ptd_sevenPointOneSideOfMean(c(rep(1, 6), -1)),
    c(0, 0, 0, 0, 0, 0, 0)
  )
  expect_equal(
    ptd_sevenPointOneSideOfMean(c(rep(1, 8), -1)),
    c(0, 0, 0, 0, 0, 0, 1, 1, 0)
  )
  expect_equal(
    ptd_sevenPointOneSideOfMean(c(rep(-1, 8), 1)),
    c(0, 0, 0, 0, 0, 0, 1, 1, 0)
  )
})

# ptd_partOfSevenTrend() ----
test_that("partOfSevenTrend works as expected", {
  expect_equal(ptd_partOfSevenTrend(numeric()), numeric())
  expect_equal(
    ptd_partOfSevenTrend(rep(0, 6)),
    c(0, 0, 0, 0, 0, 0)
  )

  a <- ptd_sevenPointOneSideOfMean(c(rep(1, 6), -1))
  expect_equal(
    ptd_partOfSevenTrend(a),
    c(0, 0, 0, 0, 0, 0, 0)
  )

  b <- ptd_sevenPointOneSideOfMean(c(-1, rep(1, 8), -1))
  expect_equal(
    ptd_partOfSevenTrend(b),
    c(0, 1, 1, 1, 1, 1, 1, 1, 1, 0)
  )

  c <- ptd_sevenPointOneSideOfMean(c(1, rep(-1, 8), 1))
  expect_equal(
    ptd_partOfSevenTrend(c),
    c(0, 1, 1, 1, 1, 1, 1, 1, 1, 0)
  )
})

# ptd_sevenPointTrend() ----
test_that("sevenPointTrend works as expected", {
  expect_equal(ptd_sevenPointTrend(numeric()), numeric())
  expect_equal(
    ptd_sevenPointTrend(1:6),
    c(0, 0, 0, 0, 0, 0)
  )

  a <- ptd_sevenPointTrend(c(1:3, 3:6))
  expect_equal(a, c(0, 0, 0, 0, 0, 0, 0))

  b <- ptd_sevenPointTrend(1:7)
  expect_equal(b, c(0, 0, 0, 0, 0, 0, 1))

  c <- ptd_sevenPointTrend(c(2, 1:7, 3))
  expect_equal(c, c(0, 0, 0, 0, 0, 0, 0, 1, 0))

  d <- ptd_sevenPointTrend(7:1)
  expect_equal(d, c(0, 0, 0, 0, 0, 0, -1))

  e <- ptd_sevenPointTrend(c(2, 7:1, 3))
  expect_equal(e, c(0, 0, 0, 0, 0, 0, 0, -1, 0))
})

# ptd_twoInThree() ----
test_that("twoInThree works as expected", {
  a <- ptd_twoInThree(numeric())
  expect_equal(a, numeric())

  b <- ptd_twoInThree(numeric(3))
  expect_equal(b, numeric(3))

  c <- ptd_twoInThree(c(1, 0, 1))
  d <- ptd_twoInThree(c(0, 1, 1))
  e <- ptd_twoInThree(c(1, 1, 0))
  expect_equal(c, d)
  expect_equal(c, e)
  expect_equal(c, rep(1, 3))

  f <- ptd_twoInThree(c(0, 0, 1, 0, 1, 0, 0))
  expect_equal(f, c(0, 0, 1, 1, 1, 0, 0))

  g <- ptd_twoInThree(c(0, 0, 1, 1, 0, 0, 0))
  expect_equal(g, c(0, 1, 1, 1, 1, 0, 0))
})

# ptd_partOfTwoInThree() ----
test_that("partOfTwoInThree works as expected", {
  av <- numeric()
  at <- ptd_twoInThree(av)
  aa <- ptd_partOfTwoInThree(at, av)
  expect_equal(aa, av)

  bv <- numeric(3)
  bt <- ptd_twoInThree(bv)
  ba <- ptd_partOfTwoInThree(bv, bt)
  expect_equal(ba, bv)

  cv <- c(1, 0, 1)
  ct <- ptd_twoInThree(cv)
  ca <- ptd_partOfTwoInThree(ct, cv)
  expect_equal(ca, cv)

  dv <- c(0, 1, 1)
  dt <- ptd_twoInThree(dv)
  da <- ptd_partOfTwoInThree(dt, dv)
  expect_equal(da, dv)

  ev <- c(1, 1, 0)
  et <- ptd_twoInThree(ev)
  ea <- ptd_partOfTwoInThree(et, ev)
  expect_equal(ea, ev)

  fv <- c(0, 0, 1, 0, 1, 0, 0)
  ft <- ptd_twoInThree(fv)
  fa <- ptd_partOfTwoInThree(ft, fv)
  expect_equal(fa, fv)

  gv <- c(0, 0, 1, 1, 0, 0, 0)
  gt <- ptd_twoInThree(gv)
  ga <- ptd_partOfTwoInThree(gv, gv)
  expect_equal(ga, gv)

  hv <- c(1, 0, 0, 1)
  ht <- ptd_twoInThree(hv)
  ha <- ptd_partOfTwoInThree(ht, hv)
  expect_equal(ha, c(0, 0, 0, 0))

  iv <- c(1, 0, 0, 1, 1)
  it <- ptd_twoInThree(iv)
  ia <- ptd_partOfTwoInThree(it, iv)
  expect_equal(ia, c(0, 0, 0, 1, 1))
})

# ptd_specialCauseFlag() ----
test_that("specialCauseFlag works as expected", {
  # there are 7 possible inpts that result in a 1 result, and 1 input that results in a 0.
  # we can mock the functions that are called and return results that can test these cases
  m1 <- mock("ptd_sevenPointOneSideOfMean")
  m2 <- mock("ptd_sevenPointTrend")
  # partOfSevenTrend: this is called twice
  m3 <- mock(
    c(1, -1, 0, 0, 0, 0, 0, 0),
    c(0, 0, 1, -1, 0, 0, 0, 0)
  )
  m4 <- mock("ptd_twoInThree")
  m5 <- mock(c(0, 0, 0, 0, 1, 0, 0, 0)) # partOfTwoInThree

  stub(ptd_specialCauseFlag, "ptd_sevenPointOneSideOfMean", m1)
  stub(ptd_specialCauseFlag, "ptd_sevenPointTrend", m2)
  stub(ptd_specialCauseFlag, "ptd_partOfSevenTrend", m3)
  stub(ptd_specialCauseFlag, "ptd_twoInThree", m4)
  stub(ptd_specialCauseFlag, "ptd_partOfTwoInThree", m5)

  a <- ptd_specialCauseFlag(
    1:8, "ptd_relativeToMean", "ptd_closeToLimits",
    c(0, 0, 0, 0, 0, 1, -1, 0)
  )
  expect_equal(a, c(rep(1, 7), 0))
})
