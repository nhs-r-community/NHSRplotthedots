library(testthat)
library(mockery)

# calculatePointHighlighting() ----
test_that("it calls functions as expected (no facet groups)", {
  a <- data.frame(f = rep(1, 1),
                  relativeToMean = rep(1, 4),
                  closeToLimits = rep(2, 4),
                  y = rep(3, 4),
                  outsideLimits = rep(4, 4))

  m1 <- mock("specialCauseFlag")
  m2 <- mock("pointType")

  stub(calculatePointHighlighting, "specialCauseFlag", m1)
  stub(calculatePointHighlighting, "case_when", m2)

  calculatePointHighlighting(a, "improvementDirection")

  expect_called(m1, 1)
  expect_called(m2, 1)

  expect_args(m1, 1, a$y, a$relativeToMean, a$closeToLimits, a$outsideLimits)
  expect_call(m2, 1, case_when(
    !specialCauseFlag ~ "common_cause",
    relativeToMean == improvementDirection ~ "special_cause_improvement",
    TRUE ~ "special_cause_concern"))
})

test_that("it calls functions as expected (with facet groups)", {
  a <- data.frame(f = 1:4,
                  relativeToMean = rep(1, 4),
                  closeToLimits = rep(2, 4),
                  y = rep(3, 4),
                  outsideLimits = rep(4, 4))

  m1 <- mock("specialCauseFlag", cycle = TRUE)
  m2 <- mock("pointType", cycle = TRUE)

  stub(calculatePointHighlighting, "specialCauseFlag", m1)
  stub(calculatePointHighlighting, "case_when", m2)

  calculatePointHighlighting(a, 1)

  expect_called(m1, 4)
  expect_called(m2, 4)
})

test_that("it returns the mutated data", {
  d <- data.frame(f = 1)
  stub(calculatePointHighlighting, "mutate", "mutate")
  stub(calculatePointHighlighting, "ungroup", "ungroup")

  a <- calculatePointHighlighting(d, 1)

  expect_equal(a, "ungroup")
})

test_that("it groups and ungroups the data", {
  d <- data.frame(f = 1)

  stub(calculatePointHighlighting, "mutate", function(x, ...) x)
  stub(calculatePointHighlighting, "ungroup", identity)

  a <- calculatePointHighlighting(d, 1)

  expect_equal(groups(a), list(as.symbol("f")))
})

# sevenPointOneSideOfMean() ----
test_that("sevenPointOneSideOfMean works as expected", {
  expect_equal(sevenPointOneSideOfMean(numeric()), numeric())
  expect_equal(sevenPointOneSideOfMean(rep(1, 6)),
               c(0, 0, 0, 0, 0, 0))
  expect_equal(sevenPointOneSideOfMean(c(rep(1, 6), -1)),
               c(0, 0, 0, 0, 0, 0, 0))
  expect_equal(sevenPointOneSideOfMean(c(rep(1, 8), -1)),
               c(0, 0, 0, 0, 0, 0, 1, 1, 0))
  expect_equal(sevenPointOneSideOfMean(c(rep(-1, 8), 1)),
               c(0, 0, 0, 0, 0, 0, 1, 1, 0))
})

# partOfSevenTrend() ----
test_that("partOfSevenTrend works as expected", {
  expect_equal(partOfSevenTrend(numeric()), numeric())
  expect_equal(partOfSevenTrend(rep(0, 6)),
               c(0, 0, 0, 0, 0, 0))

  a <- sevenPointOneSideOfMean(c(rep(1, 6), -1))
  expect_equal(partOfSevenTrend(a),
               c(0, 0, 0, 0, 0, 0, 0))

  b <- sevenPointOneSideOfMean(c(-1, rep(1, 8), -1))
  expect_equal(partOfSevenTrend(b),
               c(0, 1, 1, 1, 1, 1, 1, 1, 1, 0))

  c <- sevenPointOneSideOfMean(c(1, rep(-1, 8), 1))
  expect_equal(partOfSevenTrend(c),
               c(0, 1, 1, 1, 1, 1, 1, 1, 1, 0))
})

# sevenPointTrend() ----
test_that("sevenPointTrend works as expected", {
  expect_equal(sevenPointTrend(numeric()), numeric())
  expect_equal(sevenPointTrend(1:6),
               c(0, 0, 0, 0, 0, 0))

  a <- sevenPointTrend(c(1:3, 3:6))
  expect_equal(a, c(0, 0, 0, 0, 0, 0, 0))

  b <- sevenPointTrend(1:7)
  expect_equal(b, c(0, 0, 0, 0, 0, 0, 1))

  c <- sevenPointTrend(c(2, 1:7, 3))
  expect_equal(c, c(0, 0, 0, 0, 0, 0, 0, 1, 0))

  d <- sevenPointTrend(7:1)
  expect_equal(d, c(0, 0, 0, 0, 0, 0, -1))

  e <- sevenPointTrend(c(2, 7:1, 3))
  expect_equal(e, c(0, 0, 0, 0, 0, 0, 0, -1, 0))
})

# twoInThree() ----
test_that("twoInThree works as expected", {
  a <- twoInThree(numeric())
  expect_equal(a, numeric())

  b <- twoInThree(numeric(3))
  expect_equal(b, numeric(3))

  c <- twoInThree(c(1, 0, 1))
  d <- twoInThree(c(0, 1, 1))
  e <- twoInThree(c(1, 1, 0))
  expect_equal(c, d)
  expect_equal(c, e)
  expect_equal(c, rep(1, 3))

  f <- twoInThree(c(0, 0, 1, 0, 1, 0, 0))
  expect_equal(f, c(0, 0, 1, 1, 1, 0, 0))

  g <- twoInThree(c(0, 0, 1, 1, 0, 0, 0))
  expect_equal(g, c(0, 1, 1, 1, 1, 0, 0))
})

# partOfTwoInThree() ----
test_that("partOfTwoInThree works as expected", {
  av  <- numeric()
  at <- twoInThree(av)
  aa <- partOfTwoInThree(at, av)
  expect_equal(aa, av)

  bv <- numeric(3)
  bt <- twoInThree(bv)
  ba <- partOfTwoInThree(bv, bt)
  expect_equal(ba, bv)

  cv  <- c(1, 0, 1)
  ct <- twoInThree(cv)
  ca <- partOfTwoInThree(ct, cv)
  expect_equal(ca, cv)

  dv <- c(0, 1, 1)
  dt <- twoInThree(dv)
  da <- partOfTwoInThree(dt, dv)
  expect_equal(da, dv)

  ev <- c(1, 1, 0)
  et <- twoInThree(ev)
  ea <- partOfTwoInThree(et, ev)
  expect_equal(ea, ev)

  fv <- c(0, 0, 1, 0, 1, 0, 0)
  ft <- twoInThree(fv)
  fa <- partOfTwoInThree(ft, fv)
  expect_equal(fa, fv)

  gv <- c(0, 0, 1, 1, 0, 0, 0)
  gt <- twoInThree(gv)
  ga <- partOfTwoInThree(gv, gv)
  expect_equal(ga, gv)

  hv <- c(1, 0, 0, 1)
  ht <- twoInThree(hv)
  ha <- partOfTwoInThree(ht, hv)
  expect_equal(ha, c(0, 0, 0, 0))

  iv <- c(1, 0, 0, 1, 1)
  it <- twoInThree(iv)
  ia <- partOfTwoInThree(it, iv)
  expect_equal(ia, c(0, 0, 0, 1, 1))
})

# specialCauseFlag() ----
test_that("specialCauseFlag works as expected", {
  # there are 7 possible inpts that result in a 1 result, and 1 input that results in a 0.
  # we can mock the functions that are called and return results that can test these cases
  m1 <- mock("sevenPointOneSideOfMean")
  m2 <- mock("sevenPointTrend")
  # partOfSevenTrend: this is called twice
  m3 <- mock(c(1, -1, 0,  0, 0, 0, 0, 0),
             c(0,  0, 1, -1, 0, 0, 0, 0))
  m4 <- mock("twoInThree")
  m5 <- mock(c(0, 0, 0, 0, 1, 0, 0, 0)) # partOfTwoInThree

  stub(specialCauseFlag, "sevenPointOneSideOfMean", m1)
  stub(specialCauseFlag, "sevenPointTrend", m2)
  stub(specialCauseFlag, "partOfSevenTrend", m3)
  stub(specialCauseFlag, "twoInThree", m4)
  stub(specialCauseFlag, "partOfTwoInThree", m5)

  a <- specialCauseFlag(1:8, "relativeToMean", "closeToLimits",
                        c(0, 0, 0, 0, 0, 1, -1, 0))
  expect_equal(a, c(rep(1, 7), 0))
})
