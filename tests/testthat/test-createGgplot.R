library(testthat)
library(mockery)

# createGgplot() ----
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# plot() ----
test_that("it calls createGgplot()", {
  set.seed(123)
  s <- spc(data.frame(x = Sys.Date() + 1:20,
                      y = rnorm(20)),
           "y", "x")

  m <- mock()
  stub(plot.ptd_spc_df, "createGgplot", m)
  plot(s)
  plot(s, 5, mainTitle = "Plot")

  expect_called(m, 2)
  expect_args(m, 1, s)
  expect_args(m, 2, s, 5, mainTitle = "Plot")
})

# validattePlotOptions*() ----
test_that("it handles pointSize correctly", {
  # these should run fine
  validatePlotOptions$pointSize(NULL)
  validatePlotOptions$pointSize(5)

  # these will error
  em <- "pointSize must be a single number greater than 0 and less than or equal to 10."
  expect_error(validatePlotOptions$pointSize("a"), em)
  expect_error(validatePlotOptions$pointSize(0), em)
  expect_error(validatePlotOptions$pointSize(11), em)
  expect_error(validatePlotOptions$pointSize(c(5, 5)), em)
})

test_that("it handles percentageYAxis correctly", {
  # these should run fine
  validatePlotOptions$percentageYAxis(NULL)
  validatePlotOptions$percentageYAxis(TRUE)
  validatePlotOptions$percentageYAxis(0)
  validatePlotOptions$percentageYAxis(1)

  # these will error
  em <- "percentageYAxis argument must a single value of TRUE, FALSE, or a numeric between 0 and 1."
  expect_error(validatePlotOptions$percentageYAxis("a"), em)
  expect_error(validatePlotOptions$percentageYAxis(-1), em)
  expect_error(validatePlotOptions$percentageYAxis(2), em)
  expect_error(validatePlotOptions$percentageYAxis(c(0, 1)), em)
})

test_that("it handles mainTitle correctly", {
  # these should run fine
  validatePlotOptions$mainTitle(NULL)
  validatePlotOptions$mainTitle("title")

  # these will error
  em <- "mainTitle argument must be a character of length 1."
  expect_error(validatePlotOptions$mainTitle(1), em)
  expect_error(validatePlotOptions$mainTitle(c("a", "b")), em)
})

test_that("it handles xAxisLabel correctly", {
  # these should run fine
  validatePlotOptions$xAxisLabel(NULL)
  validatePlotOptions$xAxisLabel("title")

  # these will error
  em <- "xAxisLabel argument must be a character of length 1."
  expect_error(validatePlotOptions$xAxisLabel(1), em)
  expect_error(validatePlotOptions$xAxisLabel(c("a", "b")), em)
})

test_that("it handles yAxisLabel correctly", {
  # these should run fine
  validatePlotOptions$yAxisLabel(NULL)
  validatePlotOptions$yAxisLabel("title")

  # these will error
  em <- "yAxisLabel argument must be a character of length 1."
  expect_error(validatePlotOptions$yAxisLabel(1), em)
  expect_error(validatePlotOptions$yAxisLabel(c("a", "b")), em)
})

test_that("it handles fixedXAxisMultiple correctly", {
  # these should run fine
  validatePlotOptions$fixedXAxisMultiple(NULL)
  validatePlotOptions$fixedXAxisMultiple(TRUE)

  # these will error
  em <- "fixedXAxisMultiple argument must be a logical of length 1."
  expect_error(validatePlotOptions$fixedXAxisMultiple(1), em)
  expect_error(validatePlotOptions$fixedXAxisMultiple(c(TRUE, FALSE)), em)
})

test_that("it handles fixedYAxisMultiple correctly", {
  # these should run fine
  validatePlotOptions$fixedYAxisMultiple(NULL)
  validatePlotOptions$fixedYAxisMultiple(TRUE)

  # these will error
  em <- "fixedYAxisMultiple argument must be a logical of length 1."
  expect_error(validatePlotOptions$fixedYAxisMultiple(1), em)
  expect_error(validatePlotOptions$fixedYAxisMultiple(c(TRUE, FALSE)), em)
})

test_that("it handles xAxisDateFormat correctly", {
  # these should run fine
  validatePlotOptions$xAxisDateFormat(NULL)
  validatePlotOptions$xAxisDateFormat("a")

  # these will error
  em <- "xAxisDateFormat argument must be a character of length 1."
  expect_error(validatePlotOptions$xAxisDateFormat(1), em)
  expect_error(validatePlotOptions$xAxisDateFormat(c("a", "b")), em)
})

test_that("it handles xAxisBreaks correctly", {
  # these should run fine
  validatePlotOptions$xAxisBreaks(NULL)
  validatePlotOptions$xAxisBreaks("1 day")
  validatePlotOptions$xAxisBreaks("2 days")
  validatePlotOptions$xAxisBreaks("12 days")
  validatePlotOptions$xAxisBreaks("1 week")
  validatePlotOptions$xAxisBreaks("1 month")
  validatePlotOptions$xAxisBreaks("1 quarter")
  validatePlotOptions$xAxisBreaks("1 year")

  # these will error
  em <- "xAxisBreaks argument must be a character of length 1."
  expect_error(validatePlotOptions$xAxisBreaks(1), em)
  expect_error(validatePlotOptions$xAxisBreaks(c("1 week", "2 weeks")), em)
  expect_error(validatePlotOptions$xAxisBreaks("week"), em)
})

test_that("it handles yAxisBreaks correctly", {
  # these should run fine
  validatePlotOptions$yAxisBreaks(NULL)
  validatePlotOptions$yAxisBreaks("a")

  # these will error
  em <- "yAxisBreaks argument must be a character of length 1."
  expect_error(validatePlotOptions$yAxisBreaks(1), em)
  expect_error(validatePlotOptions$yAxisBreaks(c("a", "b")), em)
})
