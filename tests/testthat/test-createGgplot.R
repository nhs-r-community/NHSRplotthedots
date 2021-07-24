library(testthat)
library(mockery)

# createGgplot() ----
test_that("it raises an error is x is not a ptd_spc_df object", {
  expect_error(createGgplot(data.frame(x = 1, y = 2)),
               "x argument must be an 'ptd_spc_df' object, created by spc().")
})

test_that("it calls validatePlotOptions", {
  m <- mock(stop())
  stub(createGgplot, "validatePlotOptions", m)

  try(createGgplot(spc(data.frame(x = Sys.Date(), y = 1), "y", "x"),
                   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
      silent = TRUE)

  expect_called(m, 1)
  expect_args(m, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
})

test_that("it returns a ggplot object", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- spc(d, "y", "x")
  p <- createGgplot(s)

  expect_s3_class(p, c("gg", "ggplot"))
  expect_length(p$layers, 9)
  expect_equal(p$labels, list(x = "X", y = "Y", title = "SPC Chart"))
})

test_that("it facet's the plot if facetField is set", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), g = rep(c(1, 2), each = 10))

  s1 <- spc(d, "y", "x")
  p1 <- createGgplot(s1)
  expect_equal(p1$facet$vars(), character())

  s2 <- spc(d, "y", "x", facetField = "g")
  p2 <- createGgplot(s2)
  expect_equal(p2$facet$vars(), "f")
})

test_that("it sets the xAxisBreaks correctly", {
  m <- mock()
  stub(createGgplot, "scale_x_datetime", m)

  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- spc(d, "y", "x")

  # no breaks set
  p1 <- createGgplot(s)
  p2 <- createGgplot(s, xAxisBreaks = "3 days")

  expect_called(m, 2)
  expect_args(m, 1,  date_breaks = waiver(), date_labels = "%d/%m/%y")
  expect_args(m, 2,  date_breaks = "3 days", date_labels = "%d/%m/%y")
})

test_that("it sets xAxisLabel correctly", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- spc(d, "y", "x")

  p1 <- createGgplot(s)
  expect_equal(p1$labels$x, "X")

  p2 <- createGgplot(s, xAxisLabel = "X Axis Label")
  expect_equal(p2$labels$x, "X Axis Label")
})

test_that("it sets yAxisLabel correctly", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- spc(d, "y", "x")

  p1 <- createGgplot(s)
  expect_equal(p1$labels$y, "Y")

  p2 <- createGgplot(s, yAxisLabel = "Y Axis Label")
  expect_equal(p2$labels$y, "Y Axis Label")
})

test_that("it sets scales correctly in a faceted plot", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), g = rep(c(1, 2), each = 10))
  s <- spc(d, "y", "x", facetField = "g")

  p1 <- createGgplot(s)
  expect_false(p1$facet$params$free$x)
  expect_false(p1$facet$params$free$y)

  p2 <- createGgplot(s, fixedXAxisMultiple = FALSE)
  expect_true(p2$facet$params$free$x)
  expect_false(p2$facet$params$free$y)

  p3 <- createGgplot(s, fixedYAxisMultiple = FALSE)
  expect_false(p3$facet$params$free$x)
  expect_true(p3$facet$params$free$y)

  p4 <- createGgplot(s, fixedXAxisMultiple = FALSE, fixedYAxisMultiple = FALSE)
  expect_true(p4$facet$params$free$x)
  expect_true(p4$facet$params$free$y)

  p5 <- createGgplot(s, fixedXAxisMultiple = TRUE, fixedYAxisMultiple = TRUE)
  expect_false(p5$facet$params$free$x)
  expect_false(p5$facet$params$free$y)
})

test_that("it sets the y-axis to percentages if convertToPercentages is provided", {
  set.seed(123)

  m <- mock()
  stub(createGgplot, "scales::percent_format", m)

  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- spc(d, "y", "x")

  p1 <- createGgplot(s, percentageYAxis = TRUE)
  p2 <- createGgplot(s, percentageYAxis = TRUE, yAxisBreaks = 0.2)

  expect_called(m, 2)
  expect_args(m, 1, breaks = 0.1)
  expect_args(m, 2, breaks = 0.2)
})

test_that("it sets the y-axis if yAxisBreaks is provided", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- spc(d, "y", "x")

  p1 <- createGgplot(s, yAxisBreaks = 1)
  expect_true(all(diff(p1$scales$scales[[2]]$breaks) == 1))

  p2 <- createGgplot(s, yAxisBreaks = 0.5)
  expect_true(all(diff(p2$scales$scales[[2]]$breaks) == 0.5))
})

test_that("it adds themeOverride to the plot", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- spc(d, "y", "x")

  p1 <- createGgplot(s)
  expect_equal(p1$theme$panel.background$fill, NULL)

  p2 <- createGgplot(s, themeOverride = theme(panel.background = element_rect("black")))
  expect_equal(p2$theme$panel.background$fill, "black")
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

  expect_called(m, 1)
  expect_args(m, 1, s,
              pointSize = 4,
              percentageYAxis = FALSE,
              mainTitle = "SPC Chart",
              xAxisLabel = NULL,
              yAxisLabel = NULL,
              fixedXAxisMultiple = TRUE,
              fixedYAxisMultiple = TRUE,
              xAxisDateFormat = "%d/%m/%y",
              xAxisBreaks = NULL,
              yAxisBreaks = NULL,
              themeOverride = NULL)
})

# validatePlotOptions() ----
test_that("it handles pointSize correctly", {
  # these should run fine
  validatePlotOptions(pointSize = NULL)
  validatePlotOptions(pointSize = 5)

  # these will error
  em <- "pointSize must be a single number greater than 0 and less than or equal to 10."
  expect_error(validatePlotOptions(pointSize = "a"), em)
  expect_error(validatePlotOptions(pointSize = 0), em)
  expect_error(validatePlotOptions(pointSize = 11), em)
  expect_error(validatePlotOptions(pointSize = c(5, 5)), em)
})

test_that("it handles percentageYAxis correctly", {
  # these should run fine
  validatePlotOptions(percentageYAxis = NULL)
  validatePlotOptions(percentageYAxis = TRUE)
  validatePlotOptions(percentageYAxis = FALSE)

  # these will error
  em <- "percentageYAxis argument must a single logical."
  expect_error(validatePlotOptions(percentageYAxis = "a"), em)
  expect_error(validatePlotOptions(percentageYAxis = -1), em)
  expect_error(validatePlotOptions(percentageYAxis = 2), em)
  expect_error(validatePlotOptions(percentageYAxis = c(TRUE, FALSE)), em)
})

test_that("it handles mainTitle correctly", {
  # these should run fine
  validatePlotOptions(mainTitle = NULL)
  validatePlotOptions(mainTitle = "title")

  # these will error
  em <- "mainTitle argument must be a character of length 1."
  expect_error(validatePlotOptions(mainTitle = 1), em)
  expect_error(validatePlotOptions(mainTitle = c("a", "b")), em)
})

test_that("it handles xAxisLabel correctly", {
  # these should run fine
  validatePlotOptions(xAxisLabel = NULL)
  validatePlotOptions(xAxisLabel = "title")

  # these will error
  em <- "xAxisLabel argument must be a character of length 1."
  expect_error(validatePlotOptions(xAxisLabel = 1), em)
  expect_error(validatePlotOptions(xAxisLabel = c("a", "b")), em)
})

test_that("it handles yAxisLabel correctly", {
  # these should run fine
  validatePlotOptions(yAxisLabel = NULL)
  validatePlotOptions(yAxisLabel = "title")

  # these will error
  em <- "yAxisLabel argument must be a character of length 1."
  expect_error(validatePlotOptions(yAxisLabel = 1), em)
  expect_error(validatePlotOptions(yAxisLabel = c("a", "b")), em)
})

test_that("it handles fixedXAxisMultiple correctly", {
  # these should run fine
  validatePlotOptions(fixedXAxisMultiple = NULL)
  validatePlotOptions(fixedXAxisMultiple = TRUE)

  # these will error
  em <- "fixedXAxisMultiple argument must be a logical of length 1."
  expect_error(validatePlotOptions(fixedXAxisMultiple = 1), em)
  expect_error(validatePlotOptions(fixedXAxisMultiple = c(TRUE, FALSE)), em)
})

test_that("it handles fixedYAxisMultiple correctly", {
  # these should run fine
  validatePlotOptions(fixedYAxisMultiple = NULL)
  validatePlotOptions(fixedYAxisMultiple = TRUE)

  # these will error
  em <- "fixedYAxisMultiple argument must be a logical of length 1."
  expect_error(validatePlotOptions(fixedYAxisMultiple = 1), em)
  expect_error(validatePlotOptions(fixedYAxisMultiple = c(TRUE, FALSE)), em)
})

test_that("it handles xAxisDateFormat correctly", {
  # these should run fine
  validatePlotOptions(xAxisDateFormat = NULL)
  validatePlotOptions(xAxisDateFormat = "a")

  # these will error
  em <- "xAxisDateFormat argument must be a character of length 1."
  expect_error(validatePlotOptions(xAxisDateFormat = 1), em)
  expect_error(validatePlotOptions(xAxisDateFormat = c("a", "b")), em)
})

test_that("it handles xAxisBreaks correctly", {
  # these should run fine
  validatePlotOptions(xAxisBreaks = NULL)
  validatePlotOptions(xAxisBreaks = "1 day")
  validatePlotOptions(xAxisBreaks = "2 days")
  validatePlotOptions(xAxisBreaks = "12 days")
  validatePlotOptions(xAxisBreaks = "1 week")
  validatePlotOptions(xAxisBreaks = "1 month")
  validatePlotOptions(xAxisBreaks = "1 quarter")
  validatePlotOptions(xAxisBreaks = "1 year")

  # these will error
  em <- "xAxisBreaks argument must be a character of length 1."
  expect_error(validatePlotOptions(xAxisBreaks = 1), em)
  expect_error(validatePlotOptions(xAxisBreaks = c("1 week", "2 weeks")), em)
  expect_error(validatePlotOptions(xAxisBreaks = "week"), em)
})

test_that("it handles yAxisBreaks correctly", {
  # these should run fine
  validatePlotOptions(yAxisBreaks = NULL)
  validatePlotOptions(yAxisBreaks = 1)

  # these will error
  em <- "yAxisBreaks argument must be a numeric of length 1."
  expect_error(validatePlotOptions(yAxisBreaks = "a"), em)
  expect_error(validatePlotOptions(yAxisBreaks = c(1, 2)), em)
})

test_that("it handles themeOverride correctly", {
  # these should run fine
  validatePlotOptions(themeOverride = theme())

  # these will error
  em <- "themeOverride must be an object created by theme()"
  expect_error(validatePlotOptions(themeOverride = list()), em)
})
