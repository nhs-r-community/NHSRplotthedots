library(testthat)
library(mockery)

# ptd_createGgplot() ----
test_that("it raises an error is x is not a ptd_spc_df object", {
  expect_error(ptd_createGgplot(data.frame(x = 1, y = 2)),
               "x argument must be an 'ptd_spc_df' object, created by ptd_spc().")
})

test_that("it calls validatePlotOptions", {
  m <- mock(stop())
  stub(ptd_createGgplot, "ptd_validatePlotOptions", m)

  try(ptd_createGgplot(ptd_spc(data.frame(x = Sys.Date(), y = 1), "y", "x"),
                   1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
      silent = TRUE)

  expect_called(m, 1)
  expect_args(m, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
})

test_that("it returns a ggplot object", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")
  p <- ptd_createGgplot(s)

  expect_s3_class(p, c("gg", "ggplot"))
  expect_length(p$layers, 7)
  expect_equal(p$labels,
               list(x = "X",
                    y = "Y",
                    title = "SPC Chart of Y, starting 02/01/2020",
                    colour = "pointType"))
})

test_that("it facet's the plot if facetField is set", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), g = rep(c(1, 2), each = 10))

  s1 <- ptd_spc(d, "y", "x")
  p1 <- ptd_createGgplot(s1)
  expect_equal(p1$facet$vars(), character())

  s2 <- ptd_spc(d, "y", "x", facetField = "g")
  p2 <- ptd_createGgplot(s2)
  expect_equal(p2$facet$vars(), "f")
})

test_that("it sets the xAxisBreaks correctly", {
  m <- mock()
  stub(ptd_createGgplot, "scale_x_datetime", m)

  set.seed(123)
  d <- data.frame(x = as.POSIXct("2020-01-01", tz = "utc") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  # no breaks set
  p1 <- ptd_createGgplot(s)
  p2 <- ptd_createGgplot(s, xAxisBreaks = "3 days")
  p3 <- ptd_createGgplot(s, xAxisDateFormat = "%Y-%m-%d")
  p4 <- ptd_createGgplot(s, xAxisBreaks = "3 days", xAxisDateFormat = "%Y-%m-%d")

  expect_called(m, 4)
  expect_args(m, 1, breaks = d$x, date_labels = "%d/%m/%y")
  expect_args(m, 2, date_breaks = "3 days", date_labels = "%d/%m/%y")
  expect_args(m, 3, breaks = d$x, date_labels = "%Y-%m-%d")
  expect_args(m, 4, date_breaks = "3 days", date_labels = "%Y-%m-%d")
})

test_that("it sets xAxisLabel correctly", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_createGgplot(s)
  expect_equal(p1$labels$x, "X")

  p2 <- ptd_createGgplot(s, xAxisLabel = "X Axis Label")
  expect_equal(p2$labels$x, "X Axis Label")
})

test_that("it sets yAxisLabel correctly", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_createGgplot(s)
  expect_equal(p1$labels$y, "Y")

  p2 <- ptd_createGgplot(s, yAxisLabel = "Y Axis Label")
  expect_equal(p2$labels$y, "Y Axis Label")
})

test_that("it sets scales correctly in a faceted plot", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), g = rep(c(1, 2), each = 10))
  s <- ptd_spc(d, "y", "x", facetField = "g")

  p1 <- ptd_createGgplot(s)
  expect_false(p1$facet$params$free$x)
  expect_false(p1$facet$params$free$y)

  p2 <- ptd_createGgplot(s, fixedXAxisMultiple = FALSE)
  expect_true(p2$facet$params$free$x)
  expect_false(p2$facet$params$free$y)

  p3 <- ptd_createGgplot(s, fixedYAxisMultiple = FALSE)
  expect_false(p3$facet$params$free$x)
  expect_true(p3$facet$params$free$y)

  p4 <- ptd_createGgplot(s, fixedXAxisMultiple = FALSE, fixedYAxisMultiple = FALSE)
  expect_true(p4$facet$params$free$x)
  expect_true(p4$facet$params$free$y)

  p5 <- ptd_createGgplot(s, fixedXAxisMultiple = TRUE, fixedYAxisMultiple = TRUE)
  expect_false(p5$facet$params$free$x)
  expect_false(p5$facet$params$free$y)
})

test_that("it sets the y-axis to percentages if convertToPercentages is provided", {
  set.seed(123)

  m <- mock()
  stub(ptd_createGgplot, "scales::percent_format", m)

  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_createGgplot(s, percentageYAxis = TRUE)
  p2 <- ptd_createGgplot(s, percentageYAxis = TRUE, yAxisBreaks = 0.2)

  expect_called(m, 2)
  expect_args(m, 1, breaks = 0.1)
  expect_args(m, 2, breaks = 0.2)
})

test_that("it sets the y-axis if yAxisBreaks is provided", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_createGgplot(s, yAxisBreaks = 1)
  expect_true(all(diff(p1$scales$scales[[3]]$breaks) == 1))

  p2 <- ptd_createGgplot(s, yAxisBreaks = 0.5)
  expect_true(all(diff(p2$scales$scales[[3]]$breaks) == 0.5))
})

test_that("it adds themeOverride to the plot", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_createGgplot(s)
  expect_equal(p1$theme$panel.background$fill, NULL)

  p2 <- ptd_createGgplot(s, themeOverride = theme(panel.background = element_rect("black")))
  expect_equal(p2$theme$panel.background$fill, "black")
})

test_that("it sets the colour of the points based on the type", {
  m <- mock()

  stub(ptd_createGgplot, "scale_colour_manual", m)

  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p <- ptd_createGgplot(s)

  colours <- list(
    common_cause              = "#7B7D7D",
    special_cause_improvement = "#289de0",
    special_cause_concern     = "#fab428"
  )

  expect_called(m, 1)
  expect_args(m, 1, values = colours, labels = ptd_titleCase)
})

# plot() ----
test_that("it calls ptd_createGgplot()", {
  set.seed(123)
  s <- ptd_spc(data.frame(x = Sys.Date() + 1:20,
                      y = rnorm(20)),
           "y", "x")

  m <- mock()
  stub(plot.ptd_spc_df, "ptd_createGgplot", m)
  stub(plot.ptd_spc_df, "ptd_spcColours", "colours")
  plot(s)

  expect_called(m, 1)
  expect_args(m, 1, s,
              pointSize = 4,
              percentageYAxis = FALSE,
              mainTitle = NULL,
              xAxisLabel = NULL,
              yAxisLabel = NULL,
              fixedXAxisMultiple = TRUE,
              fixedYAxisMultiple = TRUE,
              xAxisDateFormat = "%d/%m/%y",
              xAxisBreaks = NULL,
              yAxisBreaks = NULL,
              colours = "colours",
              themeOverride = NULL)
})

# validatePlotOptions() ----
test_that("it handles pointSize correctly", {
  # these should run fine
  ptd_validatePlotOptions(pointSize = NULL)
  ptd_validatePlotOptions(pointSize = 5)

  # these will error
  em <- "pointSize must be a single number greater than 0 and less than or equal to 10."
  expect_error(ptd_validatePlotOptions(pointSize = "a"), em)
  expect_error(ptd_validatePlotOptions(pointSize = 0), em)
  expect_error(ptd_validatePlotOptions(pointSize = 11), em)
  expect_error(ptd_validatePlotOptions(pointSize = c(5, 5)), em)
})

test_that("it handles percentageYAxis correctly", {
  # these should run fine
  ptd_validatePlotOptions(percentageYAxis = NULL)
  ptd_validatePlotOptions(percentageYAxis = TRUE)
  ptd_validatePlotOptions(percentageYAxis = FALSE)

  # these will error
  em <- "percentageYAxis argument must a single logical."
  expect_error(ptd_validatePlotOptions(percentageYAxis = "a"), em)
  expect_error(ptd_validatePlotOptions(percentageYAxis = -1), em)
  expect_error(ptd_validatePlotOptions(percentageYAxis = 2), em)
  expect_error(ptd_validatePlotOptions(percentageYAxis = c(TRUE, FALSE)), em)
})

test_that("it handles mainTitle correctly", {
  # these should run fine
  ptd_validatePlotOptions(mainTitle = NULL)
  ptd_validatePlotOptions(mainTitle = "title")

  # these will error
  em <- "mainTitle argument must be a character of length 1."
  expect_error(ptd_validatePlotOptions(mainTitle = 1), em)
  expect_error(ptd_validatePlotOptions(mainTitle = c("a", "b")), em)
})

test_that("it handles xAxisLabel correctly", {
  # these should run fine
  ptd_validatePlotOptions(xAxisLabel = NULL)
  ptd_validatePlotOptions(xAxisLabel = "title")

  # these will error
  em <- "xAxisLabel argument must be a character of length 1."
  expect_error(ptd_validatePlotOptions(xAxisLabel = 1), em)
  expect_error(ptd_validatePlotOptions(xAxisLabel = c("a", "b")), em)
})

test_that("it handles yAxisLabel correctly", {
  # these should run fine
  ptd_validatePlotOptions(yAxisLabel = NULL)
  ptd_validatePlotOptions(yAxisLabel = "title")

  # these will error
  em <- "yAxisLabel argument must be a character of length 1."
  expect_error(ptd_validatePlotOptions(yAxisLabel = 1), em)
  expect_error(ptd_validatePlotOptions(yAxisLabel = c("a", "b")), em)
})

test_that("it handles fixedXAxisMultiple correctly", {
  # these should run fine
  ptd_validatePlotOptions(fixedXAxisMultiple = NULL)
  ptd_validatePlotOptions(fixedXAxisMultiple = TRUE)

  # these will error
  em <- "fixedXAxisMultiple argument must be a logical of length 1."
  expect_error(ptd_validatePlotOptions(fixedXAxisMultiple = 1), em)
  expect_error(ptd_validatePlotOptions(fixedXAxisMultiple = c(TRUE, FALSE)), em)
})

test_that("it handles fixedYAxisMultiple correctly", {
  # these should run fine
  ptd_validatePlotOptions(fixedYAxisMultiple = NULL)
  ptd_validatePlotOptions(fixedYAxisMultiple = TRUE)

  # these will error
  em <- "fixedYAxisMultiple argument must be a logical of length 1."
  expect_error(ptd_validatePlotOptions(fixedYAxisMultiple = 1), em)
  expect_error(ptd_validatePlotOptions(fixedYAxisMultiple = c(TRUE, FALSE)), em)
})

test_that("it handles xAxisDateFormat correctly", {
  # these should run fine
  ptd_validatePlotOptions(xAxisDateFormat = NULL)
  ptd_validatePlotOptions(xAxisDateFormat = "a")

  # these will error
  em <- "xAxisDateFormat argument must be a character of length 1."
  expect_error(ptd_validatePlotOptions(xAxisDateFormat = 1), em)
  expect_error(ptd_validatePlotOptions(xAxisDateFormat = c("a", "b")), em)
})

test_that("it handles xAxisBreaks correctly", {
  # these should run fine
  ptd_validatePlotOptions(xAxisBreaks = NULL)
  ptd_validatePlotOptions(xAxisBreaks = "1 day")
  ptd_validatePlotOptions(xAxisBreaks = "2 days")
  ptd_validatePlotOptions(xAxisBreaks = "12 days")
  ptd_validatePlotOptions(xAxisBreaks = "1 week")
  ptd_validatePlotOptions(xAxisBreaks = "1 month")
  ptd_validatePlotOptions(xAxisBreaks = "1 quarter")
  ptd_validatePlotOptions(xAxisBreaks = "1 year")

  # these will error
  em <- "xAxisBreaks argument must be a character of length 1."
  expect_error(ptd_validatePlotOptions(xAxisBreaks = 1), em)
  expect_error(ptd_validatePlotOptions(xAxisBreaks = c("1 week", "2 weeks")), em)
  expect_error(ptd_validatePlotOptions(xAxisBreaks = "week"), em)
})

test_that("it handles yAxisBreaks correctly", {
  # these should run fine
  ptd_validatePlotOptions(yAxisBreaks = NULL)
  ptd_validatePlotOptions(yAxisBreaks = 1)

  # these will error
  em <- "yAxisBreaks argument must be a numeric of length 1."
  expect_error(ptd_validatePlotOptions(yAxisBreaks = "a"), em)
  expect_error(ptd_validatePlotOptions(yAxisBreaks = c(1, 2)), em)
})

test_that("it handles colours correctly", {
  # these should run fine
  ptd_validatePlotOptions(colours = ptd_spcColours())

  # these will error
  em <- "colours must be an object created by ptd_spcColours()."
  expect_error(ptd_validatePlotOptions(colours = list()), em)
})

test_that("it handles themeOverride correctly", {
  # these should run fine
  ptd_validatePlotOptions(themeOverride = theme())

  # these will error
  em <- "themeOverride must be an object created by theme()."
  expect_error(ptd_validatePlotOptions(themeOverride = list()), em)
})

