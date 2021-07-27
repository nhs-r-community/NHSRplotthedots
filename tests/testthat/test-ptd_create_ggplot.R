library(testthat)
library(mockery)

# ptd_create_ggplot() ----
test_that("it raises an error is x is not a ptd_spc_df object", {
  expect_error(
    ptd_create_ggplot(data.frame(x = 1, y = 2)),
    "x argument must be an 'ptd_spc_df' object, created by ptd_spc()."
  )
})

test_that("it calls ptd_validate_plot_options", {
  m <- mock(stop())
  stub(ptd_create_ggplot, "ptd_validate_plot_options", m)

  try(
    ptd_create_ggplot(
      ptd_spc(data.frame(x = Sys.Date(), y = 1), "y", "x"),
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12
    ),
    silent = TRUE
  )

  expect_called(m, 1)
  expect_args(m, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
})

test_that("it returns a ggplot object", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")
  p <- ptd_create_ggplot(s)

  expect_s3_class(p, c("gg", "ggplot"))
  expect_length(p$layers, 7)
  expect_equal(
    p$labels,
    list(
      x = "X",
      y = "Y",
      title = "SPC Chart of Y, starting 02/01/2020",
      colour = "point_type"
    )
  )
})

test_that("it facet's the plot if facet_field is set", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), g = rep(c(1, 2), each = 10))

  s1 <- ptd_spc(d, "y", "x")
  p1 <- ptd_create_ggplot(s1)
  expect_equal(p1$facet$vars(), character())

  s2 <- ptd_spc(d, "y", "x", facet_field = "g")
  p2 <- ptd_create_ggplot(s2)
  expect_equal(p2$facet$vars(), "f")
})

test_that("it sets the x_axis_breaks correctly", {
  m <- mock()
  stub(ptd_create_ggplot, "scale_x_datetime", m)

  set.seed(123)
  d <- data.frame(x = as.POSIXct("2020-01-01", tz = "utc") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  # no breaks set
  p1 <- ptd_create_ggplot(s)
  p2 <- ptd_create_ggplot(s, x_axis_breaks = "3 days")
  p3 <- ptd_create_ggplot(s, x_axis_date_format = "%Y-%m-%d")
  p4 <- ptd_create_ggplot(s, x_axis_breaks = "3 days", x_axis_date_format = "%Y-%m-%d")

  expect_called(m, 4)
  expect_args(m, 1, breaks = d$x, date_labels = "%d/%m/%y")
  expect_args(m, 2, date_breaks = "3 days", date_labels = "%d/%m/%y")
  expect_args(m, 3, breaks = d$x, date_labels = "%Y-%m-%d")
  expect_args(m, 4, date_breaks = "3 days", date_labels = "%Y-%m-%d")
})

test_that("it sets x_axis_label correctly", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$labels$x, "X")

  p2 <- ptd_create_ggplot(s, x_axis_label = "X Axis Label")
  expect_equal(p2$labels$x, "X Axis Label")
})

test_that("it sets y_axis_label correctly", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$labels$y, "Y")

  p2 <- ptd_create_ggplot(s, y_axis_label = "Y Axis Label")
  expect_equal(p2$labels$y, "Y Axis Label")
})

test_that("it sets scales correctly in a faceted plot", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), g = rep(c(1, 2), each = 10))
  s <- ptd_spc(d, "y", "x", facet_field = "g")

  p1 <- ptd_create_ggplot(s)
  expect_false(p1$facet$params$free$x)
  expect_false(p1$facet$params$free$y)

  p2 <- ptd_create_ggplot(s, fixed_x_axis_multiple = FALSE)
  expect_true(p2$facet$params$free$x)
  expect_false(p2$facet$params$free$y)

  p3 <- ptd_create_ggplot(s, fixed_y_axis_multiple = FALSE)
  expect_false(p3$facet$params$free$x)
  expect_true(p3$facet$params$free$y)

  p4 <- ptd_create_ggplot(s, fixed_x_axis_multiple = FALSE, fixed_y_axis_multiple = FALSE)
  expect_true(p4$facet$params$free$x)
  expect_true(p4$facet$params$free$y)

  p5 <- ptd_create_ggplot(s, fixed_x_axis_multiple = TRUE, fixed_y_axis_multiple = TRUE)
  expect_false(p5$facet$params$free$x)
  expect_false(p5$facet$params$free$y)
})

test_that("it sets the y-axis to percentages if convertToPercentages is provided", {
  set.seed(123)

  m <- mock()
  stub(ptd_create_ggplot, "scales::percent_format", m)

  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s, percentage_y_axis = TRUE)
  p2 <- ptd_create_ggplot(s, percentage_y_axis = TRUE, y_axis_breaks = 0.2)

  expect_called(m, 2)
  expect_args(m, 1, breaks = 0.1)
  expect_args(m, 2, breaks = 0.2)
})

test_that("it sets the y-axis if y_axis_breaks is provided", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s, y_axis_breaks = 1)
  expect_true(all(diff(p1$scales$scales[[3]]$breaks) == 1))

  p2 <- ptd_create_ggplot(s, y_axis_breaks = 0.5)
  expect_true(all(diff(p2$scales$scales[[3]]$breaks) == 0.5))
})

test_that("it adds theme_override to the plot", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$theme$panel.background$fill, NULL)

  p2 <- ptd_create_ggplot(s, theme_override = theme(panel.background = element_rect("black")))
  expect_equal(p2$theme$panel.background$fill, "black")
})

test_that("it sets the colour of the points based on the type", {
  m <- mock()

  stub(ptd_create_ggplot, "scale_colour_manual", m)

  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p <- ptd_create_ggplot(s)

  colours <- list(
    common_cause              = "#7B7D7D",
    special_cause_improvement = "#289de0",
    special_cause_concern     = "#fab428"
  )

  expect_called(m, 1)
  expect_args(m, 1, values = colours, labels = ptd_title_case)
})

# plot() ----
test_that("it calls ptd_create_ggplot()", {
  set.seed(123)
  s <- ptd_spc(
    data.frame(
      x = Sys.Date() + 1:20,
      y = rnorm(20)
    ),
    "y", "x"
  )

  m <- mock()
  stub(plot.ptd_spc_df, "ptd_create_ggplot", m)
  stub(plot.ptd_spc_df, "ptd_spc_colours", "colours")
  plot(s)

  expect_called(m, 1)
  expect_args(m, 1, s,
    point_size = 4,
    percentage_y_axis = FALSE,
    main_title = NULL,
    x_axis_label = NULL,
    y_axis_label = NULL,
    fixed_x_axis_multiple = TRUE,
    fixed_y_axis_multiple = TRUE,
    x_axis_date_format = "%d/%m/%y",
    x_axis_breaks = NULL,
    y_axis_breaks = NULL,
    colours = "colours",
    theme_override = NULL
  )
})

# ptd_validate_plot_options() ----
test_that("it handles point_size correctly", {
  # these should run fine
  ptd_validate_plot_options(point_size = NULL)
  ptd_validate_plot_options(point_size = 5)

  # these will error
  em <- "point_size must be a single number greater than 0 and less than or equal to 10."
  expect_error(ptd_validate_plot_options(point_size = "a"), em)
  expect_error(ptd_validate_plot_options(point_size = 0), em)
  expect_error(ptd_validate_plot_options(point_size = 11), em)
  expect_error(ptd_validate_plot_options(point_size = c(5, 5)), em)
})

test_that("it handles percentage_y_axis correctly", {
  # these should run fine
  ptd_validate_plot_options(percentage_y_axis = NULL)
  ptd_validate_plot_options(percentage_y_axis = TRUE)
  ptd_validate_plot_options(percentage_y_axis = FALSE)

  # these will error
  em <- "percentage_y_axis argument must a single logical."
  expect_error(ptd_validate_plot_options(percentage_y_axis = "a"), em)
  expect_error(ptd_validate_plot_options(percentage_y_axis = -1), em)
  expect_error(ptd_validate_plot_options(percentage_y_axis = 2), em)
  expect_error(ptd_validate_plot_options(percentage_y_axis = c(TRUE, FALSE)), em)
})

test_that("it handles main_title correctly", {
  # these should run fine
  ptd_validate_plot_options(main_title = NULL)
  ptd_validate_plot_options(main_title = "title")

  # these will error
  em <- "main_title argument must be a character of length 1."
  expect_error(ptd_validate_plot_options(main_title = 1), em)
  expect_error(ptd_validate_plot_options(main_title = c("a", "b")), em)
})

test_that("it handles x_axis_label correctly", {
  # these should run fine
  ptd_validate_plot_options(x_axis_label = NULL)
  ptd_validate_plot_options(x_axis_label = "title")

  # these will error
  em <- "x_axis_label argument must be a character of length 1."
  expect_error(ptd_validate_plot_options(x_axis_label = 1), em)
  expect_error(ptd_validate_plot_options(x_axis_label = c("a", "b")), em)
})

test_that("it handles y_axis_label correctly", {
  # these should run fine
  ptd_validate_plot_options(y_axis_label = NULL)
  ptd_validate_plot_options(y_axis_label = "title")

  # these will error
  em <- "y_axis_label argument must be a character of length 1."
  expect_error(ptd_validate_plot_options(y_axis_label = 1), em)
  expect_error(ptd_validate_plot_options(y_axis_label = c("a", "b")), em)
})

test_that("it handles fixed_x_axis_multiple correctly", {
  # these should run fine
  ptd_validate_plot_options(fixed_x_axis_multiple = NULL)
  ptd_validate_plot_options(fixed_x_axis_multiple = TRUE)

  # these will error
  em <- "fixed_x_axis_multiple argument must be a logical of length 1."
  expect_error(ptd_validate_plot_options(fixed_x_axis_multiple = 1), em)
  expect_error(ptd_validate_plot_options(fixed_x_axis_multiple = c(TRUE, FALSE)), em)
})

test_that("it handles fixed_y_axis_multiple correctly", {
  # these should run fine
  ptd_validate_plot_options(fixed_y_axis_multiple = NULL)
  ptd_validate_plot_options(fixed_y_axis_multiple = TRUE)

  # these will error
  em <- "fixed_y_axis_multiple argument must be a logical of length 1."
  expect_error(ptd_validate_plot_options(fixed_y_axis_multiple = 1), em)
  expect_error(ptd_validate_plot_options(fixed_y_axis_multiple = c(TRUE, FALSE)), em)
})

test_that("it handles x_axis_date_format correctly", {
  # these should run fine
  ptd_validate_plot_options(x_axis_date_format = NULL)
  ptd_validate_plot_options(x_axis_date_format = "a")

  # these will error
  em <- "x_axis_date_format argument must be a character of length 1."
  expect_error(ptd_validate_plot_options(x_axis_date_format = 1), em)
  expect_error(ptd_validate_plot_options(x_axis_date_format = c("a", "b")), em)
})

test_that("it handles x_axis_breaks correctly", {
  # these should run fine
  ptd_validate_plot_options(x_axis_breaks = NULL)
  ptd_validate_plot_options(x_axis_breaks = "1 day")
  ptd_validate_plot_options(x_axis_breaks = "2 days")
  ptd_validate_plot_options(x_axis_breaks = "12 days")
  ptd_validate_plot_options(x_axis_breaks = "1 week")
  ptd_validate_plot_options(x_axis_breaks = "1 month")
  ptd_validate_plot_options(x_axis_breaks = "1 quarter")
  ptd_validate_plot_options(x_axis_breaks = "1 year")

  # these will error
  em <- "x_axis_breaks argument must be a character of length 1."
  expect_error(ptd_validate_plot_options(x_axis_breaks = 1), em)
  expect_error(ptd_validate_plot_options(x_axis_breaks = c("1 week", "2 weeks")), em)
  expect_error(ptd_validate_plot_options(x_axis_breaks = "week"), em)
})

test_that("it handles y_axis_breaks correctly", {
  # these should run fine
  ptd_validate_plot_options(y_axis_breaks = NULL)
  ptd_validate_plot_options(y_axis_breaks = 1)

  # these will error
  em <- "y_axis_breaks argument must be a numeric of length 1."
  expect_error(ptd_validate_plot_options(y_axis_breaks = "a"), em)
  expect_error(ptd_validate_plot_options(y_axis_breaks = c(1, 2)), em)
})

test_that("it handles colours correctly", {
  # these should run fine
  ptd_validate_plot_options(colours = ptd_spc_colours())

  # these will error
  em <- "colours must be an object created by ptd_spc_colours()."
  expect_error(ptd_validate_plot_options(colours = list()), em)
})

test_that("it handles theme_override correctly", {
  # these should run fine
  ptd_validate_plot_options(theme_override = theme())

  # these will error
  em <- "theme_override must be an object created by theme()."
  expect_error(ptd_validate_plot_options(theme_override = list()), em)
})
