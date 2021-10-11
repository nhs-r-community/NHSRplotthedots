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
  d <- data.frame(x = to_datetime("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  attr(d$x, "tzone") <- ""

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

test_that("it sets the y-axis to percentages if percentage_y_axis is TRUE", {
  set.seed(123)

  m <- mock()
  stub(ptd_create_ggplot, "scales::label_percent", m)

  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s, percentage_y_axis = TRUE)
  p2 <- ptd_create_ggplot(s, percentage_y_axis = TRUE, y_axis_breaks = 0.2)

  expect_called(m, 2)
  expect_args(m, 1, accuracy = NULL)
  expect_args(m, 2, accuracy = 0.2)
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
    common_cause              = "#7B7D7D", # grey
    special_cause_improvement = "#289de0", # blue
    special_cause_neutral     = "#361475", # purple
    special_cause_concern     = "#fab428" # orange
  )

  expect_called(m, 1)
  expect_args(m, 1, values = colours, labels = ptd_title_case)
})

test_that("it sets the main title correctly", {
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), z = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$labels$title, "SPC Chart of Y, starting 02/01/2020")

  p2 <- ptd_create_ggplot(s, main_title = "Thing")
  expect_equal(p2$labels$title, "Thing")
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
