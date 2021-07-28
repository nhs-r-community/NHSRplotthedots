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

  set.seed(1231)
  try(
    ptd_create_ggplot(
      ptd_spc(data.frame(x = Sys.Date() + 1:12, y = rnorm(12)), "y", "x"),
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13
    ),
    silent = TRUE
  )

  expect_called(m, 1)
  expect_args(m, 1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
})

test_that("it returns a ggplot object", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20))
  s <- ptd_spc(d, "y", "x")
  p <- ptd_create_ggplot(s)

  expect_s3_class(p, c("gg", "ggplot"))
  expect_length(p$layers, 9)
  expect_equal(
    p$labels,
    list(
      x = "X",
      y = "Y",
      title = "SPC Chart of Y, starting 02/01/2020",
      caption = NULL,
      colour = "point_type",
      label = "text"
    )
  )
})

test_that("it facet's the plot if facet_field is set", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), g = rep(c(1, 2), each = 10))

  withr::with_options(list(ptd_spc.warning_threshold = 10), {
    s1 <- ptd_spc(d, "y", "x")
    p1 <- ptd_create_ggplot(s1)
    expect_equal(p1$facet$vars(), character())

    s2 <- ptd_spc(d, "y", "x", facet_field = "g")
    p2 <- ptd_create_ggplot(s2)
    expect_equal(p2$facet$vars(), "f")
  })
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

  withr::with_options(list(ptd_spc.warning_threshold = 10), {
    s <- ptd_spc(d, "y", "x", facet_field = "g")
  })

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

  colours_neutal <- list(
    common_cause              = "#7B7D7D", # grey
    special_cause_neutral     = "#361475" # purple
  )

  colours_otherwise <- list(
    common_cause              = "#7B7D7D", # grey
    special_cause_improvement = "#289de0", # blue
    special_cause_concern     = "#fab428" # orange
  )

  # 1: improvement_direction = neutral
  s1 <- ptd_spc(d, "y", "x", improvement_direction = "neutral")
  p1 <- ptd_create_ggplot(s1)
  # 2: improvement_direction = "increase"
  s2 <- ptd_spc(d, "y", "x", improvement_direction = "increase")
  p2 <- ptd_create_ggplot(s2)
  # 3: improvement_direction = "decrease"
  s3 <- ptd_spc(d, "y", "x", improvement_direction = "decrease")
  p3 <- ptd_create_ggplot(s3)

  expect_called(m, 3)
  expect_args(m, 1, values = colours_neutal, labels = ptd_title_case)
  expect_args(m, 2, values = colours_otherwise, labels = ptd_title_case)
})

test_that("it sets the main title correctly", {
  d <- data.frame(x = as.Date("2020-01-01") + 1:20, y = rnorm(20), z = rnorm(20))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$labels$title, "SPC Chart of Y, starting 02/01/2020")

  p2 <- ptd_create_ggplot(s, main_title = "Thing")
  expect_equal(p2$labels$title, "Thing")
})

test_that("a plot with short rebase group has a warning caption", {
  d <- data.frame(x = as.Date("2020-01-01") + 1:40, y = rnorm(40))
  s1 <- ptd_spc(d, "y", "x", rebase = as.Date("2020-01-20")) # rebase at midpoint, no short groups
  s2 <- suppressWarnings(ptd_spc(d, "y", "x", rebase = as.Date("2020-02-02"))) # rebase close to end of points

  p1 <- ptd_create_ggplot(s1)
  expect_equal(p1$labels$caption, NULL)

  p2 <- ptd_create_ggplot(s2)
  expect_equal(
    p2$labels$caption,
    paste0(
      "Some trial limits created by groups of fewer than 12 points exist. \n",
      "These will become more reliable as more data is added."
    )
  )
})

test_that("it adds assurance points only when a target is set", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20,
                  y = rnorm(20))
  s1 <- ptd_spc(d, "y", "x")
  p1 <- plot(s1)

  s2 <- ptd_spc(d, "y", "x", target = 0.5)
  p2 <- plot(s2)

  expect_equal(nrow(p1$layers[[8]]$data), 1)
  expect_equal(nrow(p2$layers[[9]]$data), 2)
})

test_that("it doesn't add icons if show_icons is FALSE", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:20,
                  y = rnorm(20))

  s1 <- ptd_spc(d, "y", "x", target = 0.5)
  p1 <- plot(s1, show_icons = FALSE)

  expect_length(p1$layers, 7)
})

test_that("it changes position of icons if you set fixed_y_axis_multiple in a facet", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24,
                  y = rnorm(24),
                  f = rep(c(0, 1), each = 12))

  s1 <- ptd_spc(d, "y", "x", target = 0.5, facet = "f")
  p1 <- plot(s1)
  p2 <- plot(s1, fixed_y_axis_multiple = FALSE)

  expect_true(all(p1$layers[[8]]$data$y[[1]] == p1$layers[[8]]$data$y))
  expect_false(all(p2$layers[[8]]$data$y == p2$layers[[8]]$data$y[[4]]))
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
              show_icons = TRUE,
              colours = "colours",
              theme_override = NULL
  )
})
