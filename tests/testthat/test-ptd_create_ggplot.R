library(testthat)
library(mockery)
library(ggplot2, warn.conflicts = FALSE)

# ptd_create_ggplot() ----
test_that("it raises an error if unknown arguments are passed", {
  expect_warning(
    try(
      ptd_create_ggplot(NULL, X = 1, Y = 2),
      silent = TRUE
    ),
    paste0(
      "Unknown arguments provided by plot: X, Y.\n",
      "Check for common spelling mistakes in arguments."
    ),
    fixed = TRUE
  )
})

test_that("it raises an error is x is not a ptd_spc_df object", {
  expect_error(
    ptd_create_ggplot(data.frame(x = 1, y = 2)),
    "x argument must be an 'ptd_spc_df' object, created by ptd_spc()."
  )
})

test_that("it calls ptd_validate_plot_options", {
  m <- mock(stop())
  stub(ptd_create_ggplot, "ptd_validate_plot_options", m)
  stub(ptd_create_ggplot, "match.arg", identity)

  try(
    ptd_create_ggplot(
      ptd_spc(data.frame(x = Sys.Date() + seq(20L), y = rnorm(20L)), "y", "x"),
      "point_size",
      "percentage_y_axis",
      "main_title",
      "x_axis_label",
      "y_axis_label",
      "fixed_x_axis_multiple",
      "fixed_y_axis_multiple",
      "x_axis_date_format",
      "x_axis_breaks",
      "y_axis_breaks",
      "limit_annotations",
      "icons_size",
      "icons_position",
      "colours",
      "theme_override",
      "break_lines"
    ),
    silent = TRUE
  )

  expect_called(m, 1)
  expect_args(
    m, 1,
    "point_size",
    "percentage_y_axis",
    "main_title",
    "x_axis_label",
    "y_axis_label",
    "fixed_x_axis_multiple",
    "fixed_y_axis_multiple",
    "x_axis_date_format",
    "x_axis_breaks",
    "y_axis_breaks",
    "limit_annotations",
    "icons_size",
    "icons_position",
    "colours",
    "theme_override",
    "break_lines"
  )
})

test_that("it returns a ggplot object", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L))
  s <- ptd_spc(d, "y", "x")
  p <- ptd_create_ggplot(s)
  pl <- as.list(p$labels)

  expect_s3_class(p, c("gg", "ggplot"))
  expect_true(inherits(pl, "ggplot2::labels"))
  expect_length(p$layers, 8)
  expect_equal(pl$title, "SPC Chart of Y, starting 02/01/2020")
  expect_equal(pl$x, "X")
  expect_equal(pl$y, "Y")
  expect_equal(pl$group, NULL)
  expect_equal(pl$caption, NULL)
})

test_that("it facets the plot if facet_field is set", {
  set.seed(123)
  d <- data.frame(
    x = as.Date("2020-01-01") + 1:20,
    y = rnorm(20L),
    g = rep(c(1L, 2L), each = 10L)
  )

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
  stub(ptd_create_ggplot, "ggplot2::scale_x_datetime", m)

  set.seed(123)
  d <- data.frame(x = as.POSIXct("2020-01-01") + seq(20L), y = rnorm(20L))
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
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$labels$x, "X")

  p2 <- ptd_create_ggplot(s, x_axis_label = "X Axis Label")
  expect_equal(p2$labels$x, "X Axis Label")
})

test_that("it sets y_axis_label correctly", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$labels$y, "Y")

  p2 <- ptd_create_ggplot(s, y_axis_label = "Y Axis Label")
  expect_equal(p2$labels$y, "Y Axis Label")
})

test_that("it sets scales correctly in a faceted plot", {
  set.seed(123)
  d <- data.frame(
    x = as.Date("2020-01-01") + seq(20L),
    y = rnorm(20L),
    g = rep(c(1L, 2L), each = 10L)
  )

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

test_that("it creates a secondary y axis with percentage scales", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L))
  s <- ptd_spc(d, "y", "x")

  sec_breaks <- s |>
    dplyr::select(all_of(c("lpl", "mean_col", "upl"))) |>
    dplyr::slice_head(n = 1) |>
    unlist() |>
    unname()

  p1 <- s |>
    ptd_create_ggplot(percentage_y_axis = TRUE, label_limits = TRUE)
  expect_equal(
    round(sec_breaks, 3),
    round(p1$scales$scales[[3]]$secondary.axis$breaks, 3)
  )
  p2 <- s |>
    ptd_create_ggplot(percentage_y_axis = TRUE, y_axis_breaks = 0.5, label_limits = TRUE)
  expect_equal(
    round(sec_breaks, 3),
    round(p2$scales$scales[[3]]$secondary.axis$breaks, 3)
  )
})

test_that("it creates a secondary y axis with integer scales", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L))
  s <- ptd_spc(d, "y", "x")

  sec_breaks <- s |>
    dplyr::select(all_of(c("lpl", "mean_col", "upl"))) |>
    dplyr::slice_head(n = 1) |>
    unlist() |>
    unname()

  p1 <- ptd_create_ggplot(s, percentage_y_axis = FALSE, label_limits = TRUE)
  expect_equal(p1$scales$scales[[3]]$secondary.axis$breaks, sec_breaks)

  p2 <- ptd_create_ggplot(s, y_axis_breaks = 1, label_limits = TRUE)
  expect_equal(p2$scales$scales[[3]]$secondary.axis$breaks, sec_breaks)
})

test_that("it sets the y-axis if y_axis_breaks is provided", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s, y_axis_breaks = 1)
  expect_true(all(diff(p1$scales$scales[[3]]$breaks) == 1))

  p2 <- ptd_create_ggplot(s, y_axis_breaks = 0.5)
  expect_true(all(diff(p2$scales$scales[[3]]$breaks) == 0.5))
})

test_that("it adds theme_override to the plot", {
  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$theme$panel.background$fill, NULL)

  p2 <- s |>
    ptd_create_ggplot(
      theme_override = ggplot2::theme(panel.background = ggplot2::element_rect("black")) # nolint
    )
  expect_equal(p2$theme$panel.background$fill, "black")
})

test_that("it breaks lines", {
  set.seed(123)

  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L))
  withr::with_options(list(ptd_spc.warning_threshold = 10), {
    s <- ptd_spc(d, "y", "x", rebase = as.Date("2020-01-01") + 11)
  })

  p1 <- ptd_create_ggplot(s)
  expect_null(p1$mapping$group)
  expect_equal(rlang::eval_tidy(p1$layers[[1]]$mapping$group), rep(0:1, each = 10))
  expect_equal(rlang::eval_tidy(p1$layers[[2]]$mapping$group), rep(0:1, each = 10))
  expect_equal(rlang::eval_tidy(p1$layers[[5]]$mapping$group), rep(0:1, each = 10))
  expect_equal(rlang::eval_tidy(p1$layers[[6]]$mapping$group), rep(0:1, each = 10))

  p2 <- ptd_create_ggplot(s, break_lines = "limits")
  expect_equal(rlang::eval_tidy(p2$layers[[1]]$mapping$group), rep(0:1, each = 10))
  expect_equal(rlang::eval_tidy(p2$layers[[2]]$mapping$group), rep(0:1, each = 10))
  expect_equal(rlang::eval_tidy(p2$layers[[5]]$mapping$group), rep(0:1, each = 10))
  expect_equal(rlang::eval_tidy(p2$layers[[6]]$mapping$group), 0)

  p3 <- ptd_create_ggplot(s, break_lines = "process")
  expect_equal(rlang::eval_tidy(p3$layers[[1]]$mapping$group), 0)
  expect_equal(rlang::eval_tidy(p3$layers[[2]]$mapping$group), 0)
  expect_equal(rlang::eval_tidy(p3$layers[[5]]$mapping$group), 0)
  expect_equal(rlang::eval_tidy(p3$layers[[6]]$mapping$group), rep(0:1, each = 10))

  p4 <- ptd_create_ggplot(s, break_lines = "none")
  expect_equal(rlang::eval_tidy(p4$layers[[1]]$mapping$group), 0)
  expect_equal(rlang::eval_tidy(p4$layers[[2]]$mapping$group), 0)
  expect_equal(rlang::eval_tidy(p4$layers[[5]]$mapping$group), 0)
  expect_equal(rlang::eval_tidy(p4$layers[[6]]$mapping$group), 0)
})

test_that("it sets the colour of the points based on the type", {
  m <- mock()

  stub(ptd_create_ggplot, "ggplot2::scale_colour_manual", m)

  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L)) |>
    # introduce some special cause variation!
    dplyr::mutate(
      across("y", \(y) dplyr::if_else(.data[["x"]] > "2020-01-15", y + 0.5, y))
    )

  colours_neutral <- list(
    common_cause              = "#a6a6a6", # grey
    special_cause_neutral     = "#490092" # purple
  )

  colours_otherwise <- list(
    common_cause              = "#a6a6a6", # grey
    special_cause_improvement = "#00b0f0", # blue
    special_cause_concern     = "#e46c0a" # orange
  )

  # case 1: improvement_direction = neutral
  s1 <- ptd_spc(d, "y", "x", improvement_direction = "neutral")
  p1 <- ptd_create_ggplot(s1)
  # case 2: improvement_direction = "increase"
  s2 <- ptd_spc(d, "y", "x", improvement_direction = "increase")
  p2 <- ptd_create_ggplot(s2)
  # case 3: improvement_direction = "decrease"
  s3 <- ptd_spc(d, "y", "x", improvement_direction = "decrease")
  p3 <- ptd_create_ggplot(s3)

  expect_called(m, 3)
  expect_args(m, 1, values = colours_neutral, labels = ptd_title_case)
  expect_args(m, 2, values = colours_otherwise, labels = ptd_title_case)
})

test_that("it sets the main title correctly", {
  d <- data.frame(x = as.Date("2020-01-01") + seq(20L), y = rnorm(20L), z = rnorm(20L))
  s <- ptd_spc(d, "y", "x")

  p1 <- ptd_create_ggplot(s)
  expect_equal(p1$labels$title, "SPC Chart of Y, starting 02/01/2020")

  p2 <- ptd_create_ggplot(s, main_title = "Thing")
  expect_equal(p2$labels$title, "Thing")
})

test_that("a plot with short rebase group has a warning caption", {
  d <- data.frame(x = as.Date("2020-01-01") + seq(40L), y = rnorm(40L))
  # rebase at midpoint, no short groups
  s1 <- ptd_spc(d, "y", "x", rebase = as.Date("2020-01-20"))
  # rebase close to end of points
  s2 <- suppressWarnings(ptd_spc(d, "y", "x", rebase = as.Date("2020-02-02")))

  p1 <- ptd_create_ggplot(s1)
  expect_equal(p1$labels$caption, NULL)

  p2 <- ptd_create_ggplot(s2)
  expect_equal(
    p2$labels$caption,
    paste0(
      "Some trial limits created by groups of fewer than 12 points exist.\n",
      "These will become more reliable as more data is added."
    )
  )
})

test_that("it doesn't add icons if icons_position is 'none'", {
  m <- mock()
  stub(ptd_create_ggplot, "geom_ptd_icon", m)

  set.seed(123)
  d <- data.frame(
    x = as.Date("2020-01-01") + seq(20L),
    y = rnorm(20L)
  )

  s1 <- ptd_spc(d, "y", "x", target = 0.5)
  p1 <- ptd_create_ggplot(s1, icons_position = "top right")
  p2 <- ptd_create_ggplot(s1, icons_position = "none")

  expect_called(m, 1)
})

# plot() ----
test_that("it calls ptd_create_ggplot()", {
  set.seed(123)
  s <- ptd_spc(
    data.frame(
      x = Sys.Date() + seq(20L),
      y = rnorm(20L)
    ),
    "y", "x"
  )

  m <- mock()
  stub(plot.ptd_spc_df, "ptd_create_ggplot", m)
  stub(plot.ptd_spc_df, "ptd_spc_colours", "colours")
  plot(s, main_title = "a", x_axis_label = "b", y_axis_label = "c")

  expect_called(m, 1)
  expect_args(m, 1, s,
    point_size = 4,
    percentage_y_axis = FALSE,
    main_title = "a",
    x_axis_label = "b",
    y_axis_label = "c",
    fixed_x_axis_multiple = TRUE,
    fixed_y_axis_multiple = TRUE,
    x_axis_date_format = "%d/%m/%y",
    x_axis_breaks = NULL,
    y_axis_breaks = NULL,
    limit_annotations = FALSE,
    icons_size = 8,
    icons_position = c("top right", "bottom right", "bottom left", "top left", "none"),
    colours = "colours",
    theme_override = NULL,
    break_lines = "both"
  )
})
