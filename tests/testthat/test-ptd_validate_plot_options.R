library(testthat)
library(mockery)

# ptd_validate_plot_options() ----
test_that("it handles point_size correctly", {
  # these should run fine
  ptd_validate_plot_options(point_size = NULL)
  ptd_validate_plot_options(point_size = 5)

  # these will error
  em <- "point_size argument must be a single number greater than 0 and less than or equal to 10."
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

test_that("it handles icons_size correctly", {
  # these should run fine
  ptd_validate_plot_options(icons_size = 8L)
  ptd_validate_plot_options(icons_size = 5.2)

  # these will error
  em <- "icons_size argument must be an integer of length 1."
  expect_error(ptd_validate_plot_options(icons_size = "a"), em, fixed = TRUE)
  expect_error(ptd_validate_plot_options(icons_size = c(8, 2)), em, fixed = TRUE)
})

test_that("it handles icons_position correctly", {
  # these should run fine
  ptd_validate_plot_options(icons_position = "top right")
  ptd_validate_plot_options(icons_position = "bottom right")
  ptd_validate_plot_options(icons_position = "bottom left")
  ptd_validate_plot_options(icons_position = "top left")
  ptd_validate_plot_options(icons_position = "none")

  # these will error
  em <- "icons_position argument must be one of 'top right', 'bottom right', 'bottom_left', 'top left', or 'none'"
  expect_error(ptd_validate_plot_options(icons_position = "a"), em, fixed = TRUE)
  expect_error(ptd_validate_plot_options(icons_position = c("top right", "top left")), em, fixed = TRUE)
})

test_that("it handles colours correctly", {
  # these should run fine
  ptd_validate_plot_options(colours = ptd_spc_colours())

  # these will error
  em <- "colours argument must be an object created by ptd_spc_colours()."
  expect_error(ptd_validate_plot_options(colours = list()), em)
})

test_that("it handles theme_override correctly", {
  # these should run fine
  ptd_validate_plot_options(theme_override = ggplot2::theme())

  # these will error
  em <- "theme_override argument must be an object created by theme()."
  expect_error(ptd_validate_plot_options(theme_override = list()), em)
})

test_that("it handles break_lines correctly", {
  # these should run fine
  ptd_validate_plot_options(break_lines = "both")
  ptd_validate_plot_options(break_lines = "limits")
  ptd_validate_plot_options(break_lines = "process")
  ptd_validate_plot_options(break_lines = "none")

  # these will error
  em <- "break_lines argument must be one of 'both', 'limits', 'process', or 'none'."
  expect_error(ptd_validate_plot_options(break_lines = list()), em)
  expect_error(ptd_validate_plot_options(break_lines = 1), em)
  expect_error(ptd_validate_plot_options(break_lines = c("both", "limits")), em)
})
