library(testthat)
library(mockery)
library(NHSRdatasets)

# make sure we have consistent fonts on all machines
library(sysfonts)
if (!"Roboto" %in% font_families()) {
  font_add_google(name = "Roboto")
}

ae_all <- function() {
  ae_attendances %>%
    group_by(period) %>%
    summarise(across(attendances:breaches, sum), .groups = "drop",
              performance = 1 - breaches / attendances)
}

ae_rl4_by_type <- function() {
  ae_attendances %>%
    filter(org_code == "RL4") %>%
    group_by(period, type) %>%
    summarise(across(attendances:breaches, sum), .groups = "drop",
              performance = 1 - breaches / attendances)
}

ae_largest_4_orgs <- function() {
  x <- ae_attendances %>%
    filter(type == 1)

  y <- x %>%
    group_by(org_code) %>%
    summarise(n = sum(attendances)) %>%
    arrange(desc(n)) %>%
    slice_head(n = 4)

  semi_join(x, y, by = "org_code")
}

# helper function to save a plot image. files are saved in a temporary location but testthat will copy to the _snaps
# folder automatically
save_plot <- function(filename, plot) {
  path <- file.path(tempdir(), paste0(filename, ".svg"))
  ggsave(path, plot, dpi = 150, width = 8, height = 6)
  return(path)
}

# helper function: most of the tests below work off of a similar pattern
test_spc <- function(spc_obj, name, skip_obj = FALSE, ...) {
  skip_on_cran()
  skip_on_os("solaris")
  skip_on_covr()

  spc_plot <- plot(spc_obj, font_family = "Roboto", ...)

  if (!skip_obj) {
    expect_snapshot_value(spc_obj, style = "serialize")
  }
  expect_snapshot_file(save_plot(name, spc_plot))
}

test_that("ae_all attendances spc returns expected values", {
  ptd_spc(ae_all(), attendances, period) %>%
    test_spc("ae_all_attendances")
})

test_that("ae_all attendances spc with rebase returns expected values", {
  ptd_spc(ae_all(), attendances, period, rebase = as.Date("2017-04-01")) %>%
    test_spc("ae_all_attendances_rebase")
})

test_that("ae_all performance spc returns expected values", {
  ptd_spc(ae_all(), performance, period, fix_after_n_points = 12) %>%
    test_spc("ae_all_performance")
})

test_that("ae_rl4_by_type attendances spc returns expected values", {
  ptd_spc(ae_rl4_by_type(), attendances, period, facet_field = type) %>%
    test_spc("ae_rl4_by_type_attendances")
})

test_that("ae_rl4_by_type performance spc returns expected values", {
  ptd_spc(ae_rl4_by_type(), performance, period, facet_field = type) %>%
    test_spc("ae_rl4_by_type_performance")
})

test_that("plot arguments behave as expected", {
  spc_obj <- ptd_spc(ae_all(), performance, period)

  # changing plot size
  test_spc(spc_obj, "ae_all_performance_point_size", point_size = 5)

  # changing percentage_y_axis
  test_spc(spc_obj, "ae_all_performance_percentage_y_axis", percentage_y_axis = TRUE)

  # changing main_title
  test_spc(spc_obj, "ae_all_performance_main_title", main_title = "Main Title")

  # changing axis labels
  test_spc(spc_obj, "ae_all_performance_axis_labels", x_axis_label = "x axis", y_axis_label = "y axis")

  # changing x_axis_date_format
  test_spc(spc_obj, "ae_all_performance_x_axis_date_format", x_axis_date_format = "%b-%Y")

  # changing axis breaks
  test_spc(spc_obj, "ae_all_performance_axis_breaks", x_axis_breaks = "3 months", y_axis_breaks = 0.2)

  # changing colours (rotating the default colours by one)
  colours <- ptd_spc_colours(
    common_cause = "#289de0",
    special_cause_improvement = "#361475",
    special_cause_neutral = "#fab428",
    special_cause_concern = "#7b7d7d",
    value_line = "#000000",
    mean_line = "#7b7d7d",
    lpl = "#7b7d7d",
    upl = "#de1b1b",
    target = "#361475",
    trajectory = "#7b7d7d"
  )
  test_spc(spc_obj, "ae_all_performance_colours", colours = colours)

  # changing theme_override
  test_spc(spc_obj, "ae_all_performance_theme_override",
           theme_override = theme(panel.grid = element_blank(), axis.line = element_line()))

  # fixed_*_axis_multiple: requires a facetted plot
})

test_that("fixed_*_axis_multiple argument works as expected", {
  spc_obj <- ptd_spc(ae_largest_4_orgs(), attendances, period, facet = org_code)
  fn <- function(name, ...) test_spc(spc_obj, name, skip_obj = TRUE, ...)

  fn("ae_largest_4_attendances_fixed_axis_default")
  fn("ae_largest_4_attendances_fixed_x_axis_FALSE", fixed_x_axis_multiple = FALSE)
  fn("ae_largest_4_attendances_fixed_y_axis_FALSE", fixed_y_axis_multiple = FALSE)
  fn("ae_largest_4_attendances_fixed_xy_axis_FALSE", fixed_x_axis_multiple = FALSE, fixed_y_axis_multiple = FALSE)
})
