library(testthat)
library(mockery)

test_that("it set's up the geom correctly", {
  g <- geom_ptd_icon()

  expect_equal(g$aes_params, setNames(list(), character()))
  expect_s3_class(g$data, "ggproto_method")
  expect_s3_class(g$geom, "GeomPTDIcon")
  expect_equal(g$geom_params, list(icons_size = 8L, icons_position = "top right", na.rm = FALSE))
  expect_false(g$inherit.aes)
  expect_equal(
    sapply(g$mapping, quo_name),
    c(type = "type", icon = "icon")
  )
  expect_s3_class(g$position, "PositionIdentity")
  expect_false(g$show.legend)
  expect_s3_class(g$stat, "StatIdentity")
  expect_equal(g$stat_params, list(na.rm = FALSE))
})

test_that("it set's up GeomPTDIcon correctly", {
  expect_equal(unclass(GeomPTDIcon$default_aes), setNames(list(), character()))
  expect_equal(GeomPTDIcon$required_aes, c("type", "icon"))
  expect_s3_class(GeomPTDIcon, c("GeomPTDIcon", "Geom", "ggproto", "gg"))
})

test_that("it transforms the data correctly", {
  withr::local_options(ptd_spc.warning_threshold = 0)
  stub(geom_ptd_icon, "system.file", \(..., package = "") paste(package, ..., sep = "/"))
  g <- geom_ptd_icon()

  set.seed(123)
  d <- data.frame(
    x = as.Date("2020-01-01") + 1:24,
    y = rnorm(24),
    f = rep(0:1, each = 12)
  )

  s1 <- ptd_spc(d, "y", "x")

  expect_equal(
    g$data(s1),
    tibble(
      f = "no facet",
      type = "variation",
      icon = "NHSRplotthedots/icons/variation/common_cause.png"
    )
  )

  s2 <- ptd_spc(d, "y", "x", target = 0.5)
  expect_equal(
    g$data(s2),
    tibble(
      f = c("no facet", "no facet"),
      type = c("variation", "assurance"),
      icon = paste("NHSRplotthedots/icons", c("variation/common_cause.png", "assurance/inconsistent.png"), sep = "/")
    )
  )

  s3 <- ptd_spc(d, "y", "x", facet = "f")
  expect_equal(
    g$data(s3),
    tibble(
      f = c(0, 1),
      type = c("variation", "variation"),
      icon = rep("NHSRplotthedots/icons/variation/common_cause.png", 2)
    )
  )

  s4 <- ptd_spc(d, "y", "x", facet = "f", target = 0.5)
  expect_equal(
    g$data(s4),
    tibble(
      f = c(0, 1, 0, 1),
      type = rep(c("variation", "assurance"), each = 2),
      icon = rep(
        paste("NHSRplotthedots/icons", c("variation/common_cause.png", "assurance/inconsistent.png"), sep = "/"),
        each = 2
      )
    )
  )

  d$y[d$f == 1] <- rnorm(12, 5)
  s5 <- ptd_spc(d, "y", "x", target = -3)
  expect_equal(
    g$data(s5),
    tibble(
      f = rep("no facet", 2),
      type = c("variation", "assurance"),
      icon = paste("NHSRplotthedots/icons", c("variation/improvement_high.png", "assurance/pass.png"), sep = "/")
    )
  )

  s6 <- ptd_spc(d, "y", "x", target = -3, improvement_direction = "decrease")
  expect_equal(
    g$data(s6),
    tibble(
      f = rep("no facet", 2),
      type = c("variation", "assurance"),
      icon = paste("NHSRplotthedots/icons", c("variation/improvement_high.png", "assurance/fail.png"), sep = "/")
    )
  )

  d$y[d$f == 1] <- rnorm(12, -5)
  s7 <- ptd_spc(d, "y", "x", target = 3)
  expect_equal(
    g$data(s7),
    tibble(
      f = rep("no facet", 2),
      type = c("variation", "assurance"),
      icon = paste("NHSRplotthedots/icons", c("variation/improvement_low.png", "assurance/fail.png"), sep = "/")
    )
  )

  s8 <- ptd_spc(d, "y", "x", target = 3, improvement_direction = "decrease")
  expect_equal(
    g$data(s8),
    tibble(
      f = rep("no facet", 2),
      type = c("variation", "assurance"),
      icon = paste("NHSRplotthedots/icons", c("variation/improvement_low.png", "assurance/pass.png"), sep = "/")
    )
  )

  s9 <- ptd_spc(d, "y", "x", target = 3, improvement_direction = "neutral")
  expect_equal(
    g$data(s9),
    tibble(
      f = "no facet",
      type = "variation",
      icon = "NHSRplotthedots/icons/variation/neutral_low.png"
    )
  )
})

test_that("GeomPTDIcon draw panel works as expected", {
  # note: grid creates grob's and numbers them... this test could break if other grob's happen to be created before
  # this test is run. run testthat::accept_snapshot() in those cases
  expect_snapshot(
    GeomPTDIcon$draw_panel(
      data = data.frame(
        type = c("variation", "assurance"),
        icon = c(
          system.file("icons", "variation", "improvement_low.png", package = "NHSRplotthedots"),
          system.file("icons", "assurance", "pass.png", package = "NHSRplotthedots")
        )
      ),
      panel_params = list(),
      coord = list(transform = function(x, ...) x),
      icons_size = 8L,
      icons_position = "top right"
    )
  )
})
