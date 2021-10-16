library(testthat)
library(mockery)

test_that("it set's up the geom correctly", {
  g <- geom_ptd_icon()

  expect_equal(g$aes_params, setNames(list(), character()))
  expect_s3_class(g$data, "ggproto_method")
  expect_s3_class(g$geom, "GeomPTDIcon")
  expect_equal(g$geom_params, list(na.rm = FALSE))
  expect_false(g$inherit.aes)
  expect_equal(sapply(g$mapping, quo_name),
               c(type = "type", colour = "colour", text = "text"))
  expect_s3_class(g$position, "PositionIdentity")
  expect_false(g$show.legend)
  expect_s3_class(g$stat, "StatIdentity")
  expect_equal(g$stat_params, list(na.rm = FALSE))
})

test_that("it set's up GeomPTDIcon correctly", {
  expect_equal(unclass(GeomPTDIcon$default_aes), setNames(list(), character()))
  expect_equal(GeomPTDIcon$required_aes, c("type", "colour", "text"))
  expect_s3_class(GeomPTDIcon, c("GeomPTDIcon", "Geom", "ggproto", "gg"))
})

test_that("it transforms the data correctly", {
  g <- geom_ptd_icon()

  set.seed(123)
  d <- data.frame(x = as.Date("2020-01-01") + 1:24,
                  y = rnorm(24),
                  f = rep(0:1, each = 12))

  s1 <- ptd_spc(d, "y", "x")

  expect_equal(
    g$data(s1),
    tibble(
      f = "no facet",
      type = "variation",
      colour = "common_cause",
      text = "cc"
    )
  )

  s2 <- ptd_spc(d, "y", "x", target = 0.5)
  expect_equal(
    g$data(s2),
    tibble(
      f = c("no facet", "no facet"),
      type = c("variation", "assurance"),
      colour = c("common_cause", "common_cause"),
      text = c("cc", "i")
    )
  )

  s3 <- ptd_spc(d, "y", "x", facet = "f")
  expect_equal(
    g$data(s3),
    tibble(
      f = c(0, 1),
      type = c("variation", "variation"),
      colour = c("common_cause", "common_cause"),
      text = c("cc", "cc")
    )
  )

  s4 <- ptd_spc(d, "y", "x", facet = "f", target = 0.5)
  expect_equal(
    g$data(s4),
    tibble(
      f = c(0, 1, 0, 1),
      type = rep(c("variation", "assurance"), each = 2),
      colour = rep("common_cause", 4),
      text = rep(c("cc", "i"), each = 2)
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
        colour = c("red", "green"),
        text = c("a", "b")
      ),
      panel_params = list(),
      coord = list(transform = function(x, ...) x)
    )
  )
})
