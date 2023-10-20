library(mockery)

test_that("ptd_create_plotly returns a plotly object", {
  mock_plot <- list(
    data = list(
      short_group_warnings = TRUE
    )
  )

  m_ptd_create_ggplot <- mock(mock_plot)
  m_ptd_get_icons <- mock(list(icon = c("icon_1", "icon_2")))
  m_read_svg_as_b64 <- mock("icon_1.svg", "icon_2.svg")
  m_ggplotly <- mock("plotly")
  m_ptd_plotly_fix_tooltips <- mock("plotly_fixed_tooltips")
  m_layout <- mock()

  stub(ptd_create_plotly, "ptd_spc_colours", "colours")
  stub(ptd_create_plotly, "ptd_create_ggplot", m_ptd_create_ggplot)
  stub(ptd_create_plotly, "ptd_get_icons", m_ptd_get_icons)
  stub(ptd_create_plotly, "read_svg_as_b64", m_read_svg_as_b64)
  stub(ptd_create_plotly, "plotly::ggplotly", m_ggplotly)
  stub(ptd_create_plotly, "ptd_plotly_fix_tooltips", m_ptd_plotly_fix_tooltips)
  stub(ptd_create_plotly, "plotly::layout", m_layout)

  actual <- ptd_create_plotly(
    "data",
    main_title = "title",
    x_axis_label = "x_axis_label",
    y_axis_label = "y_axis_label"
  )

  expect_called(m_ptd_create_ggplot, 1)
  expect_args(
    m_ptd_create_ggplot,
    1,
    "data",
    point_size = 4,
    percentage_y_axis = FALSE,
    main_title = "title",
    x_axis_label = "x_axis_label",
    y_axis_label = "y_axis_label",
    fixed_x_axis_multiple = TRUE,
    fixed_y_axis_multiple = TRUE,
    x_axis_date_format = "%d/%m/%y",
    x_axis_breaks = NULL,
    y_axis_breaks = NULL,
    icons_size = 0.15,
    icons_position = "none",
    colours = "colours",
    theme_override = NULL,
    break_lines = c("both", "limits", "process", "none")
  )

  expect_called(m_ptd_plotly_fix_tooltips, 1)
  expect_args(m_ptd_plotly_fix_tooltips, 1, "plotly")

  expect_called(m_ptd_get_icons, 1)
  expect_args(m_ptd_get_icons, 1, "data")

  expect_called(m_read_svg_as_b64, 2)
  expect_args(m_read_svg_as_b64, 1, "icon_1")
  expect_args(m_read_svg_as_b64, 2, "icon_2")

  expect_called(m_ggplotly, 1)
  expect_args(m_ggplotly, 1, mock_plot)

  expect_called(m_layout, 1)
  expect_args(
    m_layout,
    1,
    "plotly_fixed_tooltips",
    hovermode = "x unified",
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = -0.6
    ),
    annotations = list(
      x = 1,
      y = -0.3,
      text = paste(
        "Some trial limits created by groups of fewer than 12 points exist.",
        "These will become more reliable as more data is added.",
        sep = "\n"
      ),
      showarrow = FALSE,
      xref = "paper",
      yref = "paper",
      xanchor = "auto",
      yanchor = "auto",
      xshift = 0,
      yshift = 0,
      font = list(size = 12, color = "red")
    ),
    images = list(
      list(
        source = "icon_1.svg",
        xref = "paper",
        yref = "paper",
        x = 0.82,
        y = 0.925,
        sizex = 0.09,
        sizey = 0.15,
        sizing = "stretch",
        opacity = 1,
        layer = "above"
      ),
      list(
        source = "icon_2.svg",
        xref = "paper",
        yref = "paper",
        x = 0.91,
        y = 0.925,
        sizex = 0.09,
        sizey = 0.15,
        sizing = "stretch",
        opacity = 1,
        layer = "above"
      )
    )
  )
})


test_that("ptd_create_plotly returns a plotly object (no annotations or icons)", {
  mock_plot <- list(
    data = list(
      short_group_warnings = FALSE
    )
  )

  m_ptd_create_ggplot <- mock(mock_plot)
  m_ptd_get_icons <- mock(list(icon = c("icon_1", "icon_2")))
  m_read_svg_as_b64 <- mock("icon_1.svg", "icon_2.svg")
  m_ggplotly <- mock("plotly")
  m_ptd_plotly_fix_tooltips <- mock("plotly_fixed_tooltips")
  m_layout <- mock()

  stub(ptd_create_plotly, "ptd_spc_colours", "colours")
  stub(ptd_create_plotly, "ptd_create_ggplot", m_ptd_create_ggplot)
  stub(ptd_create_plotly, "ptd_get_icons", m_ptd_get_icons)
  stub(ptd_create_plotly, "read_svg_as_b64", m_read_svg_as_b64)
  stub(ptd_create_plotly, "plotly::ggplotly", m_ggplotly)
  stub(ptd_create_plotly, "ptd_plotly_fix_tooltips", m_ptd_plotly_fix_tooltips)
  stub(ptd_create_plotly, "plotly::layout", m_layout)

  actual <- ptd_create_plotly(
    "data",
    main_title = "title",
    x_axis_label = "x_axis_label",
    y_axis_label = "y_axis_label",
    icons_position = "none"
  )

  expect_called(m_layout, 1)
  expect_args(
    m_layout,
    1,
    "plotly_fixed_tooltips",
    hovermode = "x unified",
    legend = list(
      orientation = "h",
      xanchor = "center",
      x = 0.5,
      y = -0.6
    ),
    annotations = NULL,
    images = NULL
  )
})


test_that("ptd_create_plotly gives a warning with facet field and icons position not none", { # nolint
  mock_plot <- list(
    data = list(
      short_group_warnings = FALSE
    )
  )

  m_ptd_create_ggplot <- mock(mock_plot)
  m_ptd_get_icons <- mock(list(icon = c("icon_1", "icon_2")))
  m_read_svg_as_b64 <- mock("icon_1.svg", "icon_2.svg")
  m_ggplotly <- mock("plotly")
  m_ptd_plotly_fix_tooltips <- mock("plotly_fixed_tooltips")
  m_layout <- mock()

  stub(ptd_create_plotly, "ptd_spc_colours", "colours")
  stub(ptd_create_plotly, "ptd_create_ggplot", m_ptd_create_ggplot)
  stub(ptd_create_plotly, "ptd_get_icons", m_ptd_get_icons)
  stub(ptd_create_plotly, "read_svg_as_b64", m_read_svg_as_b64)
  stub(ptd_create_plotly, "plotly::ggplotly", m_ggplotly)
  stub(ptd_create_plotly, "ptd_plotly_fix_tooltips", m_ptd_plotly_fix_tooltips)
  stub(ptd_create_plotly, "plotly::layout", m_layout)

  expect_warning(
    ptd_create_plotly(
      structure(
        "data",
        options = list(
          facet_field = "x"
        )
      ),
      main_title = "title",
      x_axis_label = "x_axis_label",
      y_axis_label = "y_axis_label"
    )
  )

  expect_called(m_ptd_get_icons, 0)
})


test_that("read_svg_as_b64 converts files correctly", {
  m <- mock(
    "img",
    "b64"
  )

  stub(read_svg_as_b64, "file.size", 100)
  stub(read_svg_as_b64, "readBin", m)
  stub(read_svg_as_b64, "base64enc::base64encode", m)

  actual <- read_svg_as_b64("file")

  expect_called(m, 2)

  expect_args(m, 1, "file", "raw", 100)
  expect_args(m, 2, "img")

  expect_equal(actual, "data:image/svg+xml;base64,b64")
})


test_that("ptd_plotly_fix_tooltips simplifies the tooltips that will be displayed on hover", {
  plot <- list(
    x = list(
      data = list(
        list(
          text = c("x=1<br />y=2<br />z=3", "x=2<br />y=3<br />z=4")
        ),
        list(
          text = c("x=1<br />a=10<br />b=20", "x=2<br />a=30<br />b=40")
        ),
        list(
          text = c("x=1<br />z=100", "x=2<br />z=200")
        )
      )
    )
  )

  expected <- list(
    x = list(
      data = list(
        list(
          text = c("x=1<br /><br />y=2", "x=2<br /><br />y=3")
        ),
        list(
          text = c("a=10", "a=30")
        ),
        list(
          text = c("z=100", "z=200")
        )
      )
    )
  )

  actual <- ptd_plotly_fix_tooltips(plot)

  expect_equal(actual, expected)
})
