# Create plotly

Creates a plotly object using the parameters passed in.

## Usage

``` r
ptd_create_plotly(
  x,
  point_size = 4,
  percentage_y_axis = FALSE,
  main_title,
  x_axis_label,
  y_axis_label,
  fixed_x_axis_multiple = TRUE,
  fixed_y_axis_multiple = TRUE,
  x_axis_date_format = "%d/%m/%y",
  x_axis_breaks = NULL,
  y_axis_breaks = NULL,
  icons_size = 0.15,
  icons_position = c("top right", "bottom right", "bottom left", "top left", "none"),
  colours = ptd_spc_colours(),
  theme_override = NULL,
  break_lines = c("both", "limits", "process", "none"),
  ...
)
```

## Arguments

- x:

  an object created by [`ptd_spc()`](ptd_spc.md)

- point_size:

  Specify the plotting point size for the ggplot output. Default is 2.5.

- percentage_y_axis:

  Specify whether the y axis values are percentages. Accepted values are
  TRUE for percentage y axis, FALSE for integer y axis. Defaults to
  FALSE.

- main_title:

  Specify a character string value for the ggplot title.

- x_axis_label:

  Specify a character string value for the x axis title.

- y_axis_label:

  Specify a character string value for the y axis title.

- fixed_x_axis_multiple:

  Specify whether, if producing a faceted spc, x axis should be fixed
  for all facet plots. Accepted values are TRUE for fixed x axes or
  FALSE for individual x axes.

- fixed_y_axis_multiple:

  Specify whether, if producing a faceted spc, y axis should be fixed
  for all facet plots. Accepted values are TRUE for fixed y axes or
  FALSE for individual y axes.

- x_axis_date_format:

  Specify how dates on the x axis should be displayed. Format should be
  provided as a character string using 'd m Y' etc syntax.

- x_axis_breaks:

  Specify an interval value for breaks on the x axis. Value should be a
  character string expressing interval length and type, e.g. "3 months",
  "7 days".

- y_axis_breaks:

  Specify an interval value for breaks on the y axis. Value should be a
  numeric vector of length 1, either an integer for integer scales or a
  decimal value for percentage scales. This option is ignored if
  faceting is in use.

- icons_size:

  The size of the icons, defined in terms of font size. Defaults to
  0.15.

- icons_position:

  Where to show the icons, either "top right" (default), "bottom right",
  "bottom left", "top left", or "none".

- colours:

  Specify the colours to use in the plot, use the
  [`ptd_spc_colours()`](ptd_spc_colours.md) function to change defaults.

- theme_override:

  Specify a list containing ggplot theme elements which can be used to
  override the default appearance of the plot.

- break_lines:

  whether to break lines when a rebase happens. Defaults to "both", but
  can break just "limits" lines, "process" lines, or "none".

- ...:

  currently ignored

## Value

The plotly object
