# SPC Colours

Produces a list of colours that controls the geoms in the plot

## Usage

``` r
ptd_spc_colours(
  common_cause = "#a6a6a6",
  special_cause_improvement = "#00b0f0",
  special_cause_neutral = "#490092",
  special_cause_concern = "#e46c0a",
  value_line = "#a6a6a6",
  mean_line = "#000000",
  lpl = "#a6a6a6",
  upl = "#a6a6a6",
  target = "#de1b1b",
  trajectory = "#490092"
)
```

## Arguments

- common_cause, special_cause_improvement, special_cause_neutral,
  special_cause_concern:

  the colour of the points

- value_line:

  the colour of the line joining the points

- mean_line:

  the colour of the "mean" (average) line

- lpl, upl:

  the colour the the lower and upper process limit lines

- target:

  the colour of the target line

- trajectory:

  the colour of the trajectory line

## Value

A list of colours
