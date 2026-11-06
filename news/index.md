# Changelog

## NHSRplotthedots 0.2.2

CRAN release: 2025-09-29

This second version of ‘Plot the Dots’ adds new features and bugfixes to
the original version. They are summarised below:

### New features

- Improved icons added for both variation and assurance types.
- Ploty implementation added with
  [`ptd_create_plotly()`](../reference/ptd_create_plotly.md)
- Process limits and mean can now be annotated on the right of the plot
  with `ptd_create_ggplot(... label_limits = TRUE)`.

### Bugfixes

- Fixed an issue where points on opposite sides of the mean could
  incorrectly trigger “2-in-3” special-cause flags (issue
  [\#143](https://github.com/nhs-r-community/NHSRplotthedots/issues/143)).
- Fixed an issue where trends of 7 points were coloured differently
  depending on whether they are increasing or decreasing (issue
  [\#167](https://github.com/nhs-r-community/NHSRplotthedots/issues/167)).
- Fixed an issue where trends of 7 points crossing the mean are coloured
  incorrectly (issue
  [\#171](https://github.com/nhs-r-community/NHSRplotthedots/issues/171))
- Standardised icon and point colours with the MDC team PowerBI team
  (issue
  [\#196](https://github.com/nhs-r-community/NHSRplotthedots/issues/196)).

## NHSRplotthedots 0.1.0

CRAN release: 2021-11-03

This is first release for the NHS-R Community’s ‘Plot the Dots’ package.
This supports NHS England and NHS Improvement’s ‘Making Data Count’
campaign which encourages the use of run charts and statistical process
control charts for more informed decision making. This package creates
methods for XmR charts, with consideration for multiple indicators with
facets, ‘rebasing’, and applying rules sets with summary indicators.
This package is for use across the NHS, augmenting existing tools in SQL
and MS Excel.
