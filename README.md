<!-- README.md is generated from README.Rmd. Please edit that file -->

# NHSRplotthedots <a alt="NHS-R Community's logo" href='https://nhsrcommunity.com/'><img src='https://nhs-r-community.github.io/assets/logo/nhsr-logo.png' align="right" height="80" /></a>

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/nhs-r-community/NHSRplotthedots/workflows/R-CMD-check/badge.svg)](https://github.com/nhs-r-community/NHSRplotthedots/actions)
[![Codecov test
coverage](https://codecov.io/gh/nhs-r-community/NHSRplotthedots/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nhs-r-community/NHSRplotthedots?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/NHSRplotthedots)](https://CRAN.R-project.org/package=NHSRplotthedots)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/NHSRplotthedots)](https://CRAN.R-project.org/package=NHSRplotthedots)
<!-- badges: end -->

This package is built by the [NHS-R
community](https://nhsrcommunity.com) to provide tools for drawing
statistical process control (SPC) charts. This package supports NHS
England’s [‘Making Data
Count’](https://www.england.nhs.uk/publication/making-data-count/)
programme, and allows users to draw XmR charts, use change points, and
apply rules with summary indicators for when rules are breached.

Please be aware that this package is in the early stages of development,
and features may change.

## Installation

    # install from CRAN
    install.packages("NHSRplotthedots")

    # Or install the development version from GitHub using {remotes} package:
    # install.packages("remotes")
    remotes::install_github("https://github.com/nhs-r-community/NHSRplotthedots", build_vignettes = TRUE)

# Overview

Welcome to the NHS-R community’s package for building a specific type of
statistical process control (SPC) chart, the XmR chart. We are aiming to
support NHS England’s [‘Making Data
Count’](https://www.england.nhs.uk/publication/making-data-count/)
programme. The programme encourages boards, managers, and analyst teams
to present data in ways that show change over time and drive better
understanding of indicators than ‘RAG’ (red, amber, green) rated board
reports often present.

The help files and vignettes within this package tell you more about the
possible options for controlling the charts, but below are some simple
examples of the type of chart the package produces. We will use the
`ae_attendances` dataset from the `{NHSRdatasets}` package and a bit of
`{dplyr}` code to select some organisations.

    library(NHSRplotthedots)
    library(NHSRdatasets)
    library(dplyr)

    sub_set <- ae_attendances |>
      filter(org_code == "RQM", type == 1, period < as.Date("2018-04-01"))

    sub_set |>
      ptd_spc(
        value_field = breaches,
        date_field = period,
        improvement_direction = "decrease"
      )

<img src="man/figures/README-example-1.png" width="100%" />

This plot is ok on its own, but we can specify more control options if
we explicitly pass it on to the `plot()` function.

    sub_set |>
      ptd_spc(
        value_field = breaches,
        date_field = period,
        improvement_direction = "decrease"
      ) |>
      plot(
        y_axis_label = "4-hour wait breaches",
        main_title = "SPC of A&E waiting time breaches for RQM"
      )

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

or, equivalently:

    sub_set |>
      ptd_spc(
        value_field = breaches,
        date_field = period,
        improvement_direction = "decrease"
      ) |>
      ptd_create_ggplot(
        y_axis_label = "4-hour wait breaches",
        main_title = "SPC of A&E waiting time breaches for RQM"
      )

You can also use the `summary()` function to get some basic statistics
about your SPC data frame. The function prints the SPC options, and then
returns the summarised results as a table:

    summary <- sub_set |>
      ptd_spc(
        value_field = breaches,
        date_field = period,
        improvement_direction = "decrease",
        target = 1200
      ) |>
      summary()
    #> Plot the Dots SPC options:
    #> ================================
    #> value_field:          'breaches'
    #> date_field:           'period'
    #> facet_field:          not set
    #> rebase:               not set
    #> fix_after_n_points:   not set
    #> improvement_direction:'decrease'
    #> target:               '1200'
    #> trajectory:           not set
    #> screen_outliers:      'TRUE'
    #> --------------------------------

You could assign this summary table to a variable and use it later:

    summary$variation_type
    #> [1] "common_cause"
    summary$assurance_type
    #> [1] "inconsistent"

### Interactive plots with Plotly

It’s also possible to generate interactive plots using the `{plotly}`
package by replacing the call to `plot` with `ptd_create_plotly`. This
function takes the same arguments as `plot`/`ptd_create_ggplot`.

    sub_set |>
      ptd_spc(
        value_field = breaches,
        date_field = period,
        improvement_direction = "decrease"
      ) |>
      ptd_create_plotly(
        y_axis_label = "4-hour wait breaches",
        main_title = "SPC of A&E waiting time breaches for RQM"
      )

### Adding annotations for mean and process limits

The package (from v0.2.0) supports annotating the values of the mean and
the upper and lower process limits on a secondary (right-hand side) y
axis, if this is helpful for you and your audience.

The way to achieve this is to turn on the `label_limits` option:

    sub_set |>
      ptd_spc(
        value_field = breaches,
        date_field = period,
        improvement_direction = "decrease"
      ) |>
      ptd_create_ggplot(
        y_axis_label = "4-hour wait breaches",
        main_title = "SPC of A&E waiting time breaches for RQM",
        label_limits = TRUE
      )

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

If you have rebased the chart, the mean and process limit annotations
will only show for the most recent section.

## Getting help

To find out more about the `ptd_spc()` function, you can view the help
with:

    ?ptd_spc

Details on the extra plot controls can be found using:

    ?ptd_create_ggplot

To view the vignette (worked example), use:

    vignette("intro", package = "NHSRplotthedots")

    vignette(package = "NHSRplotthedots")

# Contribution

This is an NHS-R Community project that is open for anyone to contribute
to in any way that they are able. The project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

If you want to learn more about this project, please join the discussion
at [the NHS-R Community Slack group](https://nhsrcommunity.slack.com/)
and the specific channel
[\#proj-nhsr-plot-the-dots](https://nhsrcommunity.slack.com/archives/CSVD4SYF3).

The simplest way to contribute is to raise an issue detailing the
feature or functionality you would like to see added, or any unexpected
behaviour or bugs you have experienced.
