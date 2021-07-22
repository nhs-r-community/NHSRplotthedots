
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NHSRplotthedots <a alt="NHS-R Community's logo" href='https://nhsrcommunity.com/'><img src='man/figures/logo.png' align="right" height="80" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/nhs-r-community/NHSRplotthedots/branch/main/graph/badge.svg)](https://codecov.io/gh/nhs-r-community/NHSRplotthedots?branch=main)
[![R-CMD-check](https://github.com/nhs-r-community/NHSRplotthedots/workflows/R-CMD-check/badge.svg)](https://github.com/nhs-r-community/NHSRplotthedots/actions)
<!-- badges: end -->

This package is built by the NHS-R community to provide tools for
drawing statistical process control (SPC) charts. This package supports
the NHSE/I programme [‘Making Data
Count’](https://www.england.nhs.uk/a-focus-on-staff-health-and-wellbeing/publications-and-resources/making-data-count/),
and allows users to draw XmR charts, use change points, and apply rules
with summary indicators for when rules are breached.

Please be aware that this package is in the early stages of development,
and features may change.

## Installation

As the package develops there will be a full release to CRAN if
possible, but until that time you can install from
[GitHub](https://github.com/) using the `remotes` package with:

``` r
# install.packages("remotes")
remotes::install_github("https://github.com/nhs-r-community/NHSRplotthedots")
```

# Overview

Welcome to the NHS-R community’s collaborative package for building a
specific type of statistical process control (SPC) chart, the XmR chart.
We are aiming to support the NHS England and NHS Improvement’s ‘Making
Data Count’ programme, please see
<a href="https://www.england.nhs.uk/a-focus-on-staff-health-and-wellbeing/publications-and-resources/making-data-count/">
here </a> for more details. The programme encourages boards, managers,
and analyst teams to present data in ways that show change over time,
and drive better understanding of indicators than ‘RAG’ (red, amber,
green) rated board reports often present.

The help-files, and vignette within this package tell you more about the
possible options for controlling the charts, but below is a simple
example of the type of chart the package produces. We will use the
`ae_attendances` dataset from the `NHSRdatasets` package and a bit of
`tidyverse` `dplyr` code to select some organisations.

``` r
library(NHSRplotthedots)
library(NHSRdatasets)
library(tidyverse)

sub_set <- 
  ae_attendances %>% 
  filter(org_code == 'RQM' & type ==1 & period < as.Date('2018-04-01'))

spc(sub_set, valueField = "breaches", dateField = "period"
    , options = spcOptions(improvementDirection = "decrease"
                           , yAxisLabel = "4-hour wait breaches"
                           , mainTitle = "SPC of A&E waiting time breaches for RQM"))
```

<img src="man/figures/README-example-1.png" width="100%" />

# Contribution

This is an NHS-R Community project that is open for anyone to contribute
to in any way that they are able. If you want to learn more about this
please join the discussion at [the NHS-R Community Slack
group](https://nhsrcommunity.slack.com/) and the specific channel \#
proj-shiny-spc.
