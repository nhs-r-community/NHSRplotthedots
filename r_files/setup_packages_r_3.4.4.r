# If developing for use on Microsoft PowerBI service, you need to be using 
# R 3.4.4 and some rather old versions of packages specified here:
# https://learn.microsoft.com/en-us/power-bi/connect-data/service-r-packages-support
# The difficulty with such old versions of R and packages as of June 2023 is that 
# many of the options for installing older versions of packages in themselves
# do not work. devtools fails to install due to a range of dependencies. 
# Various versions of the groundhog package also fail to work. 
# The following approach of specifying a range of key packages and installing 
# specified versions directly from CRAN seems to be the most robust approach. 
# Order of packages is very important as some are dependencies of others and the 
# dependencies are not installed automatically. 
# Rtools will also need to be installed - version 34 is the one that works with R3.4.4
# https://cran.r-project.org/bin/windows/Rtools/history.html

packageurls <- c(
  "http://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_1.0.1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.4.1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/ellipsis/ellipsis_0.3.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/backports/backports_1.1.2.tar.gz",
  "https://cran.r-project.org/src/contrib/zeallot_0.1.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/vctrs/vctrs_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/pillar/pillar_1.4.2.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/purrr/purrr_0.3.3.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/assertthat/assertthat_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/tibble/tibble_2.1.1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/tidyselect/tidyselect_0.2.5.tar.gz",
  "https://cran.r-project.org/src/contrib/plogr_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/BH/BH_1.66.0-1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.8.3.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/lifecycle/lifecycle_0.1.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/tidyr/tidyr_1.0.0.tar.gz",

  "http://cran.r-project.org/src/contrib/Archive/stringr/stringr_1.3.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/lubridate/lubridate_1.7.2.tar.gz",

  "http://cran.r-project.org/src/contrib/Archive/htmltools/htmltools_0.3.6.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/htmlwidgets/htmlwidgets_1.3.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/lazyeval/lazyeval_0.2.1.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/httpuv/httpuv_1.3.6.2.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/shiny/shiny_1.0.5.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/colorspace/colorspace_1.3-2.tar.gz",
  "https://cran.r-project.org/src/contrib/munsell_0.5.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/scales/scales_1.0.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/gtable/gtable_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/testthat/testthat_2.0.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/isoband/isoband_0.2.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_3.3.3.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/crosstalk/crosstalk_1.0.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/DT/DT_0.4.tar.gz",

  "http://cran.r-project.org/src/contrib/Archive/data.table/data.table_1.12.6.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/later/later_1.0.0.tar.gz",
  "http://cran.r-project.org/src/contrib/Archive/promises/promises_1.1.0.tar.gz",

    "http://cran.r-project.org/src/contrib/Archive/openssl/openssl_1.0.1.tar.gz",
    "http://cran.r-project.org/src/contrib/Archive/httr/httr_1.3.1.tar.gz",
    "http://cran.r-project.org/src/contrib/Archive/hexbin/hexbin_1.27.2.tar.gz",
    "http://cran.r-project.org/src/contrib/Archive/plotly/plotly_4.9.2.2.tar.gz"
)

for (packageurl in packageurls) {
  install.packages(packageurl, repos=NULL, type="source")
}