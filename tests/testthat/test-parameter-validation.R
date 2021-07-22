library(NHSRplotthedots)

# options$pointSize parameter
# test_that("error when options$pointSize is not a number", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     pointSize = "not a number"
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$pointSize is less than or equal to zero", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     pointSize = 0
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$pointSize is greater than 10", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     pointSize = 10.1
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$percentageYAxis parameter
# test_that("error when options$percentageYAxis is an invalid non-logical value", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     percentageYAxis = "yes"
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$percentageYAxis is 1 or greater", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     percentageYAxis = 1
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$percentageYAxis is 0 or less", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     percentageYAxis = 0
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$mainTitle parameter
# test_that("error when options$mainTitle is not of type 'character'", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     mainTitle = 999
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$mainTitle is not a vector of length 1", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     mainTitle = c("New Title", "invalid 2nd item in vector")
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$xAxisLabel parameter
# test_that("error when options$xAxisLabel is not of type 'character'", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     xAxisLabel = 999
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$xAxisLabel is not a vector of length 1", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     xAxisLabel = c("New X Axis Label", "invalid 2nd item in vector")
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$yAxisLabel parameter
# test_that("error when options$yAxisLabel is not of type 'character'", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     yAxisLabel = 999
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$yAxisLabel is not a vector of length 1", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     yAxisLabel = c("New Y Axis Label", "invalid 2nd item in vector")
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$fixedXAxisMultiple parameter
# test_that("error when options$fixedXAxisMultiple is not TRUE or FALSE", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     fixedXAxisMultiple = 1
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$fixedYAxisMultiple parameter
# test_that("error when options$fixedYAxisMultiple is not TRUE or FALSE", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     fixedYAxisMultiple = 0
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$xAxisDateFormat parameter
# test_that("error when options$xAxisDateFormat is not of type 'character", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     xAxisDateFormat = 999
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$xAxisBreaks parameter
# test_that("error when options$xAxisBreaks is not of type 'character", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     xAxisBreaks = "test"
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$xAxisBreaks is not a valid string for seq.Date 'by'.", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     xAxisBreaks = "1 blue moon"
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# options$yAxisBreaks parameter
# test_that("error when options$yAxisBreaks is not of type 'numeric'", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     yAxisBreaks = "not numeric"
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })

# test_that("error when options$yAxisBreaks is not a vector of length 1", {
#   # arrange
#   data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
#   date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
#   df <- tibble(data, date)
#   options <- list(
#     yAxisBreaks = c(10, 20)
#   )
#
#   # act
#   p <- function() spc(df, "data", "date", options = options)
#
#   # assert
#   expect_error(p(), "spc:")
# })
