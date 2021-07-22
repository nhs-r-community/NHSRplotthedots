library(NHSRplotthedots)

# data.frame parameter
test_that("error when df is not a data.frame", {
  # arrange
  df <- "not a data.frame"

  # act
  p <- function() spc(df, "data", "date")
  # assert
  expect_error(p(), "spc:")
})

# valueField parameter
test_that("error when valueField is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  # act
  p <- function() spc(df, 999, "date")

  # assert
  expect_error(p(), "spc:")
})

test_that("error when valueField is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  # act
  p <- function() spc(df, c("data", "invalid 2nd item in vector"), "date")

  # assert
  expect_error(p(), "spc:")
})

test_that("error when valueField is not a column in the data frame", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  # act
  p <- function() spc(df, "this data column isnt in the data frame", "date")

  # assert
  expect_error(p(), "spc:")
})

# dateField parameter
test_that("error when dateField is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  # act
  p <- function() spc(df, "data", 999)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when dateField is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  # act
  p <- function() spc(df, "data", c("date", "invalid 2nd item in vector"))

  # assert
  expect_error(p(), "spc:")
})

test_that("error when dateField is not a column in the data frame", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  # act
  p <- function() spc(df, "data", "this date column isnt in the data frame")

  # assert
  expect_error(p(), "spc:")
})

# facetField parameter
test_that("error when facetField is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 24)
  category <- c(rep("Category A", times = 12), rep("Category B", times = 12))
  df <- tibble(data, date, category)

  # act
  p <- function() spc(df, "data", "date", 999)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when facetField is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 24)
  category <- c(rep("Category A", times = 12), rep("Category B", times = 12))
  df <- tibble(data, date, category)

  # act
  p <- function() spc(df, "data", "date", c("category", "invalid 2nd item in vector"))

  # assert
  expect_error(p(), "spc:")
})

test_that("error when facetField is not a column in the data frame", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 24)
  category <- c(rep("Category A", times = 12), rep("Category B", times = 12))
  df <- tibble(data, date)

  # act
  p <- function() spc(df, "data", "date", "this date column isnt in the data frame")

  # assert
  expect_error(p(), "spc:")
})

# options
test_that("error when options is not of type 'list'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  # act
  p <- function() spc(df, "data", "date", options <- "this is not a list")

  # assert
  expect_error(p(), "spc:")
})

# options$rebase parameter
test_that("error when options$rebase is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  rebaseField <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0)
  df <- tibble(data, date, rebaseField)
  options <- list(
    rebase = 999
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$rebase is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  rebaseField <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0)
  df <- tibble(data, date, rebaseField)
  options <- list(
    rebase = c("rebaseField", "invalid 2nd item in vector")
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$rebase is not a column in the data frame", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  rebaseField <- c(0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0)
  df <- tibble(data, date, rebaseField)
  options <- list(
    rebase = "this rebase column isnt in the data frame"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$improvementDirection parameter
test_that("error when options$improvementDirection is an invalid character vector", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    improvementDirection = "invalid entry"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$improvementDirection is an invalid number", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    improvementDirection = 2
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$outputChart parameter
test_that("error when options$ouputChart is not TRUE or FALSE", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    outputChart = 1
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$pointSize parameter
test_that("error when options$pointSize is not a number", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    pointSize = "not a number"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$pointSize is less than or equal to zero", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    pointSize = 0
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$pointSize is greater than 10", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    pointSize = 10.1
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$percentageYAxis parameter
test_that("error when options$percentageYAxis is an invalid non-logical value", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    percentageYAxis = "yes"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$percentageYAxis is 1 or greater", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    percentageYAxis = 1
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$percentageYAxis is 0 or less", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    percentageYAxis = 0
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$target parameter
test_that("error when options$target is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  target <- rep(20, times = 12)
  df <- tibble(data, date, target)
  options <- list(
    target = 999
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$target is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  target <- rep(20, times = 12)
  df <- tibble(data, date, target)
  options <- list(
    target = c("target", "invalid 2nd item in vector")
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$target is not a column in the data frame", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  target <- rep(20, times = 12)
  df <- tibble(data, date, target)
  options <- list(
    target = "this target column isnt in the data frame"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$trajectory parameter
test_that("error when options$trajectory is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  trajectory <- rep(20, times = 12)
  df <- tibble(data, date, trajectory)
  options <- list(
    trajectory = 999
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$trajectory is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  trajectory <- rep(20, times = 12)
  df <- tibble(data, date, trajectory)
  options <- list(
    trajectory = c("trajectory", "invalid 2nd item in vector")
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$trajectory is not a column in the data frame", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  trajectory <- rep(20, times = 12)
  df <- tibble(data, date, trajectory)
  options <- list(
    trajectory = "this trajectory column isnt in the data frame"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$mainTitle parameter
test_that("error when options$mainTitle is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    mainTitle = 999
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$mainTitle is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    mainTitle = c("New Title", "invalid 2nd item in vector")
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$xAxisLabel parameter
test_that("error when options$xAxisLabel is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    xAxisLabel = 999
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$xAxisLabel is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    xAxisLabel = c("New X Axis Label", "invalid 2nd item in vector")
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$yAxisLabel parameter
test_that("error when options$yAxisLabel is not of type 'character'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    yAxisLabel = 999
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$yAxisLabel is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    yAxisLabel = c("New Y Axis Label", "invalid 2nd item in vector")
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$fixedXAxisMultiple parameter
test_that("error when options$fixedXAxisMultiple is not TRUE or FALSE", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    fixedXAxisMultiple = 1
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$fixedYAxisMultiple parameter
test_that("error when options$fixedYAxisMultiple is not TRUE or FALSE", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    fixedYAxisMultiple = 0
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$xAxisDateFormat parameter
test_that("error when options$xAxisDateFormat is not of type 'character", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    xAxisDateFormat = 999
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$xAxisBreaks parameter
test_that("error when options$xAxisBreaks is not of type 'character", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    xAxisBreaks = "test"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$xAxisBreaks is not a valid string for seq.Date 'by'.", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    xAxisBreaks = "1 blue moon"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

# options$yAxisBreaks parameter
test_that("error when options$yAxisBreaks is not of type 'numeric'", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    yAxisBreaks = "not numeric"
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})

test_that("error when options$yAxisBreaks is not a vector of length 1", {
  # arrange
  data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options <- list(
    yAxisBreaks = c(10, 20)
  )

  # act
  p <- function() spc(df, "data", "date", options = options)

  # assert
  expect_error(p(), "spc:")
})
