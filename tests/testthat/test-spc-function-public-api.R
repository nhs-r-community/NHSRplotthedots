library(NHSRplotthedots)

test_that("spc function can create a ggplot", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  #act
  result <- suppressMessages(spc(df, "data", "date"))

  #assert
  expect_s3_class(result,"ggplot")
  expect_identical(result$labels$title, "SPC Chart")
  expect_identical(result$labels$x, "Date")
  expect_identical(result$labels$y, "Data")
  expect_identical(ggplot_build(result)$layout$panel_scales_x[[1]]$labels[[1]], "22/03/2021") #default date format
})

test_that("spc function can create a faceted ggplot", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 24)
  category <- c(rep("Category A", times = 12), rep("Category B", times = 12))
  df <- tibble(data, date, category)

  #act
  result <- suppressMessages(spc(df, "data", "date", "category"))

  #assert
  expect_s3_class(result,"ggplot")

  expect_identical(ggplot_build(result)$layout$panel_params %>% length(), 2L) #there should be 2 facet panels

  plotOneXRange <- ggplot_build(result)$layout$panel_params[[1]]$x$continuous_range
  plotTwoXRange <- ggplot_build(result)$layout$panel_params[[2]]$x$continuous_range
  expect_equal(plotOneXRange, plotTwoXRange) #by default the x axis range is the same on all facets

  plotOneYRange <- ggplot_build(result)$layout$panel_params[[1]]$y$continuous_range
  plotTwoYRange <- ggplot_build(result)$layout$panel_params[[2]]$y$continuous_range
  expect_equal(plotOneYRange, plotTwoYRange) #by default the y axis range is the same on all facets
})

test_that("spc function returns a dataframe when options$outputChart is FALSE", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(outputChart = FALSE)

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"tbl")
})

test_that("ggplot title and axis labels can be modified with options", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    mainTitle = "New Plot Title",
    xAxisLabel = "New X Label",
    yAxisLabel = "New Y Label"
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")
  expect_identical(result$labels$title, "New Plot Title")
  expect_identical(result$labels$x, "New X Label")
  expect_identical(result$labels$y, "New Y Label")
})

test_that("limits can be rebased at an intervention point", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  intervention <- c(0,0,0,1,0,0,0,0,0,0,0,0)
  df <- tibble(data, date, intervention)
  options = list(
    rebase = "intervention"
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertions for limit recalculations in data frame and visible lines on plot
})

test_that("limits can be rebased at multiple intervention points", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  intervention <- c(0,0,0,1,0,0,0,0,0,1,0,0)
  df <- tibble(data, date, intervention)
  options = list(
    rebase = "intervention"
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertions for limit recalculations in data frame and visible lines on plot
})

test_that("plotting point size can be adjusted", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    pointSize = 4
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertion for line and point size changes
})

test_that("improvement direction can be set as 'decrease'", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    improvementDirection = "decrease"
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")
  #TODO add assertion for point colours
})

test_that("y axis values can be set as percentages using 'percentageYAxis = TRUE'", {
  #arrange
  data <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.8,0.7,0.6)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    percentageYAxis = TRUE
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  firstYLabel <- ggplot_build(result)$layout$panel_scales_y[[1]]$break_info()$labels[[1]]
  expect_identical(grepl("%", firstYLabel, fixed = TRUE), TRUE) #check there is a % sign in the first y axis label
})

test_that("y axis values can be set as percentages using 'percentageYAxis = {decimal axis break value}'", {
  #arrange
  data <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.8,0.7,0.6)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)
  options = list(
    percentageYAxis = 0.25
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  firstYLabel <- ggplot_build(result)$layout$panel_scales_y[[1]]$break_info()$labels[[1]]
  expect_identical(grepl("%", firstYLabel, fixed = TRUE), TRUE) #check there is a % sign in the first y axis label
})

test_that("a target line can be added to the plot", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  target <- rep(15, times = 12)
  df <- tibble(data, date, target)
  options = list(
    target = "target"
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  expect_identical(quo_name(result$layers[[3]]$mapping[[1]]), "target") #confirm layer 3 is "target" layer
  expect_identical(ggplot_build(result)$data[[3]]$y %>% is.na() %>% sum(), 0L ) #zero NAs in the target line data
})

test_that("a trajectory line can be added to the plot", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  trajectory <- seq(from = 0.1, to = 5, length.out = 12)
  df <- tibble(data, date, trajectory)
  options = list(
    trajectory = "trajectory"
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  expect_identical(quo_name(result$layers[[4]]$mapping[[1]]), "trajectory") #confirm layer 4 is "trajectory" layer
  expect_identical(ggplot_build(result)$data[[4]]$y %>% is.na() %>% sum(), 0L ) #zero NAs in the trajectory data
})

test_that("facet plots can be plotted with different x axis ranges", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 24)
  category <- c(rep("Category A", times = 12), rep("Category B", times = 12))
  df <- tibble(data, date, category)
  options = list(
    fixedXAxisMultiple = FALSE
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", "category", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  plotOneXRange <- ggplot_build(result)$layout$panel_params[[1]]$x$continuous_range
  plotTwoXRange <- ggplot_build(result)$layout$panel_params[[2]]$x$continuous_range
  expect_false(plotOneXRange[[1]] == plotTwoXRange[[1]]) #expect the x axis ranges to be different
})

test_that("facet plots can be plotted with different y axis ranges", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)
  date <- rep(seq(as.Date("2021-03-22"), by = 1, length.out = 12), times = 2)
  category <- c(rep("Category A", times = 12), rep("Category B", times = 12))
  df <- tibble(data, date, category)
  options = list(
    fixedYAxisMultiple = FALSE
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", "category", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  plotOneYRange <- ggplot_build(result)$layout$panel_params[[1]]$y$continuous_range
  plotTwoYRange <- ggplot_build(result)$layout$panel_params[[2]]$y$continuous_range
  expect_false(plotOneYRange[[1]] == plotTwoYRange[[1]]) #expect the y axis ranges to be different
})

test_that("x axis date format can be specified", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  options <- list(
    xAxisDateFormat = "%d%b"
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  expect_identical(ggplot_build(result)$layout$panel_scales_x[[1]]$labels[[1]], "22Mar") #new date format
})

test_that("x axis breaks can be specified", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  options <- list(
    xAxisBreaks = "1 week"
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  firstXLabel <- ggplot_build(result)$layout$panel_scales_x[[1]]$labels[[1]]
  secondXLabel <- ggplot_build(result)$layout$panel_scales_x[[1]]$labels[[2]]
  expect_identical(firstXLabel, "22/03/2021") #first label (as default)
  expect_identical(secondXLabel, "29/03/2021") #second label is one week later
})

test_that("y axis breaks can be specified", {
  #arrange
  data <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  date <- seq(as.Date("2021-03-22"), by = 1, length.out = 12)
  df <- tibble(data, date)

  options <- list(
    yAxisBreaks = 2
  )

  #act
  result <- suppressMessages(spc(df, "data", "date", options = options))

  #assert
  expect_s3_class(result,"ggplot")

  firstYLabel <- ggplot_build(result)$layout$panel_scales_y[[1]]$break_info()$labels[[1]] %>% as.numeric()
  secondYLabel <- ggplot_build(result)$layout$panel_scales_y[[1]]$break_info()$labels[[2]] %>% as.numeric()
  expect_equal(secondYLabel - firstYLabel, 2) #the difference between labels should be 2
})
