#' SPC Standard Calculations (internal function)
#'
#' Returns a data frame containing SPC fields which are common to all methodologies, including 'plot the dots'
#'
#' This function is designed to produce consistent SPC charts
#' across Information Department reporting, according to the 'plot the dots'
#' logic produced by NHSI. The function can return either a plot or data frame.
#'
#'
#' @param .data A data frame containing a value field, a date field,
#' and a category field (if for faceting). There should be no gaps in the time series
#' for each category.
#' @param valueField Specify the field name which contains the value data, to be plotted on y axis.
#' @param dateField Specify the field name which contains the date data, to be plotted on x axis.
#' @param facetField Optional: Specify field name which contains a grouping/faceting variable. SPC logic will be applied
#'     to each group separately, with outputs combined. Currently accepts 1 variable only.
#' @param options Optional: A list object containing additional control and formatting properties. Preferably created
#'     using the spcOptions function.
#'
#' @noRd


#' @import dplyr
#' @importFrom rlang .data

spcStandard <- function(.data, valueField, dateField, facetField = NULL, options = NULL) {

  # Identify a rebase, trajectory and target fields, if provided in SPC options object
  rebaseField <- options$rebase
  trajectoryField <- options$trajectory[[1]]
  targetField <- options$target[[1]]

  # set trajectory field
  if (!(is.null(trajectoryField))) {
    .data$trajectory <- .data[[trajectoryField]]
  } else {
    .data$trajectory <- rep(as.numeric(NA), nrow(.data))
  }

  # Set target field or create pseudo
  if (!(is.null(targetField))) {
    .data$target <- .data[[targetField]]
  } else {
    .data$target <- rep(as.numeric(NA), nrow(.data))
  }

  # Set facet/grouping or create pseudo
  # If no facet field specified, bind a pseudo-facet field for grouping/joining purposes
  if (facetField == "pseudo_facet_col_name") {
    .data <- mutate(.data, pseudo_facet_col_name = "no facet")
  }

  # Set rebase field or create pseudo
  if (!(is.null(rebaseField))) {
    if (rebaseField[[1]] %in% names(.data)) {
      .data$rebase <- .data[[rebaseField[[1]]]]
    } else if (all(isDate(rebaseField))) {
      dates <- .data[[dateField]]
      .data$rebase <- as.integer(as.Date(dates) %in% as.Date(rebaseField))
    }
  } else {
    .data$rebase <- rep(0, nrow(.data))
  }

  # Check validity of rebase field supplied - should be 1s and 0s only -
  # QUESTION: should this go into the validateParameters function? Or not as it's validating data?
  dferrors <- .data$rebase[.data$rebase != 1 & .data$rebase != 0]
  if (length(dferrors) > 0) stop("spc: options$rebase argument must define a field containing only 0 or 1 values.")

  ## Constants ----
  limit <- 2.66
  limitclose <- 2 * (limit / 3)

  # Restructure starting data frame
  .data <- .data %>%
    select(
      y = all_of(valueField),
      x = all_of(dateField),
      f = all_of(facetField),
      rebase = .data$rebase,
      trajectory = .data$trajectory,
      target = .data$target
    ) %>%
    # Group data frame by facet
    group_by(.data$f) %>%
    # Order data frame by facet, and x axis variable
    arrange(.data$f, .data$x) %>%
    # Add an index to each facet group
    mutate(n = row_number()) %>%
    # Ungroup for tidiness
    ungroup() %>%
    mutate(
      movingrange = case_when(
        # Add moving range, as used for basis of sigma in 'plot the dots' logic
        .data$n > 1 ~ abs(.data$y - lag(.data$y, 1))
      )
    )

  # Identify facets/indices which have been flagged for rebasing
  rebaseTable <- .data %>%
    filter(.data$rebase == 1) %>%
    select(.data$f, .data$x)

  # Identify the earliest x axis point for each facet - start of rebase group 1
  rebaseTable2 <- .data %>%
    group_by(.data$f) %>%
    summarise(x = min(.data$x))

  # Identify the latest x axis point for each facet - end of rebase group n
  rebaseTable3 <- .data %>%
    group_by(.data$f) %>%
    summarise(x = max(.data$x))

  # Create a data frame of rebase intervals, with first and last x axis points and all intervening x axis points which
  # will trigger a new rebase period
  rebaseTable4 <- rbind(rebaseTable, rebaseTable2, rebaseTable3) %>%
    # order by facet and x axis
    arrange(.data$f, .data$x) %>%
    # group by facet
    group_by(.data$f) %>%
    # add a row number which will create an index for each rebase interval within the group,
    # and identify the starting x axis coordinate for each index, using lag function
    mutate(rn = row_number(), start = lag(.data$x, 1)) %>%
    # ungroup for tidiness
    ungroup() %>%
    # remove the first rebase row from each facet, as this will have an end date but no start date
    filter(.data$rn != 1) %>%
    # limit data frame to each facet, and it's rebase start/end points
    select(.data$f, .data$start, end = .data$x) %>%
    # regroup by facet
    group_by(.data$f) %>%
    # recreate the row number index for each rebase interval within the group, now that the 1st row has been excluded
    mutate(rn = row_number()) %>%
    # reorder the data frame by facet and descending date order - to allow identification of last period in each facet
    arrange(.data$f, desc(.data$start)) %>%
    # add second row number to identify last rebase group in each facet
    mutate(rn2 = row_number()) %>%
    ungroup()

  # Join data frame to rebase groupings, and filter to x axis between grouping start (inclusive) and end (exclusive)
  # - note that this will omit the last row in each facet because it belongs to the previous rebase group
  df2 <- .data %>%
    left_join(rebaseTable4, by = c("f" = "f")) %>%
    filter(.data$x >= .data$start, .data$x < .data$end) %>%
    # set the first moving range value in each rebase group to NA, as this is not included in the new mean calculations
    mutate(movingrange = case_when(
      .data$x != .data$start ~ as.numeric(.data$movingrange),
      TRUE ~ as.numeric(NA)
    ))

  # Identify the last row in each facet, to add to the previous rebase group
  df3 <- .data %>%
    left_join(rebaseTable4, by = "f") %>%
    filter(.data$rn2 == 1, .data$x == .data$end)

  # Bind together last two data frames, so that all data points are present with a link to the appropriate rebase group
  .data <- bind_rows(df2, df3) %>%
    arrange(.data$f, .data$x) %>%
    select(
      .data$y,
      .data$x,
      .data$f,
      .data$rebase,
      .data$n,
      .data$target,
      .data$trajectory,
      .data$movingrange,
      rebaseGroup = .data$rn
    ) %>%
    group_by(.data$f, .data$rebaseGroup) %>%
    mutate(fixPointsRN = row_number())

  # Identify the mean and moving range average within each facet and rebase group
  if (is.null(options$fixAfterNPoints)) {
    ## If no point fix has been specified, find the largest number of points per facet/rebase
    fixAfterNPoints <- max(.data$n, na.rm = TRUE)
  } else {
    fixAfterNPoints <- options$fixAfterNPoints
  }

  dfAvg <- .data %>%
    ## Added to allow any rebase period to be fixed after N points
    filter(.data$fixPointsRN <= fixAfterNPoints) %>%
    group_by(.data$f, .data$rebaseGroup) %>%
    summarise(
      mean = mean(.data$y, na.rm = TRUE),
      movingrangeaverage = mean(.data$movingrange, na.rm = TRUE)
    )

  # Join data frame to moving range average and mean data, then perform standard logical tests
  .data %>%
    left_join(dfAvg, by = c("f", "rebaseGroup")) %>%
    mutate(
      # identify lower/upper process limits
      lpl = .data$mean - (limit * .data$movingrangeaverage),
      upl = .data$mean + (limit * .data$movingrangeaverage),
      # identify near lower/upper process limits
      nlpl = .data$mean - (limitclose * .data$movingrangeaverage),
      nupl = .data$mean + (limitclose * .data$movingrangeaverage),

      # Identify any points which are outside the upper or lower process limits
      outsideLimits = ifelse(.data$y > .data$upl | .data$y < .data$lpl, 1, 0),
      # Identify whether a point is above or below the mean
      relativeToMean = sign(.data$y - .data$mean),

      # Identify if a point is between the near process limits and process limits
      closeToLimits = case_when(
        .data$y > .data$nupl & .data$y <= .data$upl ~ 1,
        .data$y < .data$nlpl & .data$y >= .data$lpl ~ 1,
        TRUE ~ 0
      )
    )
}
