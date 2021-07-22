#' Validate parameters (internal function)
#'
#' Checks that all parameters passed to the main spc() function are as expected, and throws errors if not.
#'   All parameters are as passed by the user.
#'
#' @param df A data frame containing the information to be plotted.
#' @param valueField A character vector naming the column in the data frame to plot on the y axis
#' @param dateField A character vector naming the column in the data frame to plot on the x axis
#' @param facetField (optional) A character vector naming the column in the data frame containing the facet names
#' @param options (optional) A list containing detailed customisation options
#'
#' @noRd

validateParameters <- function(df, valueField, dateField, facetField, options) {

  # Check that data.frame argument is a data frame
  if (!(is.data.frame(df))) stop("spc: Data.frame argument must be of type 'data.frame'.")

  # if provided, options$pointSize must be a number between 0 and 10
  if (!is.null(options$pointSize)) {
    if (!is.numeric(options$pointSize)) {
      stop("spc: options$pointSize argument must be a number.")
    }
    if (options$pointSize <= 0 || options$pointSize > 10) {
      stop("spc: options$pointSize argument must greater than 0 and less than or equal to 10.")
    }
  }

  # if provided, options$percentageYAxis must be TRUE, FALSE, or a decimal value between 0 and 1
  # TODO: should these be < and >?
  if (!is.null(options$percentageYAxis) && !is.logical(options$percentageYAxis) && (
    options$percentageYAxis <= 0 || options$percentageYAxis >= 1)
  ) {
    stop("spc: options$percentageYAxis argument must be TRUE, FALSE, or a decimal value between 0 and 1.")
  }

  # if provided, options$mainTitle must be of character type, and length 1
  if (!is.null(options$mainTitle)) {
    if (!is.character(options$mainTitle)) stop("spc: options$mainTitle argument must be of type 'character'.")
    if (length(options$mainTitle) > 1) stop("spc: options$mainTitle argument must be a vector of length 1.")
  }

  # if provided, options$xAxisLabel must be of character type, and length 1
  if (!is.null(options$xAxisLabel)) {
    if (!is.character(options$xAxisLabel)) stop("spc: options$xAxisLabel argument must be of type 'character'.")
    if (length(options$xAxisLabel) > 1) stop("spc: options$xAxisLabel argument must be a vector of length 1.")
  }

  # if provided, options$yAxisLabel must be of character type, and length 1
  if (!is.null(options$yAxisLabel)) {
    if (!is.character(options$yAxisLabel)) stop("spc: options$yAxisLabel argument must be of type 'character'.")
    if (length(options$yAxisLabel) > 1) stop("spc: options$yAxisLabel argument must be a vector of length 1.")
  }

  # if provided, options$fixedXAxisMultiple must be TRUE or FALSE
  if (!is.null(options$fixedXAxisMultiple) && !is.logical(options$fixedXAxisMultiple)) {
    stop("spc: options$fixedXAxisMultiple argument must be TRUE or FALSE.")
  }

  # if provided, options$fixedYAxisMultiple must be TRUE or FALSE
  if (!is.null(options$fixedYAxisMultiple) && !is.logical(options$fixedYAxisMultiple)) {
    stop("spc: options$fixedYAxisMultiple argument must be TRUE or FALSE.")
  }

  # if provided, options$xAxisDateFormat must be of character type
  if (!is.null(options$xAxisDateFormat) && !is.character(options$xAxisDateFormat)) {
    stop("spc: options$xAxisDateFormat argument must be of type 'character'. Try ?strptime for formatting hints.")
  }

  # if provided, options$xAxisBreaks must be of character type, and be a vaoid string for seq.Date() 'by' argument
  if (!is.null(options$xAxisBreaks) && (
    !is.character(options$xAxisBreaks) ||
      !grepl("^\\d+ (day|week|month|quarter|year)s?$", options$xAxisBreaks)
  )) {
    stop(
      "spc: options$xAxisBreaks argument must be of type 'character', and be a valid string for seq.Date 'by'. ",
      "See seq.Date for more information."
    )
  }

  # if provided, options$yAxisBreaks must be of character type, length 1, and a column name from within the data frame
  if (!is.null(options$yAxisBreaks)) {
    if (!is.numeric(options$yAxisBreaks)) stop("spc: options$yAxisBreaks argument must be of type 'numeric'.")
    if (length(options$yAxisBreaks) > 1) stop("spc: options$yAxisBreaks argument must be a vector of length 1.")
  }
}
