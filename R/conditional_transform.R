#' conditional_transform
#'
#' A function to perform conditional transformations of the x/y raw data.
#' The function takes the dataframe and performs a single axis flip based on the values specificed in the cond_column.
#' The primary use of this function is to correct or normalise the data when counterbalancing stimulus placement within experiments (e.g., having a target stimulus appear on the left and right equally often)
#'
#' @param data a dataframe that includes columns of time, x, y, trial, and cue_order. Cue order should currently be in the state of 1,2 which refer to the counterbalancing of trials.
#' @param cond_column a column name, on which the flips are conditional
#' @param cond_values which values in this column result in the flip
#' @param flip either x or y to specify a flip across either the vertical axis or horizontal axis
#' @param resolution_x screen size in pixels for the x axis
#' @param resolution_y screen size in pixels for the y axis
#'
#' @return a dataframe of the equivalent format as the input data
#' @export
#'
#' @examples
#' conditional_transform(example_counterbalance, flip = "x", cond_column = "cue_order", cond_values = 2)
#'

conditional_transform <- function(data, flip = c("x", "y"), cond_column, cond_values, resolution_x = 1920, resolution_y = 1080) {

  if(sum(colnames(data) %in% c("time", "x", "y", "trial", "cue_order")) != 5) {
    stop("missing column names. Please check your data")
  }
  #if flip is not specified
  if (missing(flip)) {
    stop("counterbalancing is either x (across vertical axis) or y (across horizontal axis). Please provide a value for flip.")

  }
  if (!(flip %in% c("x", "y"))) { #if not v or h
    stop("counterbalancing is either x (across vertical axis) or y (across horizontal axis)")
  }

  if (resolution_x < max(data$x)) {
    stop("screen resolution is smaller than the greatest value of x. Please check data or update default values of screen resolution")
  }
  if (resolution_y < max(data$y)) {
    stop("screen resolution is smaller than the greatest value of x. Please check data or update default values of screen resolution")
  }

  #perform main action
  if (flip == "x") {
    message("Flipping from left to right")

    data[data[[cond_column]] %in% cond_values,]$x <- resolution_x-data[data[[cond_column]] %in% cond_values,]$x

  }

  if (flip == "y") {
    message("Flipping from top to bottom")

    data[data[[cond_column]] %in% cond_values,]$y <- resolution_y-data[data[[cond_column]] %in% cond_values,]$y

  }

  return(data)
}
