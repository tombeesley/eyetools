#' conditional_transform
#'
#' A function to perform conditional transformations of the x/y raw data.
#' The function takes the dataframe and performs a single axis flip based on the values specified in the cond_column.
#' The primary use of this function is to correct or normalise the data when counterbalancing stimulus placement within experiments (e.g., having a target stimulus appear on the left and right equally often)
#' The screen width and height in pixels can be set using tracker_properties_set().
#'
#' @param data a dataframe that includes columns x and y and the column specified in cond_column. Can be raw, fixation, or saccade data.
#' @param cond_column a column name, on which the flips are conditional
#' @param cond_values a single value or vector stating which values in con_column result in a flip
#' @param flip either "x", to flip across vertical midline, or "y" to flip across horizontal midline
#' @param message whether to output messages during function. Useful to turn off when using in a vectorised fashion where it is running multiple times
#'
#' @return a dataframe of the equivalent format as the input data
#' @export
#'
#' @examples
#' data <- combine_eyes(HCL)
#' data <- merge(data, HCL_behavioural)
#' conditional_transform(data, flip = "x",
#'                       cond_column = "cue_order",
#'                       cond_values = 2)

conditional_transform <- function(data, flip = c("x", "y"), cond_column, cond_values, message = TRUE) {
  
  if(missing(cond_column)) {
    stop("missing column names. Please check your data and the documentation")
  }

  if(sum(colnames(data) %in% c("x", "y", cond_column)) != 3) {
    stop("missing column names. Please check your data and the documentation")
  }
  #if flip is not specified
  if (missing(flip)) {
    stop("counterbalancing is either x (across vertical axis) or y (across horizontal axis). Please provide a value for flip.")

  }
  if (!(flip %in% c("x", "y"))) { #if not x or y
    stop("counterbalancing is either x (across vertical axis) or y (across horizontal axis)")
  }

  #if the screen size is smaller than the observations
  if (the$eyetracker_properties$screen_width_pixels < max(data$x, na.rm = TRUE)) {
    stop("screen resolution is smaller than the greatest value of x. Please check data or update default values of screen resolution")
  }
  if (the$eyetracker_properties$screen_height_pixels < max(data$y, na.rm = TRUE)) {
    stop("screen resolution is smaller than the greatest value of x. Please check data or update default values of screen resolution")
  }

  #perform main action
  if (flip == "x") {
    if (message == TRUE) {
    message("Flipping across x midline")}

    data[data[[cond_column]] %in% cond_values,]$x <- the$eyetracker_properties$screen_width_pixels-data[data[[cond_column]] %in% cond_values,]$x

  }

  if (flip == "y") {
    if (message == TRUE) {
      message("Flipping across y midline")}

    data[data[[cond_column]] %in% cond_values,]$y <- the$eyetracker_properties$screen_height_pixels-data[data[[cond_column]] %in% cond_values,]$y

  }

  return(data)
}
