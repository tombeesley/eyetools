#' conditional_transform
#'
#' A function to counterbalance trials. The function takes the dataframe and performs a single axis flip to correct counterbalancing.
#'
#' @param data a dataframe that includes columns of time, x, y, trial, and cue_order. Cue order should currently be in the state of 1,2 which refer to the counterbalancing of trials.
#' @param flip either x or y to specify a flip across either the vertical axis or horizontal axis
#' @param resolution_x screen size in pixels for the x axis
#' @param resolution_y screen size in pixels for the y axis
#' @return
#'
#' @examples
#' \dontrun {
#' example_raw_HCL <- example_raw_HCL |> mutate(x = ifelse(x > 1920, NA, x),
#' y = ifelse(y > 1080, NA, y))
#' data <- example_raw_HCL |> left_join(example_resp_HCL, by = join_by(trial))
#' conditional_transform(data, flip = "x")}
#'
#'

conditional_transform <- function(data, flip = c("x", "y"), resolution_x = 1920, resolution_y = 1080) {

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

  #perform main action
  if (flip == "x") {
    message("Flipping from left to right")

    data[data$cue_order == 2,]$x <- resolution_x-data[data$cue_order == 2,]$x

  }

  if (flip == "y") {
    message("Flipping from top to bottom")

    data[data$cue_order == 2,]$y <- resolution_y-data[data$cue_order == 2,]$y

  }

  return(data)
}

### Needs a catch for if the resolution is smaller than the maximum x value
