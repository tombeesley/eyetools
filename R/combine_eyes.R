#' Combine binocular data into single X/Y coordinate pairs
#'
#' Combines the data from binocular samples into X/Y coordinate pairs. Two
#' methods can be used: "average" or "best_eye". For "average", the result is based on the average of the two eyes for each sample,
#' where the data from a single eye is taken is the other is missing (NA). For "best_eye", a summary of the proportion of missing samples
#' is computed, and the eye with the fewest missing samples is used.
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#'

combine_eyes <- function(data, method = "average") {

  if (method == "average") {

    x <- rowMeans(data[,c('left_x','right_x')], na.rm = TRUE)
    y <- rowMeans(data[,c('left_y','right_y')], na.rm = TRUE)

  } else if (method == "best_eye") {

    if (mean(is.na(d$left_x)) < mean(is.na(d$right_x))){
      # left eye has fewer NAs
      x <- data$left_x
      y <- data$left_y
    } else {
      # right eye has fewer NAs
      x <- data$right_x
      y <- data$right_y
    }

  }

  data <- cbind(data$time, x, y, data$trial, data$trial_phase)

  # need to fix that this is returning NaN and not NA for the average method


  return(data)

}
