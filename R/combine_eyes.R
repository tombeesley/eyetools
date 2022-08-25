#' Combine binocular data into single X/Y coordinate pairs
#'
#' Combines the data from binocular samples into X/Y coordinate pairs. Two
#' methods can be used: "average" or "best_eye". For "average", the result is based on the average of the two eyes for each sample,
#' or for samples where there is data from only a single eye, that eye is used. For "best_eye", a summary of the proportion of missing samples
#' is computed, and the eye with the fewest missing samples is used.
#'
#' @param data raw data with columns time, left_x, left_y, right_x, right_y, and trial
#' @param method either "average" or "best_eye" - see description.
#'
#' @return
#' @export
#'
#' @examples combine_eyes(example_raw_binocular, method = "average")
#'

combine_eyes <- function(data, method = "average") {

  if (method == "average") {

    x <- rowMeans(data[,c('left_x','right_x')], na.rm = TRUE)
    y <- rowMeans(data[,c('left_y','right_y')], na.rm = TRUE)

  } else if (method == "best_eye") {

    if (mean(is.na(data$left_x)) < mean(is.na(data$right_x))){
      # left eye has fewer NAs
      x <- data$left_x
      y <- data$left_y
    } else {
      # right eye has fewer NAs
      x <- data$right_x
      y <- data$right_y
    }

  } else {

    stop("Unexpected input to parameter 'method'. Use 'avergage' or 'best_eye'.")

  }

  data <- cbind(data$time, x, y, data$trial)

  data[data == 'NaN']=NA # convert any NaN (from mean()) to NA

  colnames(data) <- c("time", "x", "y", "trial")

  data <- data.frame(data)

  return(data)

}
