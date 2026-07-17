#' Combine binocular data into single X/Y coordinate pairs
#'
#' Combines the data from binocular samples into X/Y coordinate pairs. Two
#' methods can be used: "average" or "best_eye". For "average", the result is based on the average of the two eyes for each sample,
#' or for samples where there is data from only a single eye, that eye is used. For "best_eye", a summary of the proportion of missing samples
#' is computed, and the eye with the fewest missing samples is used.
#'
#' @param data raw data with columns time, left_x, left_y, right_x, right_y, and trial
#' @param method either "average" or "best_eye" - see description.
#' @param progress whether to provide a progress bar or not.
#' @return a dataframe of x-2 variables (with left_x and right_x condensed to x, and left_y and right_y condensed to y) and the same number of observations as the input data
#' @export
#'
#' @examples combine_eyes(HCL, method = "average")
#'

combine_eyes <- function(data, method = "average", progress = TRUE) {
  
  # check data format
  .check_binocular_data_format(data)
  
  if (!method %in% c("average", "best_eye")) {
    stop("Unexpected input to parameter 'method'. Use 'average' or 'best_eye'.")
  }
  
  if (method == "average") {
    if (progress) {
      xy <- pbapply::pbapply(
        data[, c("left_x", "right_x", "left_y", "right_y")],
        1,
        function(row) {
          c(mean(row[1:2], na.rm = TRUE), mean(row[3:4], na.rm = TRUE))
        }
      )
      x <- xy[1, ]
      y <- xy[2, ]
    } else {
      x <- rowMeans(data[, c("left_x", "right_x")], na.rm = TRUE)
      y <- rowMeans(data[, c("left_y", "right_y")], na.rm = TRUE)
    }
    
  } else if (method == "best_eye") {
    use_left <- mean(is.na(data$left_x)) < mean(is.na(data$right_x))
    cols <- if (use_left) c("left_x", "left_y") else c("right_x", "right_y")
    
    if (progress) {
      xy <- pbapply::pbapply(data[, cols], 1, identity)
      x <- xy[1, ]
      y <- xy[2, ]
    } else {
      x <- data[[cols[1]]]
      y <- data[[cols[2]]]
    }
  }
  
  # cleanup (only written once)
  data <- within(data, {
    left_x <- left_y <- right_x <- right_y <- NULL
  })
  
  data <- cbind(data, x, y)
  data[data == "NaN"] <- NA
  data <- data.frame(data)
  
  return(data)
}
