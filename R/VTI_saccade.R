#' Velocity threshold identification of saccades
#'
#' Takes a
#'
#' @param data
#' @param sample_rate sample rate of the eye-tracker. If default of NULL, then it will be computed from the timestamp data and the number of samples
#'
#' @return
#' @export
#'
#' @examples VTI_saccade()
#'

VTI_saccade <- function(data, sample_rate = NULL, ...){

  if (is.null(sample_rate)) {
    # estimate sample rate
    time <- data$time[nrow(data)] - data$time[1]
    sample_rate <- 1000/(time/nrow(data)) # total time taken / samples

  }


  x <- data$x
  y <- data$y

  d <- as.matrix(dist(cbind(x,y)))

  d_diag <- diag(d[2:nrow(d),])

  data_new <- cbind(data,
                    distance = c(NA,d_diag))

  data_new$distance <- dist_to_visual_angle(data_new$distance, ...)

  data_new$vel <- data_new$distance*sample_rate # visual angle per second

  return(data_new)


}
