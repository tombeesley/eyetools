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

VTI_saccade <- function(data, sample_rate = NULL, threshold = 150, ...){

  if (is.null(sample_rate)) {
    # estimate sample rate
    time <- data$time[nrow(data)] - data$time[1]
    sample_rate <- 1000/(time/nrow(data)) # total time taken / samples

  }

  x <- data$x
  y <- data$y

  d <- as.matrix(dist(cbind(x,y)))

  d_diag <- diag(d[2:nrow(d),])

  data <- cbind(data,
                distance = c(NA,d_diag))

  data$distance <- dist_to_visual_angle(data$distance, ...) # convert to VisAng

  data$vel <- data$distance*sample_rate # visual angle per second

  data$saccade_detected <- ifelse(data$vel > threshold, 2, 1) # saccade 2, otherwise 1

  data$saccade_detected[is.na(data$saccade_detected)] <- 0 # convert NA to 0

  data$event_n <- c(NA,cumsum(abs(diff(data$saccade_detected)))) # get event numbers

  data <- data[data$saccade_detected == 2,] # get just the saccades

  # define function to pull out relevant data from saccades
  get_sac_info <- function(dataIn){

    fpos <- dataIn[1,2:3] # x and y of FIRST time stamp
    lpos <- dataIn[nrow(dataIn),2:3] # x and y of LAST time stamp
    meanVel <- mean(dataIn$vel) # mean velocity
    peakVel <- max(dataIn$vel) # peak velocity during saccade

    outData <- c(fpos, lpos, meanVel, peakVel)

    return(outData)

  }

  events <- split(data, data$event_n) # split into the different events

  trial_data <- lapply(events, get_sac_info)

  trial_data <- do.call(rbind.data.frame,trial_data)

  colnames(trial_data) <- c("start_x", "start_y",
                            "end_x", "end_y",
                            "mean_velocity", "peak_velocity")

  return(trial_data)

}
