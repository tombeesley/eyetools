#' Velocity threshold identification of saccades
#'
#' Takes a...
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param sample_rate sample rate of the eye-tracker. If default of NULL, then it will be computed from the timestamp data and the number of samples
#'
#' @return
#' @export
#'
#' @examples VTI_saccade()
#'

VTI_saccade <- function(data, sample_rate = NULL, threshold = 150, ...){

  # sample rate estimation if NULL
  if (is.null(sample_rate)) {
    ts <- aggregate(time~trial, data = data, range)
    total_time <- sum(ts$time[,2]-ts$time[,1])
    sample_rate <- 1000/(total_time/nrow(data)) # total time taken / samples
  }

  message(sample_rate)


  VTI_saccade_trial <- function(data, ...){

    x <- data$x
    y <- data$y

    d <- as.matrix(dist(cbind(x,y)))

    d_diag <- diag(d[2:nrow(d),])

    data <- cbind(data,
                  distance = c(NA,d_diag))

    data$distance <- dist_to_visual_angle(data$distance, dist_type = "pixel") # convert to VisAng

    data$vel <- data$distance*sample_rate # visual angle per second

    data$saccade_detected <- ifelse(data$vel > threshold, 2, 1) # saccade 2, otherwise 1

    data$saccade_detected[is.na(data$saccade_detected)] <- 0 # convert NA to 0

    data$event_n <- c(NA,cumsum(abs(diff(data$saccade_detected)))) # get event numbers
    message(dim(data))
    data <- data[data$saccade_detected == 2,] # get just the saccades
    message(dim(data))
    # define function to pull out relevant data from saccades
    get_sac_info <- function(dataIn){

      fpos <- dataIn[1,2:3] # x and y of FIRST time stamp
      lpos <- dataIn[nrow(dataIn),2:3] #dataIn[nrow(dataIn),2:3] # x and y of LAST time stamp
      n_samples <- nrow(dataIn)
      meanVel <- mean(dataIn$vel) # mean velocity
      peakVel <- max(dataIn$vel) # peak velocity during saccade
      outData <- c(fpos, lpos, n_samples, meanVel, peakVel)
      return(outData)

    }

    events <- split(data, data$event_n) # split into the different events

    trial_data <- lapply(events, get_sac_info)

    trial_data <- do.call(rbind.data.frame,trial_data)

    #message(trial_data)
    colnames(trial_data) <- c("start_x", "start_y",
                              "end_x", "end_y", "n_samples",
                              "mean_velocity", "peak_velocity")

    trial_data$trial <- data$trial[1]
    trial_data$sac_n <- 1:nrow(trial_data)

    return(trial_data)

  }

  data_s <- split(data, data$trial)
  by_trial_sac <- lapply(data_s, VTI_saccade_trial, ...)
  flat_trials <- do.call(rbind.data.frame,by_trial_sac)
  flat_trials <- flat_trials[,c(8,9,1:7)] # reorder cols
  return(flat_trials)

}
