#' Velocity threshold identification of saccades
#'
#' Takes a...
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param sample_rate sample rate of the eye-tracker. If default of NULL, then it will be computed from the timestamp data and the number of samples
#' @param threshold velocity threshold (degrees of VA / sec) to be used for identifying saccades
#' @param minDur minimum duration (ms) expected for saccades. This helps to avoid identification of very short saccades occuring at the boundary of velocity threshold
#'
#' @return
#' @export
#'
#' @examples
#'

VTI_saccade <- function(data, sample_rate = NULL, threshold = 150, minDur = 20, ...){

  # sample rate estimation if NULL
  if (is.null(sample_rate)) {
    ts <- aggregate(time~trial, data = data, range)
    total_time <- sum(ts$time[,2]-ts$time[,1])
    sample_rate <- 1000/(total_time/nrow(data)) # total time taken / samples
  }

  VTI_saccade_trial <- function(data, ...){

    trialNumber <- data$trial[1]
    x <- data$x
    y <- data$y

    data[,1] <- data[,1] - data[1,1,drop=TRUE] # start trial timestamps at 0

    d <- as.matrix(dist(cbind(x,y)))

    d_diag <- diag(d[2:nrow(d),])

    data <- cbind(data,
                  distance = c(NA,d_diag))

    data$distance <- dist_to_visual_angle(data$distance, dist_type = "pixel") # convert to VisAng

    data$vel <- data$distance*sample_rate # visual angle per second

    data$saccade_detected <- ifelse(data$vel > threshold, 2, 1) # saccade 2, otherwise 1

    data$saccade_detected[is.na(data$saccade_detected)] <- 0 # convert NA to 0

    data$event_n <- c(NA,cumsum(abs(diff(data$saccade_detected)))) # get event numbers

    data <- data[data$saccade_detected == 2,] # get just the saccades

    # define function to pull out relevant data from saccades
    summarise_saccades <- function(dataIn){

      first_ts <- dataIn[1,1]
      last_ts <- dataIn[nrow(dataIn),1]
      fpos <- dataIn[1,2:3] # x and y of FIRST time stamp
      lpos <- dataIn[nrow(dataIn),2:3] #dataIn[nrow(dataIn),2:3] # x and y of LAST time stamp
      meanVel <- mean(dataIn$vel) # mean velocity
      peakVel <- max(dataIn$vel) # peak velocity during saccade
      duration <- dataIn[nrow(dataIn),1] - dataIn[1,1]
      return(c(first_ts, last_ts, fpos, lpos, meanVel, peakVel, duration))

    }

    # get trial summary of saccades
    if (nrow(data) > 0){

      events <- split(data, data$event_n) # split into the different events
      trial_sac_store <- lapply(events, summarise_saccades)
      trial_sac_store <- do.call(rbind.data.frame,trial_sac_store)
      trial_sac_store <- trial_sac_store[trial_sac_store[,9] >= minDur,]
      trial_sac_store$sac_n <- 1:nrow(trial_sac_store)
    } else {
      trial_sac_store <- matrix(NA,1,10)

    }
    # add col headers, trial number and return
    colnames(trial_sac_store) <- c("startBLAH", "end", "origin_x", "origin_y", "terminal_x", "terminal_y",
                                   "mean_velocity", "peak_velocity", "duration", "sac_n")
    trial_sac_store <- cbind(trial_sac_store, trialNumber) # add trial number
    return(trial_sac_store)

  }

  data <- split(data, data$trial)
  data_sac <- lapply(data, VTI_saccade_trial, ...)
  data_sac <- do.call(rbind.data.frame,data_sac)
  data_sac <- data_sac[,c(11,10,1,2,9,3:8)] # reorder cols
  row.names(data_sac) <- NULL # remove the row names
  return(as.data.frame(data_sac))

}
