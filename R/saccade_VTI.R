#' Velocity threshold identification of saccades
#'
#' Use the velocity threshold algorithm from Salvucci & Goldberg (2000) to determine saccadic eye movements.
#' Returns a summary of the saccades found per trial, including start and end coordinates, timing, duration, mean velocity, and peak velocity.
#'
#' Analyses data separately for each unique combination of values in `pID` and `trial`.
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param sample_rate sample rate of the eye-tracker. If default of NULL, then it will be computed from the timestamp data and the number of samples
#' @param threshold velocity threshold (degrees of VA / sec) to be used for identifying saccades
#' @param min_dur minimum duration (ms) expected for saccades. This helps to avoid identification of very short saccades occurring at the boundary of velocity threshold
#'
#' @importFrom stats dist aggregate
#' @importFrom pbapply pblapply
#' @return a data frame giving the saccades found by trial
#' @export
#'
#' @examples
#' data <- combine_eyes(HCL)
#' saccade_VTI(data)
#' 
#' @references Salvucci, D. D., & Goldberg, J. H. (2000). Identifying fixations and saccades in eye-tracking protocols. Proceedings of the Symposium on Eye Tracking Research & Applications - ETRA '00, 71–78.

saccade_VTI <- function(data, sample_rate = NULL, threshold = 150, min_dur = 20){

  internal_saccade_VTI <- function(data, sample_rate, threshold, min_dur) {


    # estimate sample rate
    if (is.null(sample_rate)==TRUE) sample_rate <- .estimate_sample_rate(data)


    data <- split(data, data$trial)
    data_sac <- pbapply::pblapply(data, saccade_VTI_trial, sample_rate, threshold, min_dur)
    data_sac <- do.call(rbind.data.frame,data_sac)

    data_sac <- data_sac[,c("pID", "trial", "sac_n", "start", "end", "duration",
                            "origin_x", "origin_y", "terminal_x", "terminal_y", "mean_velocity", "peak_velocity")]

    row.names(data_sac) <- NULL # remove the row names
    return(as.data.frame(data_sac))

  }

  saccade_VTI_trial <- function(data, sample_rate, threshold, min_dur){

    ppt_label <- data$pID[1]

    trialNumber <- data$trial[1]
    x <- data$x
    y <- data$y
    data$time <- data$time - data$time[1] # start trial timestamps at 0

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

      first_ts <- dataIn$time[1]
      last_ts <- dataIn$time[nrow(dataIn)]
      fpos <- dataIn[colnames(data) %in% c("x", "y")][1,] # x and y of FIRST time stamp
      lpos <- dataIn[colnames(data) %in% c("x", "y")][nrow(dataIn),] #dataIn[nrow(dataIn),2:3] # x and y of LAST time stamp
      meanVel <- mean(dataIn$vel) # mean velocity
      peakVel <- max(dataIn$vel) # peak velocity during saccade
      duration <- dataIn$time[nrow(dataIn)] - dataIn$time[1]
      return(data.frame(first_ts, last_ts, fpos, lpos, meanVel, peakVel, duration))

    }

    # get trial summary of saccades
    if (nrow(data) > 0){

      events <- split(data, data$event_n) # split into the different events
      trial_sac_store <- lapply(events, summarise_saccades)
      trial_sac_store <- do.call(rbind.data.frame,trial_sac_store)

      if (nrow(trial_sac_store[trial_sac_store$duration >= min_dur,]) == 0) { #test for saccades of minimum length
        trial_sac_store <- matrix(NA,1,10)

      } else {
        trial_sac_store <- trial_sac_store[trial_sac_store$duration >= min_dur,]
        trial_sac_store$sac_n <- 1:nrow(trial_sac_store)

      }


    } else {
      trial_sac_store <- matrix(NA,1,10)

    }
    # add col headers, trial number and return
    colnames(trial_sac_store) <- c("start", "end", "origin_x", "origin_y", "terminal_x", "terminal_y",
                                   "mean_velocity", "peak_velocity", "duration", "sac_n")
    
    # add pID and trial number
    trial_sac_store["trial"] <- trialNumber
    trial_sac_store["pID"] <- ppt_label
    return(trial_sac_store)
  }

  data <- split(data, data$pID)
  out <- lapply(data, internal_saccade_VTI, sample_rate, threshold, min_dur)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  return(out)

}
