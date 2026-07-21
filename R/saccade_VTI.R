#' Velocity threshold identification of saccades
#'
#' Use the velocity threshold algorithm from Salvucci & Goldberg (2000) to determine saccadic eye movements.
#' Returns a summary of the saccades found per trial, including start and end coordinates, timing, duration, distance, angle, mean velocity, and peak velocity. 
#' If a set of AOIs are passed, then origin and terminal points of the saccade are also checked against AOIs and these names are expressed in the output. 
#'
#' Analyses data separately for each unique combination of values in `pID` and `trial`.
#'
#' @param data A dataframe with raw data (pID, time, x, y, trial), the standardised raw data form for eyetools
#' @param threshold velocity threshold (degrees of VA / sec) to be used for identifying saccades
#' @param min_dur minimum duration (ms) expected for saccades. This helps to avoid identification of very short saccades occurring at the boundary of velocity threshold
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (name, x, y, width_radius, height).
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

saccade_VTI <- function(data, threshold = 150, min_dur = 20, AOIs = NULL){

  data <- .make_NA_consistent(data)
  
    internal_saccade_VTI <- function(data, threshold, min_dur, AOIs) {


    # estimate sample rate
    if (is.null(the$eyetracker_properties$sample_frequency)) .estimate_sample_rate(data)
    
    data <- split(data, data$trial)
    data_sac <- pbapply::pblapply(data, saccade_VTI_trial, threshold, min_dur, AOIs)
    data_sac <- do.call(rbind.data.frame,data_sac)

    data_sac <- data_sac[,c("pID", "trial", "sac_n", "start", "end", "duration",
                            "origin_x", "origin_y", "origin_AOI", "terminal_x", "terminal_y", "terminal_AOI", "distance", "angle", "mean_velocity", "peak_velocity")]

    row.names(data_sac) <- NULL # remove the row names
    return(as.data.frame(data_sac))

  }

  saccade_VTI_trial <- function(data, threshold, min_dur, AOIs){

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

    data$vel <- data$distance*the$eyetracker_properties$sample_frequency # visual angle per second

    data$saccade_detected <- ifelse(data$vel > threshold, 2, 1) # saccade 2, otherwise 1

    data$saccade_detected[is.na(data$saccade_detected)] <- 0 # convert NA to 0

    data$event_n <- c(NA,cumsum(abs(diff(data$saccade_detected)))) # get event numbers

    data <- data[data$saccade_detected == 2,] # get just the saccades

    # define function to pull out relevant data from saccades
    summarise_saccades <- function(dataIn, AOIs){

      first_ts <- dataIn$time[1]
      last_ts <- dataIn$time[nrow(dataIn)]
      fpos <- dataIn[colnames(data) %in% c("x", "y")][1,] # x and y of FIRST time stamp
      lpos <- dataIn[colnames(data) %in% c("x", "y")][nrow(dataIn),] # x and y of LAST time stamp
      meanVel <- mean(dataIn$vel) # mean velocity
      peakVel <- max(dataIn$vel) # peak velocity during saccade
      duration <- dataIn$time[nrow(dataIn)] - dataIn$time[1]
      if (!is.null(AOIs)) {
        fpos_AOI <- check_sac_AOI(fpos, AOIs)
        lpos_AOI <- check_sac_AOI(lpos, AOIs)
      } else {
        fpos_AOI <- NA
        lpos_AOI <- NA
      }
      dist_angle <- sac_direction(fpos, lpos) # get distance and angle
      
      return(data.frame(first_ts, last_ts, fpos, fpos_AOI, lpos, lpos_AOI, dist_angle[1], dist_angle[2], meanVel, peakVel, duration))

    }

    # get trial summary of saccades
    if (nrow(data) > 0){

      events <- split(data, data$event_n) # split into the different events
      trial_sac_store <- lapply(events, summarise_saccades, AOIs)
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
    colnames(trial_sac_store) <- c("start", "end", "origin_x", "origin_y", "origin_AOI", "terminal_x", "terminal_y", "terminal_AOI",
                                   "distance", "angle", "mean_velocity", "peak_velocity", "duration", "sac_n")
    
    # add pID and trial number
    trial_sac_store["trial"] <- trialNumber
    trial_sac_store["pID"] <- ppt_label
    return(trial_sac_store)
  }

  data <- split(data, data$pID)
  out <- lapply(data, internal_saccade_VTI, threshold, min_dur, AOIs)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  return(out)

}

check_sac_AOI <- function(dataIn, AOIs) {
    
  
    aoi_entry <- NULL
    x <- dataIn[1]
    y <- dataIn[2]
    
    for (a in 1:nrow(AOIs)) {
    
      if (!is.na(AOIs[a,"height"])) {
        # square AOI
        aoi_entry[a] <- ((x >= AOIs[a,"x"]-AOIs[a,"width_radius"]/2 & x <= AOIs[a,"x"]+AOIs[a,"width_radius"]/2) &
                           (y >= AOIs[a,"y"]-AOIs[a,"height"]/2 & y <= AOIs[a,"y"]+AOIs[a,"height"]/2))
      } else if (sum(!is.na(AOIs[a,])) == 3) {
        # circle AOI
        aoi_entry[a] <- sqrt((AOIs[a,"x"]-x)^2+(AOIs[a,"y"]-y)^2) < AOIs[a,"width_radius"]
      } else {
        # report error message of bad AOI definition
        stop("Bad AOI definition. Consider using function create_AOI_df()")
        
      }
    
    }
    if (sum(aoi_entry) == 1) {
      return(AOIs[aoi_entry,1]) # return the name of AOI hit
    } else {
      return(NA)
    }
}
  
sac_direction <- function(A, B) {
  
  delta <- B - A  # c(dx, dy)
  
  # Distance
  distance <- sqrt(sum(delta^2))
  distance <- dist_to_visual_angle(distance, dist_type = "pixel")
  
  # Direction angle using atan2(y, x)
  rad <- atan2(as.double(delta[2]), as.double(delta[1]))
  deg <- rad * (180 / pi)
  
  # Normalize negative angles to 0-360 degrees
  if (deg < 0) deg <- deg + 360
  
  return(list(distance = distance, angle_degrees = deg))
}  
  

