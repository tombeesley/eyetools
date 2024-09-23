#' Fixation detection using inverse saccade identification
#'
#' Determine fixations through the removal of saccades using the velocity threshold algorithm from Salvucci & Goldberg (1996).
#' Applies the algorithm used in VTI_saccade and removes the identified saccades before assessing whether separated fixations are outside of the dispersion tolerance.
#' If they are outside this tolerance, the fixation is treated as a new fixation regardless of the length of saccade separating them.
#' Compared to fix_dispersion(), fixation_VTI() is more conservative in determining a fixation as smaller saccades are discounted and following data treated as a continued fixation (assuming within pixel tolerance using disp_tol).
#' Returns a summary of the saccades found per trial, including start and end coordinates, timing, duration, mean velocity, and peak velocity.
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param sample_rate sample rate of the eye-tracker. If default of NULL, then it will be computed from the timestamp data and the number of samples
#' @param threshold velocity threshold (degrees of VA / sec) to be used for identifying saccades.
#' @param min_dur Minimum duration (in milliseconds) of period over which fixations are assessed
#' @param min_dur_sac Minimum duration (in milliseconds) for saccades to be determined
#' @param disp_tol Maximum tolerance (in pixels) for the dispersion of values allowed over fixation period
#' @param run_interp include a call to eyetools::interpolate on each trial.
#' @param smooth include a call to eyetools::smoother on each trial
#' @param progress Display a progress bar
#'
#'
#' @importFrom stats dist aggregate
#' @importFrom pbapply pblapply
#' @return a data frame giving the fixations found using an inverse-saccade algorithm found by trial.
#' @export
#'
#' @examples
#' # single trials:
#' fixation_VTI(example_raw_WM[example_raw_WM$trial == 9,])
#'
#' # multiple trials:
#' data <- rbind(example_raw_WM[example_raw_WM$trial %in% c(3,10),])
#'
#' fixation_VTI(data)

fixation_VTI <- function(data, sample_rate = NULL, threshold = 100, min_dur = 150, min_dur_sac = 20, disp_tol = 100, run_interp = TRUE, smooth = FALSE, progress = TRUE){

  if (run_interp == FALSE & sum(is.na(data)) > 0) { # if interpolation not run AND NA present in dataset
    stop("No interpolation carried out and NAs detected in your data. Either run interpolation via run_interp = TRUE, or check your data. Cannot compute inverse saccades with NAs present.", call. = FALSE)
  }

  # sample rate estimation if NULL
  if (is.null(sample_rate)) {
    ts <- aggregate(time~trial, data = data, range)
    total_time <- sum(ts$time[,2]-ts$time[,1])
    sample_rate <- 1000/(total_time/nrow(data)) # total time taken / samples
  }

  data <- split(data, data$trial)

  if(progress) {
    data_fix <- pbapply::pblapply(data, fixation_by_trial, sample_rate, threshold, min_dur, min_dur_sac, run_interp, disp_tol, smooth)
  } else {
    data_fix <- lapply(data, fixation_by_trial, sample_rate, threshold, min_dur, min_dur_sac, run_interp, disp_tol, smooth)

  }
  data_fix <- do.call(rbind.data.frame,data_fix)
  data_fix <- data_fix[,c(7,6,1,2,5,3,4,8, 9)] # reorder cols
  row.names(data_fix) <- NULL # remove the row names
  return(as.data.frame(data_fix))
}

fixation_by_trial <- function(data, sample_rate, threshold, min_dur, min_dur_sac, run_interp, disp_tol, smooth){

  if (run_interp){
    data <- eyetools::interpolate(data)
  }

  if (smooth){
    data <- eyetools::smoother(data)
  }

  trialNumber <- data$trial[1]
  x <- data$x
  y <- data$y

  data[,1] <- data[,1] - data[1,1,drop=TRUE] # start trial timestamps at 0
  d <- as.matrix(dist(cbind(x,y)))
  d_diag <- diag(d[2:nrow(d),])
  data <- cbind(data, distance = c(NA,d_diag))

  data$distance <- dist_to_visual_angle(data$distance, dist_type = "pixel") # convert to VisAng
  data$vel <- data$distance*sample_rate # visual angle per second
  data$saccade_detected <- ifelse(data$vel > threshold, 2, 1) # saccade 2, otherwise 1
  data$saccade_detected[is.na(data$saccade_detected)] <- 0 # convert NA to 0

  #first row will always be a non-event due to no preceding data, so treat as the start of a fixation
  data$saccade_detected[1] <- 1

  data$event_n <- c(1,cumsum(abs(diff(data$saccade_detected)))+1) # get event numbers
  events <- split(data, data$event_n) # split into the different events

  get_duration <- function(dataIn) {
    first <- dataIn[1,1]
    last <- dataIn[nrow(dataIn),1]
    duration <- diff(c(first, last))
    return(duration)
  }

  timing_store <- lapply(events, get_duration)
  timing_store <- do.call(rbind.data.frame,timing_store)
  colnames(timing_store) <- "event_duration"
  timing_store$event_n <- c(1:nrow(timing_store)) # add in event_n for merge

  data <- merge(data, timing_store) # add into main df

  data$short_sac <- ifelse(data$saccade_detected == 2 & data$event_duration < min_dur_sac, TRUE, FALSE) # if is saccade and shorter than duration, flag as TRUE, else FALSE



  single_row_doubled <- function(dataIn) {

    if (nrow(dataIn) == 0) {
      dataIn <- NULL
    }

    if (nrow(dataIn) == 1) {
      dataIn[2,] <- dataIn[1,] # duplicate row
    }

    return(dataIn)
  }

  events <- split(data, data$event_n) # split into the different events
  data <- lapply(events, single_row_doubled)
  data <- do.call(rbind.data.frame, data)


  first_in_group <- aggregate(data, list(data$event_n), head, 1) #get value of first row of each event
  first_in_group$saccade_detected_first <- first_in_group$saccade_detected

  last_in_group <- aggregate(data, list(data$event_n), tail, 1) #get value of last row
  last_in_group$saccade_detected_last <- last_in_group$saccade_detected

  data <- merge(data, first_in_group, all.x = TRUE)
  data$Group.1 <- NULL
  data <- merge(data, last_in_group, all.x = TRUE)
  data$Group.1 <- NULL

  # if event is a saccade, the set first and last observation as a fixation as the eye is typically within the tolerance and is set to move/slow
  data$saccade_detected_first <- ifelse(data$saccade_detected_first == 2, 1, data$saccade_detected_first)
  data$saccade_detected_last <- ifelse(data$saccade_detected_last == 2, 1, data$saccade_detected_last)

  data$saccade_detected <- ifelse(!is.na(data$saccade_detected_first), data$saccade_detected_first, data$saccade_detected)
  data$saccade_detected <- ifelse(!is.na(data$saccade_detected_last), data$saccade_detected_last, data$saccade_detected)

  first_in_group_short <- first_in_group[first_in_group$saccade_detected ==1,]

  first_in_group_short$x_diff <- c(NA, abs(diff(first_in_group[first_in_group$saccade_detected ==1,]$x))) # get difference in x between fixation events

  first_in_group_short$y_diff <- c(NA, abs(diff(first_in_group[first_in_group$saccade_detected ==1,]$y))) # get difference in y between fixation events

  data <- merge(data, first_in_group_short, all.x = TRUE)
  data$Group.1 <- NULL

  # following steps check whether the dispersion tolerance is broken between fixations and if so,
  # artificially adds in a saccade (to ensure that the fixations are recognised as independent of each other)
  add_blank_row <- function(dataIn, disp_tol) {
    if (!is.na(head(dataIn,1)$x_diff) | !is.na(head(dataIn,1)$y_diff)) {

      if (head(dataIn,1)$saccade_detected == 1 & (head(dataIn,1)$x_diff > disp_tol | head(dataIn,1)$y_diff > disp_tol)) {
        out <- rbind(
          c(head(dataIn,1)$event_n, head(dataIn,1)$time, NA, NA,
            head(dataIn,1)$trial, NA, NA, 2, head(dataIn,1)$event_duration,
            FALSE, NA, NA, NA, NA),
          dataIn)
      } else { out <- dataIn}

    } else { out <- dataIn }


    return(out)
  }

  events <- split(data, data$event_n) # split into the different events
  data <- lapply(events, add_blank_row, disp_tol)
  data <- do.call(rbind.data.frame, data)


  data$event_n <- c(1,cumsum(abs(diff(data$saccade_detected)))+1) # get event numbers

  data <- data[data$short_sac == FALSE,] # remove short saccades
  data <- data[data$saccade_detected == 1,] # get the inverse-saccades


  get_duration2 <- function(dataIn) {
    first <- dataIn[1,2] #row $first
    last <- dataIn[nrow(dataIn),2] # $last
    duration <- diff(c(first, last))
    event_n <- head(dataIn$event_n, 1)

    return(c(first, last,duration, event_n))
  }

  # recalculate durations
  events <- split(data, data$event_n) # split into the different events
  timing_store <- lapply(events, get_duration2)
  timing_store <- do.call(rbind.data.frame,timing_store)
  colnames(timing_store) <- c("first", "last", "event_duration2", "event_n")
  timing_store <- unique(timing_store)
  #timing_store$event_n <- c(1:nrow(timing_store)) # add in event_n for merge
  data <- merge(data, timing_store) # add into main df
  data <- data[order(data$time),] # merge throws the ordering off

  # define function to pull out relevant data from fixations
  summarise_fixations <- function(dataIn){

    first_ts <- dataIn[1,2]
    last_ts <- dataIn[nrow(dataIn),2]
    x <- mean(dataIn$x)
    y <- mean(dataIn$y)
    duration <- dataIn[nrow(dataIn),2] - dataIn[1,2]
    return(c(first_ts, last_ts, x, y, duration))

  }

  # get trial summary of fixations
  if (nrow(data) > 0){

    events <- split(data, data$event_n) # split into the different events
    trial_fix_store <- lapply(events, summarise_fixations)
    trial_fix_store <- do.call(rbind.data.frame,trial_fix_store)

    if (nrow(trial_fix_store[trial_fix_store[,5] >= min_dur,]) == 0) { #test for fixations of minimum length
      trial_fix_store <- matrix(NA,1,5)

    } else {
      trial_fix_store <- trial_fix_store[trial_fix_store[,5] >= min_dur,]

    }

    trial_fix_store$fix_n <- 1:nrow(trial_fix_store)


  } else {
    trial_fix_store <- matrix(NA,1,5)
  }
  # add col headers, trial number and return
  trial_fix_store <- cbind(trial_fix_store, trialNumber) # add trial number
  colnames(trial_fix_store) <- c("start", "end", "x", "y",
                                 "duration", "fix_n", "trial")
  trial_fix_store$min_dur <- min_dur
  trial_fix_store$disp_tol <- disp_tol
  return(trial_fix_store)

}

# include for backwards compatibility with existing code
fix_inverse_saccade <- fixation_VTI

