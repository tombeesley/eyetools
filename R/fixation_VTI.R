#' Fixation detection using a velocity threshold identification method
#'
#' Determine fixations by assessing the velocity of eye-movements, using a method that is similar to that proposed by Salvucci & Goldberg (1996).
#' Applies the algorithm used in VTI_saccade and removes the identified saccades before assessing whether separated fixations are outside of the dispersion tolerance.
#' If they are outside of this tolerance, the fixation is treated as a new fixation regardless of the length of saccade separating them.
#' Compared to fixation_dispersion(), fixation_VTI() is more conservative in determining a fixation as smaller saccades are discounted and the resulting data is
#' treated as a continued fixation (assuming it is within the pixel tolerance set by disp_tol).
#' Returns a summary of the fixations found per trial, including start and end coordinates, timing, duration, mean velocity, and peak velocity.
#'
#' It can take either single participant data or multiple participants where there is a variable for unique participant identification.
#' The function looks for an identifier named `participant_ID` by default and will treat this as multiple-participant data as default,
#' if not it is handled as single participant data, or the participant_ID needs to be specified
#'
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param sample_rate sample rate of the eye-tracker. If default of NULL, then it will be computed from the timestamp data and the number of samples
#' @param threshold velocity threshold (degrees of VA / sec) to be used for identifying saccades.
#' @param min_dur Minimum duration (in milliseconds) of period over which fixations are assessed
#' @param min_dur_sac Minimum duration (in milliseconds) for saccades to be determined
#' @param disp_tol Maximum tolerance (in pixels) for the dispersion of values allowed over fixation period
#' @param smooth include a call to eyetools::smoother on each trial
#' @param progress Display a progress bar
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
#'
#' @importFrom stats dist aggregate na.omit
#' @importFrom pbapply pblapply
#' @return a dataframe containing each detected fixation by trial, with mean x/y position in pixel, start and end times, and duration.
#' @export
#'
#' @examples
#' \donttest{
#' data <- combine_eyes(HCL)
#' data <- interpolate(data)
#' fixation_VTI(data, participant_ID = "pNum")
#' }
#'
#' @references Salvucci, D. D., & Goldberg, J. H. (2000). Identifying fixations and saccades in eye-tracking protocols. Proceedings of the Symposium on Eye Tracking Research & Applications - ETRA '00, 71–78.

fixation_VTI <- function(data, sample_rate = NULL, threshold = 100, min_dur = 150, min_dur_sac = 20, disp_tol = 100, smooth = FALSE, progress = TRUE, participant_ID = "participant_ID"){
  if (sum(is.na(data)) > 0) { # if NA present in dataset
    stop("NAs detected in your data. Cannot compute inverse saccades with NAs present.", call. = FALSE)
  }


  #first check for multiple/single ppt data
  test <- .check_ppt_n_in(participant_ID, data)
  participant_ID <- test[[1]]
  data <- test[[2]]

  internal_fixation_VTI <- function(data, sample_rate, threshold, min_dur, min_dur_sac, disp_tol, smooth, progress, participant_ID) {

    # estimate sample rate
    if (is.null(sample_rate)==TRUE) sample_rate <- .estimate_sample_rate(data)


    data <- split(data, data$trial)
    # either show a progress bar, or not
    if(progress) {
      data_fix <- pbapply::pblapply(data, fixation_by_trial, sample_rate, threshold, min_dur, min_dur_sac, disp_tol, smooth, participant_ID)
    } else {
      data_fix <- lapply(data, fixation_by_trial, sample_rate, threshold, min_dur, min_dur_sac, disp_tol, smooth, participant_ID)
    }

    data_fix <- do.call(rbind.data.frame,data_fix)
    data_fix <- data_fix[,c(participant_ID, "trialNumber", "fix_n", "start", "end", "duration", "x", "y", "min_dur", "disp_tol")]
    row.names(data_fix) <- NULL # remove the row names

    return(as.data.frame(data_fix))
  }

  fixation_by_trial <- function(data, sample_rate, threshold, min_dur, min_dur_sac, disp_tol, smooth, participant_ID){

    ppt_label <- data[[participant_ID]][1]


    if (smooth){
      data <- smoother(data, participant_ID = participant_ID)
    }

    if(is.null(data[[participant_ID]])) data$participant_ID <- ppt_label
    #### THIS NEXT CHUNK CALCULATES THE DISTANCES AND VELOCITY OF THE SACCADES ####
    trialNumber <- data$trial[1]
    x <- data$x
    y <- data$y

    data['time'] <- data['time'] - data['time'][1,] # start trial timestamps at 0
    d <- as.matrix(dist(cbind(x,y)))
    d_diag <- diag(d[2:nrow(d),])
    data <- cbind(data, distance = c(NA,d_diag))

    data$distance <- dist_to_visual_angle(data$distance, dist_type = "pixel") # convert to VisAng
    data$vel <- data$distance*sample_rate # visual angle per second
    data$saccade_detected <- ifelse(data$vel > threshold, 2, 1) # saccade 2, otherwise 1
    data$saccade_detected[is.na(data$saccade_detected)] <- 0 # convert NA to 0

    #first row will always be a non-event due to no preceding data, so set as a fixation
    data$saccade_detected[1] <- 1

    #### END DISTANCE/VELOCITY CALCULATIONS ####

    data$event_n <- c(1,cumsum(abs(diff(data$saccade_detected)))+1) # get event numbers
    events <- split(data, data$event_n) # split into the different events

    #CALCULATE DURATIONS OF EACH EVENT
    get_duration <- function(dataIn) {
      first <- dataIn$time[1]
      last <- dataIn$time[nrow(dataIn)]
      duration <- diff(c(first, last))
      return(duration)
    }

    timing_store <- lapply(events, get_duration)
    timing_store <- do.call(rbind.data.frame,timing_store)
    colnames(timing_store) <- "event_duration"
    timing_store$event_n <- c(1:nrow(timing_store)) # add in event_n for merge
    data <- merge(data, timing_store) # add into main df

    # if is saccade and shorter than duration, flag as TRUE, else FALSE
    # USEFUL TO FLAG SHORT SACCADES THAT DON'T MEET A THRESHOLD, SO FIXATIONS CAN BE TREATED AS CONTINUOUS
    data$short_sac <- ifelse(data$saccade_detected == 2 & data$event_duration < min_dur_sac, TRUE, FALSE)

    # if an event is only one row long, then proceeding steps of identifying start/end of events doesn't work
    # so add in a duplicate row this is useful for when the end points of a saccade (at a higher velocity)
    # ought to be treated as the start of a fixation instead

    single_row_doubled <- function(dataIn) {

      if (nrow(dataIn) == 0) { #IF EMPTY
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

    data <- data[data$short_sac == FALSE,] # remove short saccades

    #this is why doubled rows are needed, split the dataset and identify the first and last observation of each event
    first_in_group <- aggregate(data, list(data$event_n), head, 1) #get value of first row of each event
    first_in_group$saccade_detected_first <- first_in_group$saccade_detected

    last_in_group <- aggregate(data, list(data$event_n), tail, 1) #get value of last row
    last_in_group$saccade_detected_last <- last_in_group$saccade_detected

    #MERGE AND DROP UNNECESSARY VARIABLES
    data <- merge(data, first_in_group, all.x = TRUE)
    data$Group.1 <- NULL
    data <- merge(data, last_in_group, all.x = TRUE)
    data$Group.1 <- NULL

    # if event is a saccade, set first and last observation as a fixation as the eye is
    # typically within the tolerance and is set to move or slow down
    data$event_n <- ifelse(is.na(data$saccade_detected_first), data$event_n,
                           ifelse(data$saccade_detected_first == 2, data$event_n-1, data$event_n))

    data$saccade_detected_first <- ifelse(data$saccade_detected_first == 2, 1, data$saccade_detected_first)

    data$event_n <-  ifelse(is.na(data$saccade_detected_last), data$event_n,
                            ifelse(data$saccade_detected_last == 2, data$event_n+1, data$event_n))
    data$saccade_detected_last <- ifelse(data$saccade_detected_last == 2, 1, data$saccade_detected_last)

    data$saccade_detected <- ifelse(!is.na(data$saccade_detected_first), data$saccade_detected_first, data$saccade_detected)
    data$saccade_detected <- ifelse(!is.na(data$saccade_detected_last), data$saccade_detected_last, data$saccade_detected)

    # collect just fixation data
    first_in_group_short <- first_in_group[first_in_group$saccade_detected ==1,]

    #get differences in x, y coords
    first_in_group_short$x_diff <- c(0, diff(first_in_group[first_in_group$saccade_detected ==1,]$x)) # get difference in x between fixation events, non-abs() to calculate drift over events
    first_in_group_short$y_diff <- c(0, diff(first_in_group[first_in_group$saccade_detected ==1,]$y)) # get difference in y between fixation events

    #GET THE TOTAL CHANGE IN PIXEL DEVIATION
    first_in_group_short$distance_euclidean <- sqrt(first_in_group_short$x_diff^2 + first_in_group_short$y_diff^2)

    #### THIS FUNCTION CALCULATES THE DISTANCE DRIFTED ACROSS CONSECUTIVE FIXATIONS
    #### IN ORDER TO COMBINE THEM IF THEY ARE WITHIN TOLERANCE. AS OTHERWISE MICRO-SACCADES INTERFERE WITH THE
    #### CLASSIFICATION OF THE FIXATIONS
    calculate_drift <- function(i) {
      #this function tests whether proceeding values of x_diff deviate by over 100 pixels from the origin timepoint
      # x values - drift from i (origin event)

      if ( i != 0) {
        distance <- first_in_group_short$distance_euclidean
        distance[i] <- 0

        distance[is.na(distance)] <- 0

        value <- distance[i:length(distance)]

        #IF LAST VALUE IN LENGTH AND IS NA, SET AS 0 OTHERWISE IT GETS DROPPED FROM THE OUTPUT
        if ( length(value) == 1) { if(is.na(value)) { value <- 0 }}

        value_rle <- rle(value < disp_tol) # GET A LOGICAL OF WHETHER VALUES ARE WITHIN TOLERANCE

        if(value_rle$values[1] == TRUE) { upper_cap <- value_rle$lengths[1]} else { upper_cap = 0} # SET CAP AS THE LENGTH OF VALUES IN TOLERANCE

        ind <- i + 1:upper_cap #GET INDEX

        if(any(ind > nrow(first_in_group_short))) { ind <- i }

        if (upper_cap == 0) { upper_cap <- 1}

        data.frame(dist_cum = value[upper_cap],
                   event_n = first_in_group_short$event_n[i],
                   upper_cap)
      }
    }

    drift_store <- do.call("rbind", lapply(seq(nrow(first_in_group_short)), calculate_drift))

    #### THIS FOLLOWING CHUNK AND FUNCTION PROGRAMMATICALLY GOES THROUGH THE UPPER CAP VECTOR, AND GROUPS THE EVENTS TOGETHER
    #### ASKED AND ASNSWERED HERE:
    #### https://stackoverflow.com/questions/79015668/is-there-an-r-function-to-repeat-a-value-for-the-length-of-value-then-skip-ahea

    filler <- function(x, y) {
      # Decrement 'rem'
      x$rem <- x$rem - 1

      # Reset 'rem' and 'val', and increment 'event' if 'rem' is less than 1
      if (x$rem < 1) {
        x$rem = x$val = y
        x$event = x$event + 1
      }
      # store the current event in 'event_n'
      x$event_n <- x$event
      # Return the modified object
      x
    }

    # add the output information into a new variable
    drift_store$fix_n <- sapply(tail(Reduce(filler, drift_store$upper_cap, init = list(rem = 0, val = NA, event = 0), accumulate = TRUE),
                                     -1),
                                `[[`, "event_n")


    data <- merge(data, drift_store, all.x = TRUE)
    data$Group.1 <- NULL
    data <- data[order(data$time),] # merge throws the ordering off

    data <- data[data$saccade_detected == 1,] # get the inverse-saccades

    ### NOW THAT FIXATIONS HAVE BEEN GROUPED AND CORRECTED, THE DURATIONS NEED TO BE RECALCULATED
    get_duration2 <- function(dataIn) {
      first <- dataIn$time[1] #row $first
      last <- dataIn$time[nrow(dataIn)] # $last
      event_duration2 <- diff(c(first, last))
      fix_n <- head(dataIn$fix_n, 1)

      return(data.frame(first, last, event_duration2, fix_n))
    }

    # recalculate durations
    events <- split(data, data$fix_n) # split into the different events
    timing_store <- lapply(events, get_duration2)
    timing_store <- do.call(rbind.data.frame,timing_store)
    timing_store <- unique(timing_store)
    #timing_store$event_n <- c(1:nrow(timing_store)) # add in event_n for merge
    data <- merge(data, timing_store) # add into main df
    data <- data[order(data$time),] # merge throws the ordering off
    # define function to pull out relevant data from fixations

    summarise_fixations <- function(dataIn){

      participant_ID = data[[participant_ID]][1]
      start <- dataIn$time[1]
      end <- dataIn$time[nrow(dataIn)]
      x <- mean(dataIn$x)
      y <- mean(dataIn$y)
      duration <- end - start

      return(data.frame(participant_ID, start, end, x, y, duration))

    }
    # get trial summary of fixations
    if (nrow(data) > 0){

      events <- split(data, data$fix_n) # split into the different events
      trial_fix_store <- lapply(events, summarise_fixations)
      trial_fix_store <- do.call("rbind.data.frame", trial_fix_store)

      if (nrow(trial_fix_store[trial_fix_store$duration >= min_dur,]) == 0) { #test for fixations of minimum length
        trial_fix_store <- data.frame(participant_ID = c(NA), start = c(NA), end = c(NA),
                                      x = c(NA), y = c(NA), duration = c(NA))
      } else {
        trial_fix_store <- trial_fix_store[trial_fix_store$duration >= min_dur,]

      }
      trial_fix_store$fix_n <- 1:nrow(trial_fix_store)

    } else {
      trial_fix_store <- data.frame(participant_ID = c(NA), start = c(NA), end = c(NA),
                                    x = c(NA), y = c(NA), duration = c(NA))
    }
    # add col headers, trial number and return

    colnames(trial_fix_store)[1] <- c(participant_ID)

    trial_fix_store <- cbind(trial_fix_store, trialNumber) # add trial number
    trial_fix_store$min_dur <- min_dur
    trial_fix_store$disp_tol <- disp_tol

    return(trial_fix_store)
  }

  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_fixation_VTI, sample_rate, threshold, min_dur, min_dur_sac, disp_tol, smooth, progress, participant_ID)

  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  out <- .check_ppt_n_out(out)

  return(out)
}
