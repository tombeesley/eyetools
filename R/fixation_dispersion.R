#' Fixation detection using a dispersion method
#'
#' Detects fixations by assessing dispersion of the eye position, using a method that is similar to that proposed by Salvucci & Goldberg (1996).
#' Evaluates the maximum dispersion (distance) between x/y coordinates across a window of data. Looks for sufficient periods
#' in which this maximum dispersion is below the specified dispersion tolerance. NAs are considered breaks
#' in the data and are not permitted within a valid fixation period.
#'
#' It can take either single participant data or multiple participants where there is a variable for unique participant identification.
#' The function looks for an identifier named `participant_ID` by default and will treat this as multiple-participant data as default,
#' if not it is handled as single participant data, or the participant_ID needs to be specified
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant (the standardised raw data form for eyetools)
#' @param min_dur Minimum duration (in milliseconds) of period over which fixations are assessed
#' @param disp_tol Maximum tolerance (in pixels) for the dispersion of values allowed over fixation period
#' @param NA_tol the proportion of NAs tolerated within any window of samples that is evaluated as a fixation
#' @param progress Display a progress bar
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
#' @return a dataframe containing each detected fixation by trial, with mean x/y position in pixel, start and end times, and duration.
#' @export
#' @examples
#' \donttest{
#' data <- combine_eyes(HCL)
#' fixation_dispersion(data, participant_ID = "pNum")
#' }
#'
#' @importFrom utils tail
#' @importFrom pbapply pblapply
#'
#' @references Salvucci, D. D., & Goldberg, J. H. (2000). Identifying fixations and saccades in eye-tracking protocols. Proceedings of the Symposium on Eye Tracking Research & Applications - ETRA '00, 71–78.

fixation_dispersion <- function(data, min_dur = 150, disp_tol = 100, NA_tol = .25, progress = TRUE, participant_ID = "participant_ID") {

  #first check for multiple/single ppt data
  test <- .check_ppt_n_in(participant_ID, data)
  participant_ID <- test[[1]]
  data <- test[[2]]


  internal_fixation_dispersion <- function(data, min_dur, disp_tol, NA_tol, progress) {

    ppt_label <- data[[participant_ID]][1]

    data <- split(data,data$trial) # create list from data by trial
    # present a progress bar, unless set to false
    if (progress) {
      data_fix <- pbapply::pblapply(data, trial_level_process, min_dur, disp_tol, NA_tol)
    } else {
      data_fix <- lapply(data, trial_level_process, min_dur, disp_tol, NA_tol)

    }

    # take the list data and bind into a single dataframe.
    data_fix <- do.call("rbind", data_fix)
    data_fix <- as.data.frame(data_fix)

    # tidy columns
    colnames(data_fix) <- c("trial", "fix_n", "start", "end", "duration", "x", "y",
                            "prop_NA", "min_dur", "disp_tol")

    ###if x (V6) and y (V7) are  all NA, return NA as a fixation
    if (sum(!is.na(as.numeric(data_fix$x))) == 0 || sum(!is.na(as.numeric(data_fix$y))) == 0) {
      trial_fix_store <- matrix(NA,1,7)
    }


    data_fix <- cbind(ppt_label, data_fix)
    colnames(data_fix)[1] <- participant_ID


    #row.names(data_fix) <- NULL # remove the row names

    return(data_fix)

  }

  # this is the trial level process that runs on the data for a single trial within the main algorithm
  trial_level_process <- function(data, min_dur, disp_tol, NA_tol) {

    trial <- data$trial[1]

    #if no observations for x or y at all
    if (sum(!is.na(data$x)) == 0 || sum(!is.na(data$y)) == 0) {
      trial_fix_store <- matrix(NA,1,7)
      trial_fix_store <- cbind(trial_fix_store, min_dur) # add param setting
      trial_fix_store <- cbind(trial_fix_store, disp_tol)  # add param setting
      trial_fix_store <- cbind(trial, trial_fix_store) # add trial number

      #trial_fix_store <- cbind(ppt_label, trial_fix_store)

      return(trial_fix_store) # returns the fixations for that trial to the main algorithm

    } else {

      data$time <- data$time - data$time[1] # start trial timestamp values at 0

      #get first row number where x and y is NOT NA
      min_x <- min(which(!is.na(data$x)))
      min_y <- min(which(!is.na(data$y)))
      #then get max row number
      max_x <- max(which(!is.na(data$x)))
      max_y <- max(which(!is.na(data$y)))

      #remove the leading and trailing NAs
      data <- data[min(min_x, min_y):max(max_x, max_y),]

      data$fix_num  <- NA # add a column that stores the fix number

      first_ts <- 1 # first timestamp of window
      last_ts <- 1 # allows step into the loop

      fix_cnt  <- 1
      new_window <- TRUE

      while (last_ts <= nrow(data)) { # while window is within limits of data

        if (new_window == TRUE){

          future_ts <- which(data$time >= data$time[first_ts] + min_dur)
          last_ts <- future_ts[1] #gets the earliest timestamp from all future valid ts
          if (is.na(last_ts)){
            break # last time stamp not valid (beyond window)
          }

          win <- data[first_ts:last_ts,] # the window of trials to evaluate

          if (mean(is.na(win$x)) < NA_tol) { # if within the tolerance of NA_tol

            max_d_win <- max(dist(win[,c("x", "y")]),na.rm = TRUE) # get max dispersion across this new window
            if (is.infinite(max_d_win)) {
              stop("is infinite")
            }
          } else {
            # window has too many NA, so shift along
            max_d_win <- disp_tol + 1 # artificially make this not a fixation
          }


          if(max_d_win <= disp_tol){
            # start of a fixation
            data$fix_num[first_ts:last_ts] <- fix_cnt
            # print(fix_cnt)

            new_window = FALSE # not a new window; look to extend fixation

          } else {
            # looking for the start of a new fixation
            # shift window along 1 timestamp
            first_ts  <- first_ts + 1
            last_ts  <- last_ts + 1
          }
        } else { # extend the window

          # increase the size of the window by a single timestamp
          last_ts  <- last_ts + 1
          # compute the new distances from this new data point
          xy_data <- data[last_ts,c("x", "y")]
          xy_win <- win[,c("x", "y")]

          max_d_new_data <- max(sqrt((xy_win$x - xy_data$x)^2 + (xy_win$y - xy_data$y)^2))

          if (is.na(max_d_new_data) || max_d_new_data >= disp_tol) {
            # either NA detected, or
            # the addition of data point broke the dispersion threshold
            # so make this last data point the first one for a new window
            new_window <- TRUE
            first_ts <- last_ts
            fix_cnt <- fix_cnt + 1 # next fixation

          } else { # otherwise this can be included in last fixation
            data$fix_num[last_ts] <- fix_cnt # add current fixation number to this timestamp
            win <- data[first_ts:last_ts,] # update the window to include this data point
          }
        }
      }

      # function to extract summary information from fixations
      summarise_fixations <- function(data){
        start <- as.numeric(data$time[1]) # first timestamp
        end <- as.numeric(data$time[nrow(data)]) # last timestamp
        dur <- end-start
        mean_x <- as.numeric(round(mean(data$x, na.rm = TRUE)),digits = 0)
        mean_y <- as.numeric(round(mean(data$y, na.rm = TRUE)),digits = 0)
        prop_NA <- as.numeric(round(mean((is.na(data$x) | is.na(data$y))),digits = 3))

        return(c(start, end, dur, mean_x, mean_y, prop_NA))

      }
      # print(data[1,4])

      # get trial summary of fixations
      if ((sum(is.na(data$fix_num)) == nrow(data)) == FALSE){
        data_s <- split(data,data$fix_num) # create list based on the fixation number from data
        trial_fix_store <- t(sapply(data_s, summarise_fixations))
        trial_fix_store <- cbind(1:nrow(trial_fix_store), trial_fix_store) #fixation number
      }
      else {
        # no fixations detected - write NAs
        trial_fix_store <- matrix(NA,1,7)
      }
      trial_fix_store <- cbind(trial_fix_store, min_dur) # add param setting
      trial_fix_store <- cbind(trial_fix_store, disp_tol)  # add param setting
      trial_fix_store <- cbind(trial, trial_fix_store) # add trial number

      #trial_fix_store <- cbind(ppt_label, trial_fix_store)

      return(trial_fix_store) # returns the fixations for that trial to the main algorithm


    }
  }

  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_fixation_dispersion, min_dur, disp_tol, NA_tol, progress)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  out <- .check_ppt_n_out(out)

  return(out)

}




