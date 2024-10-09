#' Fixation detection using a dispersion method
#'
#' Detects fixations by assessing dispersion of the eye position, using a method that is similar to that proposed by Salvucci & Goldberg (1996).
#' Evaluates the maximum dispersion (distance) between x/y coordinates across a window of data. Looks for sufficient periods
#' in which this maximum dispersion is below the specified dispersion tolerance. NAs are considered breaks
#' in the data and are not permitted within a valid fixation period.
#' Runs the interpolation algorithm by default to fix small breaks in the data.
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant (the standardised raw data form for eyetools)
#' @param min_dur Minimum duration (in milliseconds) of period over which fixations are assessed
#' @param disp_tol Maximum tolerance (in pixels) for the dispersion of values allowed over fixation period
#' @param run_interp include a call to eyetools::interpolate on each trial
#' @param NA_tol the proportion of NAs tolerated within any window of samples that is evaluated as a fixation
#' @param progress Display a progress bar
#' @return a dataframe containing each detected fixation by trial, with mean x/y position in pixel, start and end times, and duration.
#' @export
#' @examples
#' \dontrun{
#' fixation_dispersion(example_raw_fix, disp_tol = 150)
#' }
#'
#' @importFrom utils tail
#' @importFrom pbapply pblapply
#'
#' @references Salvucci, D. D., & Goldberg, J. H. (2000). Identifying fixations and saccades in eye-tracking protocols. Proceedings of the Symposium on Eye Tracking Research & Applications - ETRA ’00, 71–78.

fixation_dispersion <- function(data, min_dur = 150, disp_tol = 100, run_interp = TRUE, NA_tol = .25, progress = TRUE) {

  data <- split(data,data$trial) # create list from data by trial
  # present a progress bar, unless set to false
  if (progress) {
    data_fix <- pbapply::pblapply(data, trial_level_process, min_dur, disp_tol, run_interp, NA_tol)
  } else {
    data_fix <- lapply(data, trial_level_process, min_dur, disp_tol, run_interp, NA_tol)

  }

  # take the list data and bind into a single dataframe.
  data_fix <- do.call("rbind", data_fix)

  #if x and y are still all NA, stop
  if (sum(!is.na(data_fix[[4]])) == 0|sum(!is.na(data_fix[[5]])) == 0) {
    stop("Too many NAs present in x and y.")
  }

  # tidy columns
  colnames(data_fix) <- c("trial", "fix_n", "start", "end", "duration", "x", "y",
                          "prop_NA", "min_dur", "disp_tol")
  row.names(data_fix) <- NULL # remove the row names
  return(as.data.frame(data_fix))

}

# this is the trial level process that runs on the data for a single trial within the main algorithm
trial_level_process <- function(data, min_dur, disp_tol, run_interp, NA_tol) {

  trialNumber <- data$trial[1]

  #if no observations for x or y at all
  if (sum(!is.na(data$x)) == 0|sum(!is.na(data$y)) == 0) {
    trial_fix_store <- NULL
  } else {

    if (run_interp){data <- eyetools::interpolate(data)}
    data[,1] <- data[,1] - data[1,1,drop=TRUE] # start trial timestamp values at 0

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

        future_ts <- which(data[,1] >= data[first_ts,1,drop=TRUE] + min_dur)
        last_ts <- future_ts[1] #gets the earliest timestamp from all future valid ts
        if (is.na(last_ts)){
          break # last time stamp not valid (beyond window)
        }

        win <- data[first_ts:last_ts,] # the window of trials to evaluate

        if (mean(is.na(win$x)) < NA_tol) { # if within the tolerance of NA_tol
          max_d_win <- max(dist(win[,2:3]),na.rm = TRUE) # get max dispersion across this new window
          if (is.infinite(max_d_win)) {
            print("is infinite")
          }
        } else {
          # window has too many NA, so shift along
          max_d_win <- disp_tol + 1 # artificially make this not a fixation
        }


        if(max_d_win <= disp_tol){
          # start of a fixation
          data[first_ts:last_ts,"fix_num"] <- fix_cnt
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
        max_d_new_data <- max(rdist::cdist(data[last_ts,2:3],win[,2:3]))

        if (is.na(max_d_new_data) | max_d_new_data >= disp_tol) {
          # either NA detected, or
          # the addition of data point broke the dispersion threshold
          # so make this last data point the first one for a new window
          new_window <- TRUE
          first_ts <- last_ts
          fix_cnt <- fix_cnt + 1 # next fixation

        } else { # otherwise this can be included in last fixation
          data[last_ts,"fix_num"] <- fix_cnt # add current fixation number to this timestamp
          win <- data[first_ts:last_ts,] # update the window to include this data point
        }
      }
    }

    # function to extract summary information from fixations
    summarise_fixations <- function(data){
      start <- as.numeric(data[1,1]) # first timestamp
      end <- as.numeric(data[nrow(data),1]) # last timestamp
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
    trial_fix_store <- cbind(trialNumber, trial_fix_store) # add trial number


    return(trial_fix_store) # returns the fixations for that trial to the main algorithm
  }
}

# ALIAS
# include old function name for backwards compatibility with existing code
fix_dispersion <- fixation_dispersion




