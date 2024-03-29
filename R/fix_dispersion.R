#' Fixation detection by dispersion method
#'
#' Detects fixations according to a method similar to that proposed by Salvucci & Goldberg (1996).
#' Evaluates the absolute range of values of both x and y coordinates. Looks for sufficient periods
#' in which the ranges are both below the specified dispersion tolerance. NAs are considered breaks
#' in the data and are not permitted within a valid fixation period.
#'
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant (the standardised raw data form for eyeproc)
#' @param min_dur Minimum duration (in milliseconds) of period over which fixations are assessed
#' @param disp_tol Maximum tolerance (in pixels) for the dispersion of values allowed over fixation period
#' @param run_interp include a call to eyetools::interpolation on each trial
#' @param NA_tol the proportion of NAs tolerated within any window of samples that is evaluated as a fixation
#' @return a dataframe containing each detected fixation by trial, with mean x/y position in pixel, start and end times, and duration.
#' @export
#' @examples fix_dispersion(example_raw_fix, disp_tol = 150)
#'
#' @importFrom zoo na.trim
#' @importFrom utils tail
#' @importFrom pbapply pblapply

fix_dispersion <- function(data, min_dur = 150, disp_tol = 100, run_interp = TRUE, NA_tol = .25) {

  data <- split(data,data$trial) # create list from data by trial
  # try with progress bar
  data_fix <- pbapply::pblapply(data, trial_level_process, min_dur, disp_tol, run_interp, NA_tol)

  data_fix <- do.call("rbind", data_fix)
  colnames(data_fix) <- c("start", "end", "duration", "x", "y",
                          "prop_NA", "fix_n", "min_dur", "disp_tol", "trial")
  data_fix <- data_fix[,c(10,7,1:6,8,9)] # reorder cols
  row.names(data_fix) <- NULL # remove the row names
  return(as.data.frame(data_fix))

}

trial_level_process <- function(data, min_dur, disp_tol, run_interp, NA_tol) {

  trialNumber <- data$trial[1]

  if (run_interp){data <- eyetools::interpolate(data)}
  data[,1] <- data[,1] - data[1,1,drop=TRUE] # start trial timestamps at 0

  data <- na.trim(data) # remove leading and trailing NAs
  data$fix_num  <- NA # add a column that stores the fix number

  first_ts <- 1 # first timestamp of window
  last_ts <- 1 # allows step into the loop

  fix_cnt  <- 1
  new_window <- TRUE

  while (last_ts <= nrow(data)) { # while window is within limits of data

    if (new_window == TRUE){

      # print("start new window")

      future_ts <- which(data[,1] >= data[first_ts,1,drop=TRUE] + min_dur)
      last_ts <- future_ts[1] #gets the earliest timestamp from all future valid ts
      if (is.na(last_ts)){
        break # last time stamp not valid (beyond window)
      }

      # print(first_ts)
      # print(last_ts)
      # Sys.sleep(0.05)

      win <- data[first_ts:last_ts,]

      if (mean(is.na(win$x)) < NA_tol) { # if within the tolerance of NA_tol
        max_d_win <- max(dist(win[,2:3]),na.rm = TRUE) # get max dispersion across this new window
        if (is.infinite(max(dist(win[,2:3]),na.rm = TRUE))) {
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
        # shift window along 1 timestamp
        first_ts  <- first_ts + 1
        last_ts  <- last_ts + 1
      }
    } else { # extend the window

      last_ts  <- last_ts + 1
      # compute the new distances from this new data point
      max_d_new_data <- max(rdist::cdist(data[last_ts,2:3],win[,2:3]))

      if (is.na(max_d_new_data) | max_d_new_data >= disp_tol) {
        # either NaN detected, or
        # the addition of data point broke the dispersion threshold
        # so make this last data point the first one and draw new window
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
    data_s <- split(data,data$fix_num) # create list from data by trial
    trial_fix_store <- t(sapply(data_s, summarise_fixations))
    trial_fix_store <- cbind(trial_fix_store, 1:nrow(trial_fix_store)) #fixation number
  }
  else {
    trial_fix_store <- matrix(NA,1,7)
    #colnames(trial_fix_store) <- c("start", "end", "dur", "mean_x", "mean_y", "prop_NA", "fix_n")
  }
  trial_fix_store <- cbind(trial_fix_store, min_dur) # add param setting
  trial_fix_store <- cbind(trial_fix_store, disp_tol)  # add param setting
  trial_fix_store <- cbind(trial_fix_store, trialNumber) # add trial number


  return(trial_fix_store)

}






