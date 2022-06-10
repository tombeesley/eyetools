#' Fixation detection by dispersion method
#'
#' Detects fixations according to a method similar to that proposed by Salvucci & Goldberg (1996).
#' Evaluates the absolute range of values of both x and y coordinates. Looks for sufficient periods
#' in which the ranges are both below the specified dispersion tolerance. NAs are considered breaks
#' in the data and are not permitted within a valid fixation period.
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param min_dur Minimum duration (in milliseconds) of period over which fixations are assessed
#' @param disp_tol Maximum tolerance (in pixels) for the dispersion of values allowed over fixation period
#' @param run_interp include a call to eyetools::interpolation on each trial
#' @param round rounding digits of fixation coordinates in output
#'
#' @return
#' @export
#'
#' @examples fix_dispersion(example_raw_psy, min_dur = 200, disp_tol = 50)
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @importFrom rlang .data
#' @importFrom zoo na.trim
#' @importFrom RcppRoll roll_min roll_max
#'

fix_dispersion_old <- function(data,
                           min_dur = 150,
                           disp_tol = 100,
                           run_interp = TRUE,
                           round = 0) {

  data <- split(data,data$trial) # create list from data by trial
  data_fix <- data %>%
    map(~trial_level_process(.,
                             min_dur = min_dur,
                             disp_tol = disp_tol,
                             run_interp = run_interp,
                             round = round))

  data_fix <- do.call("rbind", data_fix)
  colnames(data_fix) <- c("start", "end", "x", "y", "dur", "disp_tol", "trial")
  return(data_fix)

}

trial_level_process <- function(data,
                                min_dur,
                                disp_tol,
                                run_interp,
                                round) {

  if (run_interp) data <- interpolate(data)

  data[,1] <- data[,1] - data[1,1,drop=TRUE] # start trial timestamps at 0

  sample_rate <- as.numeric(data[nrow(data),1]) / nrow(data)

  min_rows = round(min_dur/sample_rate) # convert min_dur into number of samples to search ahead in min/max

  seg <- as.matrix(data)

  min_max <- data.frame(roll_min(seg[,2:3], n = min_rows, fill = NA, align = "left"),
                        roll_max(seg[,2:3], n = min_rows, fill = NA, align = "left"))

  within_disp_tol <-
    abs(min_max[,1]-min_max[,3])<disp_tol &
    abs(min_max[,2]-min_max[,4])<disp_tol

  if (sum(is.na(within_disp_tol)==FALSE) > 0) { # if there are valid periods

    seg <- data.frame(seg, within_disp_tol)

    # this isn't going to be needed once part of trial_level_process proper - check if can be removed
    seg[is.na(seg$within_disp_tol),] <- FALSE # remove any NAs from the array

    # split dataframe into list by consecutive TRUE/FALSE on dispersion
    seg <- split(seg,cumsum(c(0,as.numeric(diff(seg$within_disp_tol))!=0)))

    # keep only valid fixation periods (TRUE), within dispersion threshold
    seg <- seg[sapply(seg, function(x) TRUE %in% x$within_disp_tol)]

    # get timestamps of start and end points of possible fixations
    seg_ind <- cbind(sapply(seg, function(x) x[1,1]),
                     sapply(seg, function(x) x[nrow(x),1]) + min_dur - 1) # probably needs fix/checking

    # remove indices that are too short (compared to min_dur)
    #seg_ind <- seg_ind[seg_ind[,2]-seg_ind[,1] < min_dur,]

    # extract these fixation periods from the original data
    fix_store <- NULL
    if (nrow(seg_ind)>0) {

      fix_store <- data.frame(matrix(NA, nrow = nrow(seg_ind), ncol = 7))

      for (f in 1:nrow(seg_ind)) {

        d <- dplyr::filter(data, between(time,seg_ind[f,1],seg_ind[f,2]))
        fix_store[f,1:2] <- c(seg_ind[f,1],seg_ind[f,2]) # first & last timestamps
        fix_store[f,3:4] <- round(colMeans(d[,2:3]), digits = round) # mean x and y coordinates
        fix_store[f,5] <- d[nrow(d),1] - d[1,1] # duration
        fix_store[f,6] <- disp_tol # dispersion
        fix_store[f,7] <- d[1,4] # trial number

      }

    }

  } else {
    # if no valid periods, return NAs as fixations
    fix_store <- data.frame(matrix(NA, nrow = 1, ncol = 7))
    fix_store[1,7] <- data[1,4] # add the trial number

  }

  return(fix_store)

}


