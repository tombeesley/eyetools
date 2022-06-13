#' Fixation detection by dispersion method OLD - no longer in us
#'
#' Detects fixations according to the method proposed by S & G
#'
#'
#' @param data dataframe with columns time, x, y, trial (the standardised raw data form for eyeproc)
#'
#' @return
#'
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import purrr
#' @importFrom rlang .data
#' @importFrom zoo na.trim
#' @importFrom RcppRoll roll_min roll_max
#'
#' @keywords internal
#'
fix_dispersion_old <- function(data, min_dur = 150, disp_tol = 100) {

  trial_level_process <- function(data, min_dur, disp_tol, round = 0) {

    # need to think about how I catch NAs in the raw data
    # argument option to skip ahead to the next valid data, or average over NAs

    data <- interpolate(data)

    data[,1] <- data[,1] - data[1,1,drop=TRUE] # start trial timestamps at 0

    sample_rate <- as.numeric(tail(data[,1],n=1)) / nrow(data)
    #browser()

    data <- na.trim(data) # remove leading and trailing NAs
    first_ts <- 1 # first timestamp of window
    last_ts <- min(which(data[,1] >= data[first_ts,1,drop=TRUE] + min_dur)) # last timestamp of window

    # preallocate a tibble to store fixations
    n <- ceiling(nrow(data)/min_dur) # maximum possible fixations
    fix_store <- data.frame(matrix(NA, nrow = n, ncol = 4))

    new_fix <- FALSE
    new_Window <- FALSE
    fix_cnt <- 0
    dist_cnt <- 0
    while (last_ts <= nrow(data)) { # while not at the end of the data

      win <- data[first_ts:last_ts,]

      if (anyNA(win)==FALSE) {

        disp <- max(dist(win[,2:3])) # compute euclidean distance between all rows on X and Y

        if (disp <= disp_tol) {
          # increase window size
          last_ts <- last_ts + 1
          new_fix <- TRUE
        } else {
          # disp_tol has been exceeded

          if (new_fix == TRUE){ # record a new valid fixation

            fix_cnt <- fix_cnt + 1
            fix_store[fix_cnt,1] <- win[1,1,drop=TRUE] # start timestamp of the fixation
            fix_store[fix_cnt,2] <- tail(win[,1],n=1) - win[1,1] # subtract first from last timestamp for duration
            fix_store[fix_cnt,3:4] <- round(colMeans(win[,2:3]), digits = round) # x/y coordinate means
            new_Window <- FALSE
            new_fix <- FALSE

          } else {
            # move window by one timestamp
            first_ts <- first_ts + 1
            last_ts <- last_ts + 1
          }
        }
      } else {
        new_Window = TRUE
      }    # window contains NAs, shift it on

      if (new_Window) {
        new_Window <- FALSE
        first_ts <- last_ts + 1
        last_ts <- min(which(data[,1] >= data[first_ts,1,drop=TRUE] + min_dur)) # last timestamp of window

      }


    }
    # tidy up fixation dataframe
    fix_store <- na.trim(fix_store)
    if (nrow(fix_store)>0) fix_store <- cbind(fix_store,data[1,4]) # gets the trial number from top row

    return(fix_store)
  }



  data <- split(data,data$trial) # create list from data by trial
  data_fix <- data %>% map(~trial_level_process(., min_dur = min_dur, disp_tol = disp_tol))
  data_fix <- do.call("rbind", data_fix)
  colnames(data_fix) <- c("start", "dur", "x", "y", "trial")
  return(data_fix)

  # seems to be a problem with trial 8 in example_raw_psy

}





