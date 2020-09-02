#' Fixation detection by dispersion method
#'
#' Detects fixations according to the method proposed by S & G
#'
#'
#' @param data dataframe with columns time, x, y, trial (the standardised raw data form for eyeproc)
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import purrr
#' @importFrom rlang .data
#' @importFrom zoo na.trim
#'
fix_dispersion <- function(data, min_dur = 50, disp_tol = 30, report = FALSE) {

  data <- split(data,data$trial) # create list from data by trial
  data_fix <- map(data, trial_level_process(min_dur = min_dur, disp_tol = disp_tol))
  data_fix <- do.call("rbind", data_fix)

}

trial_level_process <- function(data, min_dur, disp_tol, round = TRUE) {

  # need to think about how I catch NAs in the raw data
  # argument option to skip ahead to the next valid data, or average over NAs

  # also need to make min_dur based on ms not number of samples

  data[,1] <- data[,1] - as.numeric(data[1,1]) # start trial timestamps at 0

  data <- na.trim(data) # remove leading and trailing NAs
  first_ts <- 1 # first timestamp of window
  last_ts <- min_dur # last timestamp of window

  # preallocate a tibble to store fixations
  n <- ceiling(nrow(data)/min_dur) # maximum possible fixations
  fix_store <- data.frame(matrix(NA, nrow = n, ncol = 4))

  new_fix <- FALSE
  fix_cnt <- 0
  while (last_ts <= nrow(data)) {

    win <- data[first_ts:last_ts,]

    disp <- max(dist(win[,2:3])) # compute distance on X and Y

    if (disp <= disp_tol) {
      # increase window size
      last_ts <- last_ts + 1
      new_fix <- TRUE
      #browser()
    } else {
      # disp_tol has been exceeded
      if (new_fix == TRUE){ # record the fixation
        #browser()
        fix_cnt <- fix_cnt + 1
        fix_store[fix_cnt,1] <- as.numeric(win[1,1]) # start timestamp of the fixation
        fix_store[fix_cnt,2] <- tail(win[,1],n=1) - win[1,1] # subtract first from last timestamp for duration
        if (round == TRUE) {
          fix_store[fix_cnt,3:4] <- round(colMeans(win[,2:3])) # x/y coordinate means
        } else {
          fix_store[fix_cnt,3:4] <- colMeans(win[,2:3]) # x/y coordinate means
        }
        first_ts <- last_ts + 1
        last_ts <- first_ts + min_dur
        new_fix = FALSE
        #browser()
      } else {
        # move window by one timestamp
        first_ts <- first_ts + 1
        last_ts <- last_ts + 1
      }
    }
  }
  # tidy up fixation dataframe
  fix_store <- na.trim(fix_store)
  if (nrow(fix_store)>0) fix_store <- cbind(fix_store,data[1,4]) # gets the trial number from top row

  return(fix_store)
}


