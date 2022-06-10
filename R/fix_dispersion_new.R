#' Fixation detection by dispersion method
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
fix_dispersion <- function(data, min_dur = 150, disp_tol = 100) {

  trial_level_process <- function(data, min_dur, disp_tol, round = 0) {

    data <- interpolate(data)
    data[,1] <- data[,1] - data[1,1,drop=TRUE] # start trial timestamps at 0
    data <- na.trim(data) # remove leading and trailing NAs
    data$fix_num  <- NA # add a column that stores the fix number

    sample_rate <- as.numeric(tail(data[,1],n=1)) / nrow(data)
    first_ts <- 1 # first timestamp of window

    fix_cnt  <- 1
    new_window <- TRUE

    while (last_ts <= nrow(data)) { # while not at the end of the data

      if (new_window == TRUE){

        last_ts <- min(which(data[,1] >= data[first_ts,1,drop=TRUE] + min_dur)) # last timestamp of window
        win <- data[first_ts:last_ts,]
        max_d_win <- max(dist(win[,2:3])) # get max dispersion across this new window

        if(max_d_win<= disp_tol){
          # start of a fixation
          data[first_ts:last_ts,"fix_num"] <- fix_cnt

          new_window = FALSE # not a new window; look to extend fixation

        } else {
          # shift window along 1 timestamp
          first_ts  <- first_ts + 1
          last_ts  <- last_ts + 1
        }
      } else { # extend the window

        last_ts  <- last_ts + 1
        # compute the new distances from this new data point
        max_d_new_data <- max(cdist(data[last_ts,2:3],win[,2:3]))

        if (is.nan(max_d_new_data) | max_d_new_data >= disp_tol) {
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

    # preallocate a tibble to store fixations
    fix_store <- data.frame(matrix(NA, nrow = max(data$fix_num), ncol = 4))
    summarise_fixations <- function(data){
      start <- data[1,1] # first timestamp
      end <- data[1,nrow(data_s)] # last timestamp


      return(c(start,end))

    }

    data_s <- split(data,data$fix_num) # create list from data by trial


  }

  data <- split(data,data$trial) # create list from data by trial
  data_fix <- data %>% map(~trial_level_process(., min_dur = min_dur, disp_tol = disp_tol))
  data_fix <- do.call("rbind", data_fix)
  colnames(data_fix) <- c("start", "dur", "x", "y", "trial")
  return(data_fix)

  # seems to be a problem with trial 8 in example_raw_psy

}





