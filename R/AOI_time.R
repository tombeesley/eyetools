#' Area of interest analysis at the trial level
#'
#' Analyses total time on defined AOI regions across trials. Currently only works with fixation data as the input.
#'
#' @param data A dataframe with fixation data (from fix_dispersion)
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc.
#'
#' @return a dataframe containing the time on the passed AOIs for each trial
#' @export
#'
#' @examples
#' fix_d <- fix_dispersion(eyetools::example_raw_WM)
#' AOI_time(fix_d, eyetools::AOIs_WM)
#'
#' @importFrom dplyr between
#'

AOI_time <- function(fix_data = NULL, raw_data = NULL, AOIs, AOI_names = NULL, sample_rate = NULL) {

  # dataframe to hold AOI entry results
  # columns are trial, AOI time * number of AOIs

  if (is.null(fix_data)==FALSE & is.null(raw_data)==FALSE) {
    # input data for both fixations and raw data
    stop("Attempt to use multiple input data objects; specify only one of fix_data or raw_data")

  } else if (is.null(fix_data)==FALSE) {

    # process as fixation data input
    proc_data <- sapply(split(fix_data, fix_data$trial),
                        AOI_trial_process_fix,
                        AOIs = AOIs)

    data <- cbind(unique(fix_data$trial), t(proc_data))

  } else if(is.null(raw_data)==FALSE) {

    # process as raw data input
    proc_data <- sapply(split(raw_data, raw_data$trial),
                        AOI_trial_process_raw,
                        AOIs = AOIs,
                        sample_rate = sample_rate)

    data <- cbind(unique(raw_data$trial), t(proc_data))

  }


  if (is.null(AOI_names)==FALSE) {
    AOI_name_text <- AOI_names
  }
  else {
    AOI_name_text <- sprintf("AOI_%s",1:nrow(AOIs))
  }

  colnames(data) <- c("trial", AOI_name_text)
  return(data.frame(data))

}


AOI_trial_process_fix <- function(trial_data, AOIs) {

  aoi_time_sums <- data.frame(matrix(nrow = 1, ncol = nrow(AOIs)))

    for (a in 1:nrow(AOIs)) {

      if (sum(!is.na(AOIs[a,])) == 4) {
        # square AOI
        xy_hits <- (between(trial_data$x, AOIs[a,1]-AOIs[a,3]/2, AOIs[a,1]+AOIs[a,3]/2) &
                      between(trial_data$y, AOIs[a,2]-AOIs[a,4]/2, AOIs[a,2]+AOIs[a,4]/2))
      } else if (sum(!is.na(AOIs[a,])) == 3) {
        # circle AOI
        xy_hits <- sqrt((AOIs[a,1]-trial_data$x)^2+(AOIs[a,2]-trial_data$y)^2) < AOIs[a,3]
      } else {
        # report error message of bad AOI definition

      }

      # convert hits into data on time and entries
      aoi_time_sums[a] <- sum(xy_hits*trial_data$dur) # sum the valid AOI hits

    }

  return(aoi_time_sums)

}

AOI_trial_process_raw <- function(trial_data, AOIs, sample_rate) {



  aoi_time_sums <- data.frame(matrix(nrow = 1, ncol = nrow(AOIs)))

  for (a in 1:nrow(AOIs)) {

    if (sum(!is.na(AOIs[a,])) == 4) {
      # square AOI
      xy_hits <- (between(trial_data$x, AOIs[a,1]-AOIs[a,3]/2, AOIs[a,1]+AOIs[a,3]/2) &
                    between(trial_data$y, AOIs[a,2]-AOIs[a,4]/2, AOIs[a,2]+AOIs[a,4]/2))
    } else if (sum(!is.na(AOIs[a,])) == 3) {
      # circle AOI
      xy_hits <- sqrt((AOIs[a,1]-trial_data$x)^2+(AOIs[a,2]-trial_data$y)^2) < AOIs[a,3]
    } else {
      # report error message of bad AOI definition

    }

    # convert hits into data on time and entries
    aoi_time_sums[a] <- round(sum(xy_hits*(1000/sample_rate), na.rm = TRUE),0) # sum the valid AOI hits - multiply by sample rate

  }

  return(aoi_time_sums)

}
