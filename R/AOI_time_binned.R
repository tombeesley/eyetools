#' Binned time analysis of area of interest entries
#'
#' Analyses total time on defined AOI regions across trials separated into bins. Works with raw data as the input.
#' Data can be separated into bins of a given length of time and the number of bins per trial is calculated automatically, keeping the bin length
#' consistent across varying lengths of trial. Any data that cannot fill a bin (typically the last few milliseconds of the trial) are dropped to
#' ensure that bins are of a consistent length
#'
#' AOI_time_binned can take either single participant data or multiple participants, where participants are demarcated by values in the "pID" column.
#'
#' @param data A dataframe of raw data
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (name, x, y, width_radius, height).
#' @param bin_length the time duration to be used for each bin.
#' @param max_time maximum length of time to use, default is total trial length
#' @param as_prop whether to return time in AOI as a proportion of the total time of trial
#'
#' @return a dataframe containing the time on the passed AOIs for each trial. One column for each AOI separated by trial.
#' @export
#'
#' @examples
#'
#' \donttest{
#' data <- combine_eyes(HCL)
#'
#'
#' #with bins of 100ms each and only for the first 2000ms
#' AOI_time_binned(data = data, AOIs = HCL_AOIs, 
#'     bin_length = 100, max_time = 2000)
#' }
#'


AOI_time_binned <- function(data, AOIs, bin_length = NULL, max_time = NULL, as_prop = FALSE) {

  if(missing(bin_length)) stop("Requires bin_length")

  internal_AOI_time_binned <- function(data, AOIs, bin_length, max_time) {

    ppt_label <- data$pID[1]

    # process as raw data input
    proc_data <- lapply(split(data, data$trial),
                        AOI_binned_time_trial_process_raw,
                        AOIs = AOIs,
                        bin_length,
                        max_time)

data <- do.call('rbind.data.frame', proc_data)

    AOI_name_text <- c("trial", "bin_n", AOIs$name)

    data <- cbind(ppt_label, data)

    colnames(data) <- c("pID", AOI_name_text)

    return(data)
  }

  data <- split(data, data$pID)
  out <- lapply(data, internal_AOI_time_binned, AOIs, bin_length, max_time)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  if (as_prop) {

    #calculate prop
    out[,4:ncol(out)] <- out[,4:ncol(out)]/bin_length

    out[,4:ncol(out)][out[,4:ncol(out)] > 1] <- 1 #due to sample approximation, if over 1 then return to 1 as a value
    out$bin_time <- NULL

  }

  return(out)

}


AOI_binned_time_trial_process_raw <- function(trial_data, AOIs, bin_length, max_time) {

  # estimate sample rate
  if (is.null(the$eyetracker_properties$sample_frequency)) .estimate_sample_rate(trial_data)
  time_per_sample <- 1000/the$eyetracker_properties$sample_frequency

  if (is.null(max_time)) max_time <- max(trial_data$time) #set as the total trial time

  if(!is.null(bin_length)) {

    time_ceil <- max_time/bin_length*bin_length
    #split data into bins depending on the lengths given
    trial_data$bin <- cut(trial_data$time,
                          # The breaks line cuts off any data over the final bin length, so all are of ~equal length when bin_length is used
                          breaks = c(-Inf, seq(0, time_ceil, bin_length)[-1]), #drop the starting 0 in favour of -Inf
                          label = FALSE)

    data_bin <- split(trial_data, trial_data$bin)

  }


  data_binned <- split(trial_data, trial_data$bin)

  AOI_bin_process <- function(trial_data, AOIs, bin_length, max_time) {

    aoi_time_sums <- data.frame(matrix(nrow = 1, ncol = nrow(AOIs)))

    for (a in 1:nrow(AOIs)) {

      if (!is.na(AOIs[a,"height"])) {
        # square AOI
        xy_hits <- 
          (trial_data$x >= as.numeric(AOIs[a,"x"] - AOIs[a,"width_radius"]/2) &
             trial_data$x <= as.numeric(AOIs[a,"x"] + AOIs[a,"width_radius"]/2)) &
          (trial_data$y >= as.numeric(AOIs[a,"y"] - AOIs[a,"height"]/2) & trial_data$y <= as.numeric(AOIs[a,"y"] + AOIs[a,"height"]/2))
        
      } else if (is.na(AOIs[a,"height"]) & !is.na(AOIs[a,"width_radius"])) {
        # circle AOI
        xy_hits <- sqrt(as.numeric(AOIs[a,"x"]-trial_data$x)^2+as.numeric(AOIs[a,"y"]-trial_data$y)^2) < as.numeric(AOIs[a,"width_radius"])
      } else {
        # report error message of bad AOI definition
        stop("Bad AOI definition. Consider using function create_AOI_df()")
      }

      # convert hits into data on time and entries
      aoi_time_sums[a] <- round(sum(xy_hits*time_per_sample,
                                    na.rm = TRUE),0) # sum the valid AOI hits - multiply by duration of a sample

    }

    out <- data.frame(trial = trial_data$trial[1],
                      bin = trial_data$bin[1],
                      aoi_time_sums)

    return(out)
  }

  data_out <- lapply(data_binned, AOI_bin_process, AOIs, bin_length, max_time)
  data_out <- do.call('rbind.data.frame', data_out)

  return(data_out)

}
