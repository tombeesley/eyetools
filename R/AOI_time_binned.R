#' Binned time analysis of area of interest entries
#'
#' Analyses total time on defined AOI regions across trials separated into bins. Works with raw data as the input.
#' Data can be sepataed into bins of a given length of time and the number of bins per trial is calculated automatically, keeping the bin length
#' consistent across varying lengths of trial. Any r=data that cannot fill a bin (tpyically the last few milliseconds of the trial) are dropped to
#' ensure that bins are of a consistent length
#'
#' AOI_time_binned can take either single participant data or multiple participants where there is a variable for unique participant identification.
#' The function looks for an identifier named `participant_ID` by default and will treat this as multiple-participant data as default,
#' if not it is handled as single participant data, or the participant_ID needs to be specified
#'
#' @param data A dataframe  of raw data
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc.
#' @param sample_rate Optional sample rate of the eye-tracker (Hz) for use with data. If not supplied, the sample rate will be estimated from the time column and the number of samples.
#' @param bin_length the time duration to be used for each bin.
#' @param max_time maximum length of time to use, default is total trial length
#' @param as_prop whether to return time in AOI as a proportion of the total time of trial
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
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
#' AOI_time_binned(data = data, AOIs = HCL_AOIs, participant_ID = "pNum",
#'     bin_length = 100, max_time = 2000)
#' }
#'


AOI_time_binned <- function(data, AOIs, AOI_names = NULL, sample_rate = NULL, bin_length = NULL, max_time = NULL, as_prop = FALSE, participant_ID = "participant_ID") {

  if(missing(bin_length)) stop("Requires bin_length")

  #first check for multiple/single ppt data
  test <- .check_ppt_n_in(participant_ID, data)
  participant_ID <- test[[1]]
  data <- test[[2]]
  # dataframe to hold AOI entry results
  # columns are trial, AOI time * number of AOIs

  internal_AOI_time_binned <- function(data, AOIs, AOI_names, sample_rate, bin_length, max_time) {

    ppt_label <- data[[participant_ID]][[1]]

    # process as raw data input
    proc_data <- lapply(split(data, data$trial),
                        AOI_binned_time_trial_process_raw,
                        AOIs = AOIs,
                        sample_rate = sample_rate,
                        bin_length,
                        max_time)

data <- do.call('rbind.data.frame', proc_data)

    if (is.null(AOI_names)==FALSE) {
      AOI_name_text <- c("trial", "bin_n", AOI_names)
    } else {
      AOI_name_text <- c("trial", "bin_n", sprintf("AOI_%s",1:nrow(AOIs)))
    }

    data <- cbind(ppt_label, data)

    colnames(data) <- c(participant_ID, AOI_name_text)

    return(data)
  }

  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_AOI_time_binned, AOIs, AOI_names, sample_rate, bin_length, max_time)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  out <- .check_ppt_n_out(out)
  if (as_prop) {

    #calculate prop
    out[,4:ncol(out)] <- out[,4:ncol(out)]/bin_length

    out[,4:ncol(out)][out[,4:ncol(out)] > 1] <- 1 #due to sample approximation, if over 1 then return to 1 as a value
    out$bin_time <- NULL

  }

  return(out)

}


AOI_binned_time_trial_process_raw <- function(trial_data, AOIs, sample_rate, bin_length, max_time) {
  if (is.null(sample_rate)==TRUE){
    # estimate sample rate (ms) from timestamps and number of samples
    trial_data$time <- trial_data$time - trial_data$time[1] # start trial timestamps at 0
    sample_rate <- as.numeric(utils::tail(trial_data$time,n=1)) / nrow(trial_data)
  } else {
    sample_rate <- 1000/sample_rate # express in ms per sample
  }
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

  AOI_bin_process <- function(trial_data, AOIs, sample_rate, bin_length, max_time) {

    aoi_time_sums <- data.frame(matrix(nrow = 1, ncol = nrow(AOIs)))

    for (a in 1:nrow(AOIs)) {

      if (sum(!is.na(AOIs[a,])) == 4) {
        # square AOI
        xy_hits <- ((trial_data$x >= AOIs[a,1]-AOIs[a,3]/2 & trial_data$x <= AOIs[a,1]+AOIs[a,3]/2) &
                      (trial_data$y >= AOIs[a,2]-AOIs[a,4]/2 & trial_data$y <= AOIs[a,2]+AOIs[a,4]/2))
      } else if (sum(!is.na(AOIs[a,])) == 3) {
        # circle AOI
        xy_hits <- sqrt((AOIs[a,1]-trial_data$x)^2+(AOIs[a,2]-trial_data$y)^2) < AOIs[a,3]
      } else {
        # report error message of bad AOI definition
        stop("Bad AOI definition")

      }

      # convert hits into data on time and entries
      aoi_time_sums[a] <- round(sum(xy_hits*sample_rate,
                                    na.rm = TRUE),0) # sum the valid AOI hits - multiply by sample rate

    }

    out <- data.frame(trial = trial_data$trial[1],
                      bin = trial_data$bin[1],
                      aoi_time_sums)

    return(out)
  }

  data_out <- lapply(data_binned, AOI_bin_process, AOIs, sample_rate,  bin_length, max_time)
  data_out <- do.call('rbind.data.frame', data_out)

  return(data_out)

}
