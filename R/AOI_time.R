#' Time analysis of area of interest entries
#'
#' Analyses total time on defined AOI regions across trials. Works with fixation and raw data as the input (must use one or the other, not both).
#'
#' @param data A dataframe  of either fixation data (from fix_dispersion) or raw data
#' @param data_type Whether data is a fixation ("fix") or raw data ("raw")
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc.
#' @param sample_rate Optional sample rate of the eye-tracker (Hz) for use with data. If not supplied, the sample rate will be estimated from the time column and the number of samples.
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
#'
#' @return a dataframe containing the time on the passed AOIs for each trial
#' @export
#'
#' @examples
#' data <- combine_eyes(HCL)
#' fix_d <- fixation_dispersion(data, participant_ID = "pNum")
#'
#' # fixation data
#' AOI_time(data = fix_d, data_type = "fix", AOIs = HCL_AOIs)
#'
#' #raw data
#' AOI_time(data = data, data_type = "raw", AOIs = HCL_AOIs, sample_rate = 120)


AOI_time <- function(data, data_type = NULL, AOIs, AOI_names = NULL, sample_rate = NULL, participant_ID = "participant_ID") {

  #first check for multiple/single ppt data
  test <- .check_ppt_n_in(participant_ID, data)
  participant_ID <- test[[1]]
  data <- test[[2]]
  # dataframe to hold AOI entry results
  # columns are trial, AOI time * number of AOIs

internal_AOI_time <- function(data, data_type, AOIs, AOI_names, sample_rate) {
  if (is.null(data_type) == TRUE) {
    # input data for both fixations and raw data
    stop("Type of data not specified. Use `data_type = 'fix'` for fixations or `data_type = 'raw'` for raw data")

  } else if (data_type == "fix") {

    ppt_label <- data[[participant_ID]][[1]]
    # process as fixation data input
    proc_data <- sapply(split(data, data$trial),
                        AOI_time_trial_process_fix,
                        AOIs = AOIs)

    data <- cbind(unique(data$trial), t(proc_data))

  } else if(data_type == "raw") {
    ppt_label <- data[[participant_ID]][[1]]

    # process as raw data input
    proc_data <- sapply(split(data, data$trial),
                        AOI_time_trial_process_raw,
                        AOIs = AOIs,
                        sample_rate = sample_rate)

    data <- cbind(unique(data$trial), t(proc_data))

  }


  if (is.null(AOI_names)==FALSE) {
    AOI_name_text <- AOI_names
  } else {
    AOI_name_text <- sprintf("AOI_%s",1:nrow(AOIs))
  }


  data <- data.frame(data)

  data <- do.call(cbind.data.frame, lapply(1:length(colnames(data)), function(i) {

      data[,i] <- as.numeric(data[,i])

    return(data[,i])
  }))

  data <- cbind(ppt_label, data)
  colnames(data) <- c(participant_ID, "trial", AOI_name_text)

  return(data)
}

  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_AOI_time, data_type, AOIs, AOI_names, sample_rate)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  data <- .check_ppt_n_out(out)

  return(data)

}


AOI_time_trial_process_fix <- function(trial_data, AOIs) {

  aoi_time_sums <- data.frame(matrix(nrow = 1, ncol = nrow(AOIs)))

    for (a in 1:nrow(AOIs)) {

      if (sum(!is.na(AOIs[a,])) == 4) {
        # square AOI
        xy_hits <- (trial_data$x >= (AOIs[a,1] - AOIs[a,3]/2) & trial_data$x <= (AOIs[a,1] + AOIs[a,3]/2)) &
          (trial_data$y >= (AOIs[a,2] - AOIs[a,4]/2) & trial_data$y <= (AOIs[a,2] + AOIs[a,4]/2))

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

AOI_time_trial_process_raw <- function(trial_data, AOIs, sample_rate) {

  if (is.null(sample_rate)==TRUE){
    # estimate sample rate (ms) from timestamps and number of samples
    trial_data[,1] <- trial_data[,1] - trial_data[1,1,drop=TRUE] # start trial timestamps at 0
    sample_rate <- as.numeric(utils::tail(trial_data[,1],n=1)) / nrow(trial_data)
  } else {
    sample_rate <- 1000/sample_rate # express in ms per sample
  }


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

    }

    # convert hits into data on time and entries
    aoi_time_sums[a] <- round(sum(xy_hits*sample_rate,
                                  na.rm = TRUE),0) # sum the valid AOI hits - multiply by sample rate

  }

  return(aoi_time_sums)

}
