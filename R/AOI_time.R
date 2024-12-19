#' Time analysis of area of interest entries
#'
#' Analyses total time on defined AOI regions across trials. Works with fixation and raw data as the input (must use one or the other, not both).
#'
#' AOI_time can take either single participant data or multiple participants where there is a variable for unique participant identification.
#' The function looks for an identifier named `participant_ID` by default and will treat this as multiple-participant data as default,
#' if not it is handled as single participant data, or the participant_ID needs to be specified
#'
#' @param data A dataframe  of either fixation data (from fix_dispersion) or raw data
#' @param data_type Whether data is a fixation ("fix") or raw data ("raw")
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc.
#' @param sample_rate Optional sample rate of the eye-tracker (Hz) for use with data. If not supplied, the sample rate will be estimated from the time column and the number of samples.
#' @param as_prop whether to return time in AOI as a proportion of the total time of trial
#' @param trial_time needed if as_prop is set to TRUE. a vector of the time taken in each trial. Equal to the length of x trials by y participants in the dataset
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
#'
#' @return a dataframe containing the time on the passed AOIs for each trial. One column for each AOI separated by trial.
#' @export
#'
#' @importFrom utils stack

#' @examples
#'
#' \donttest{
#' data <- combine_eyes(HCL)
#' fix_d <- fixation_dispersion(data, participant_ID = "pNum")
#'
#' # fixation data
#' AOI_time(data = fix_d, data_type = "fix", AOIs = HCL_AOIs, participant_ID = "pNum")
#'
#' #raw data
#' AOI_time(data = data, data_type = "raw", AOIs = HCL_AOIs, participant_ID = "pNum")
#'
#' #as proportional data
#' AOI_time(data = fix_d, data_type = "fix", AOIs = HCL_AOIs, participant_ID = "pNum",
#'          as_prop = TRUE, trial_time = HCL_behavioural$RT)
#'}


AOI_time <- function(data, data_type = NULL, AOIs, AOI_names = NULL, sample_rate = NULL, as_prop = FALSE, trial_time = NULL, participant_ID = "participant_ID") {

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

    colnames(data) <- c("trial", AOI_name_text)
    trial <- data$trial
    long_data <- stack(data, select = -trial)
    long_data <- cbind(rep(data$trial, length(AOI_name_text)), long_data)

    long_data <- cbind(ppt_label, long_data)

    colnames(long_data) <- c(participant_ID, "trial", "time", "AOI")

    return(long_data)
  }

  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_AOI_time, data_type, AOIs, AOI_names, sample_rate)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  out <- .check_ppt_n_out(out)

  if (as_prop) {

    if (length(trial_time) != nrow(out)/nrow(AOIs)) stop(paste("trial_time is not equal to the number of trials * participants in the data. Expected", nrow(out)/nrow(AOIs), "trial_time observations. Received", length(trial_time)))

    trial_time <- data.frame(trial_time)
    trial_time <- trial_time[rep(seq_len(nrow(trial_time)), each = nrow(AOIs)), ]

    out <- out[order(out$pNum, out$trial),]

    out$trial_time <- trial_time
    out$time <- out$time/out$trial_time
    out$trial_time <- NULL
    rownames(out) <- NULL
  }

  #reorder
  out <- out[, c(participant_ID, "trial", "AOI", "time")]
  return(out)

}


AOI_time_trial_process_fix <- function(trial_data, AOIs) {

  aoi_time_sums <- data.frame(matrix(nrow = 1, ncol = nrow(AOIs)))

  for (a in 1:nrow(AOIs)) {

    if (sum(!is.na(AOIs[a,])) == 4) {
      # square AOI
      xy_hits <- (trial_data$x >= as.numeric(AOIs[a,1] - AOIs[a,3]/2) & trial_data$x <= as.numeric(AOIs[a,1] + AOIs[a,3]/2)) &
        (trial_data$y >= as.numeric(AOIs[a,2] - AOIs[a,4]/2) & trial_data$y <= as.numeric(AOIs[a,2] + AOIs[a,4]/2))

    } else if (sum(!is.na(AOIs[a,])) == 3) {
      # circle AOI
      xy_hits <- sqrt(as.numeric(AOIs[a,1]-trial_data$x)^2+as.numeric(AOIs[a,2]-trial_data$y)^2) < as.numeric(AOIs[a,3])
    } else {
      # report error message of bad AOI definition

    }

    # convert hits into data on time and entries
    aoi_time_sums[a] <- sum(xy_hits*trial_data$dur) # sum the valid AOI hits

  }


  return(aoi_time_sums)

}

AOI_time_trial_process_raw <- function(trial_data, AOIs, sample_rate) {


  if (is.null(sample_rate)==TRUE) sample_rate <- .estimate_sample_rate(trial_data)
  sample_rate <- 1000/sample_rate

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

  return(aoi_time_sums)

}
