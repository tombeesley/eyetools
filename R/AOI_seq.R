#' Sequence analysis of area of interest entries
#'
#' Analyses the sequence of entries into defined AOI regions across trials. Should be used with fixation data.
#'
#'
#' @param data A dataframe with fixation data (from fixation_dispersion). Either single or multi participant data
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc.
#' @param sample_rate Optional sample rate of the eye-tracker (Hz) for use with raw_data. If not supplied, the sample rate will be estimated from the time column and the number of samples.
#' @param long Whether to return the AOI fixations in long or wide format. Defaults to long
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
#' @return a long format dataframe containing the sequence of entries into AOIs on each trial
#' @export
#'
#' @examples
#' fix_d <- fixation_dispersion(example_raw_WM)
#' AOI_seq(fix_d, AOIs = AOIs_WM)
#'
#' @importFrom stats setNames complete.cases
#' @importFrom utils stack

AOI_seq <- function(data, AOIs, AOI_names = NULL, sample_rate = NULL, long = TRUE, participant_ID = "participant_ID") {

  if(is.null(data[["fix_n"]])) stop("column 'fix_n' not detected. Are you sure this is fixation data from eyetools?")

  #first check for multiple/single ppt data
  test <- .check_ppt_n_in(participant_ID, data)
  participant_ID <- test[[1]]
  data <- test[[2]]

  #internal_AOI_seq carries the per-participant functionality to be wrapped in the lapply for ppt+ setup
  internal_AOI_seq <- function(data, AOIs, AOI_names, sample_rate, long) {

    # split data by trial
    proc_data <- sapply(split(data, data$trial),
                        AOI_seq_trial_process,
                        AOIs = AOIs,
                        AOI_names)

    data <- data.frame(data[[participant_ID]][1],
                       trial = unique(data$trial),
                       AOI_entry_seq = proc_data)

    colnames(data)[1] <- participant_ID #keep same column as entered

    if (long == TRUE) {

      split_list <- strsplit(data$AOI_entry_seq,';')

      split_list_names <- setNames(split_list, data$trial)

      data_long <- stack(split_list_names)

      data <- data.frame(participant_ID = data[[participant_ID]][1],
                         trial = as.numeric(data_long$ind),
                         AOI = data_long$value)

      # add in entry_n by way of indexing each trial
      get_row_n <- function(i) {
        store <- data[data$trial == i,]

        if (nrow(store) == 0) { store <- NULL} else {
          store$entry_n <- 1:nrow(store)}

        store
      }

      data <- do.call(rbind.data.frame, lapply(1:max(data$trial), get_row_n))

      data <- data[data$AOI != "NA",] # remove rows that are NA
    }

    #RETURN THE DATA TO THE SAME FORMAT IF SINGLE PPT
    if (data[['participant_ID']][1] == "NOT A VALID ID") data[['participant_ID']] <- NULL

    return(data)

      }

  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_AOI_seq, AOIs, AOI_names, sample_rate, long)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  return(out)
}


AOI_seq_trial_process <- function(trial_data, AOIs, AOI_names) {

  trial_data <- trial_data[complete.cases(trial_data),] # remove any NAs (i.e., in raw data)

  aoi_entries <- data.frame(matrix(nrow = nrow(trial_data), ncol = nrow(AOIs)))

  for (a in 1:nrow(AOIs)) {

    if (sum(!is.na(AOIs[a,])) == 4) {
      # square AOI
      aoi_entries[,a] <- ((trial_data$x >= AOIs[a,1]-AOIs[a,3]/2 & trial_data$x <= AOIs[a,1]+AOIs[a,3]/2) &
                            (trial_data$y >= AOIs[a,2]-AOIs[a,4]/2 & trial_data$y <= AOIs[a,2]+AOIs[a,4]/2))
    } else if (sum(!is.na(AOIs[a,])) == 3) {
      # circle AOI
      aoi_entries[,a] <- sqrt((AOIs[a,1]-trial_data$x)^2+(AOIs[a,2]-trial_data$y)^2) < AOIs[a,3]
    } else {
      # report error message of bad AOI definition
      stop("bad definition of AOI. Cannot identify AOI region")

    }
  }
  # this gives unique values in each row of which AOI had a hit
  aoi_entries <- as.matrix(aoi_entries)%*%diag(c(1:nrow(AOIs)))

  # simplify to vector of AOI entries
  aoi_seq <- rowSums(aoi_entries)
  #aoi_seq <- aoi_seq[aoi_seq>0] # remove fixations without aoi hits
  find_repeat_entries <- c(TRUE, diff(aoi_seq)!=0)
  aoi_seq <- aoi_seq[find_repeat_entries]
  aoi_seq <- aoi_seq[aoi_seq != 0] #remove non AOI fixations

  if (is.null(AOI_names)==FALSE) {
    aoi_seq <- paste0(AOI_names[aoi_seq], collapse = ";")
  } else {
    aoi_seq <- paste0(aoi_seq, collapse = ";")
  }

  aoi_seq <- .check_ppt_n_out(aoi_seq)

  return(aoi_seq)

}

