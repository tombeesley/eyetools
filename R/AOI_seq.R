#' Sequence analysis of area of interest entries
#'
#' Analyses the sequence of entries into defined AOI regions across trials. Works with fixation data or raw data as the input.
#'
#' @param data A dataframe with fixation data (from fix_dispersion) or raw data.
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc.
#' @param sample_rate Optional sample rate of the eye-tracker (Hz) for use with raw_data. If not supplied, the sample rate will be estimated from the time column and the number of samples.
#' @param long Whether to return the AOI fixations in long or wide format. Defaults to long
#' @return a long format dataframe containing the sequence of entries into AOIs on each trial
#' @export
#'
#' @examples
#' fix_d <- fixation_dispersion(example_raw_WM)
#' AOI_seq(fix_d, AOIs_WM)
#'
#' @importFrom stats setNames complete.cases
#' @importFrom utils stack

AOI_seq <- function(data,
                    AOIs,
                    AOI_names = NULL,
                    sample_rate = NULL,
                    long = TRUE) {

  # split data by trial
  proc_data <- sapply(split(data, data$trial),
                      AOI_seq_trial_process,
                      AOIs = AOIs,
                      AOI_names)

  data <- data.frame(trial = unique(data$trial),
                     AOI_entry_seq = proc_data)

  if (long == TRUE) {
    #data <- separate_longer_delim(data, AOI_entry_seq, delim = ";")

    split_list <- strsplit(data$AOI_entry_seq,';')

    split_list_names <- setNames(split_list, data$trial)

    data <- stack(split_list_names)

    data <- data.frame(trial = data$ind,
           AOI_entry_seq = data$value)

  }


  return(data)

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

    }
  }
  # this gives unique values in each row of which AOI had a hit
  aoi_entries <- as.matrix(aoi_entries)%*%diag(c(1:nrow(AOIs)))

  # simplify to vector of AOI entries
  aoi_seq <- rowSums(aoi_entries)
  aoi_seq <- aoi_seq[aoi_seq>0] # remove fixations without aoi hits
  find_repeat_entries <- c(TRUE, diff(aoi_seq)!=0)
  aoi_seq <- aoi_seq[find_repeat_entries]

  if (is.null(AOI_names)==FALSE) {
    aoi_seq <- paste0(AOI_names[aoi_seq], collapse = ";")
  } else {
    aoi_seq <- paste0(aoi_seq, collapse = ";")
  }

  return(aoi_seq)

}

