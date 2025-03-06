#' Sequence analysis of area of interest entries
#'
#' Analyses the sequence of entries into defined AOI regions across trials. Can only be used with fixation data with a "fix_n" column denoting fixation events.
#'
#'
#' @param data A dataframe with fixation data (from fixation_dispersion). Either single or multi participant data
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc.
#' @param progress Display a progress bar
#' @return a dataframe containing the sequence of entries into AOIs on each trial, entry/exit/duration time into AOI
#' @export
#'
#' @examples
#' \donttest{
#' data <- combine_eyes(HCL)
#' fix_d <- fixation_dispersion(data, participant_col = "pNum")
#'
#' AOI_seq(fix_d, AOIs = HCL_AOIs, participant_col = "pNum")
#' }
#'
#' @import pbapply
#' @importFrom stats setNames complete.cases

AOI_seq <- function(data, AOIs, AOI_names = NULL, progress = TRUE) {

  if(is.null(data[["fix_n"]])) stop("column 'fix_n' not detected. Are you sure you are supplying fixation data from eyetools?")

  # #first check for multiple/single ppt data
  # test <- .check_ppt_n_in(participant_col, data)
  # participant_col <- test[[1]]
  # data <- test[[2]]

  #internal_AOI_seq carries the per-participant functionality to be wrapped in the lapply for ppt+ setup
  internal_AOI_seq <- function(data, AOIs, AOI_names) {


    # split data by trial
    data <- do.call("rbind.data.frame", lapply(split(data, data$trial),
                                                    AOI_seq_trial_process,
                                                    AOIs = AOIs,
                                                    AOI_names))

    # colnames(data)[1] <- participant_col #keep same column as entered
    # 
    # 
    # #RETURN THE DATA TO THE SAME FORMAT IF SINGLE PPT
    # if (data[[participant_col]][1] == "NOT A VALID ID") data[[participant_col]] <- NULL

    return(data)

  }

  data <- split(data, data$pID)
  if(progress) out <- pblapply(data, internal_AOI_seq, AOIs, AOI_names) else out <- lapply(data, internal_AOI_seq, AOIs, AOI_names)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL
  # out <- .check_ppt_n_out(out)

  return(out)
}


AOI_seq_trial_process <- function(trial_data, AOIs, AOI_names) {

  trial_val <- trial_data$trial[[1]]
  ppt_val <- trial_data$pID[1]

  trial_data <- trial_data[complete.cases(trial_data),] # remove any NAs (i.e., in raw data)

  aoi_entries <- data.frame(matrix(nrow = nrow(trial_data), ncol = nrow(AOIs)))

  for (a in 1:nrow(AOIs)) {

    if (sum(!is.na(AOIs[a,])) == 4) {
      # square AOI
      aoi_entries[,a] <- ((trial_data$x >= as.numeric(AOIs[a,1]-AOIs[a,3]/2) & trial_data$x <= as.numeric(AOIs[a,1]+AOIs[a,3]/2)) &
                            (trial_data$y >= as.numeric(AOIs[a,2]-AOIs[a,4]/2) & trial_data$y <= as.numeric(AOIs[a,2]+AOIs[a,4]/2)))
    } else if (sum(!is.na(AOIs[a,])) == 3) {
      # circle AOI
      aoi_entries[,a] <- sqrt((as.numeric(AOIs[a,1])-trial_data$x)^2+(as.numeric(AOIs[a,2])-trial_data$y)^2) < as.numeric(AOIs[a,3])
    } else {
      # report error message of bad AOI definition
      stop("bad definition of AOI. Cannot identify AOI region")

    }
  }

  # check if trial has no fixations on any AOIs
  if (sum(aoi_entries)==0) {

    # if no data, return a trial result with NAs
    aoi_trial_out <- data.frame(pID = ppt_val,
                                trial = trial_val,
                                AOI = NA,
                                start = NA,
                                end = NA,
                                duration = NA,
                                entry_n = NA)

    aoi_trial_out

    return(aoi_trial_out)
  }

  # this gives unique values in each row of which AOI had a hit
  aoi_entries <- as.data.frame(as.matrix(aoi_entries)%*%diag(c(1:nrow(AOIs))))

  aoi_entries$string <- Reduce(paste0, aoi_entries) # get a string to check for duplicates

  aoi_entries$start <- trial_data$start
  aoi_entries$end <- trial_data$end

  aoi_entries$group <- cumsum(c(TRUE, diff(as.numeric(aoi_entries$string)) != 0))

  aoi_entries <- do.call('rbind.data.frame', lapply(split(aoi_entries, aoi_entries$group), function(data) {
    data$start <- min(data$start)
    data$end <- max(data$end)
    return(data)

  }))

  #next section removes duplicate consecutive AOI entries
  aoi_entries <- aoi_entries[!duplicated(with(rle(aoi_entries$string),rep(seq_along(values), lengths))),]
  #remove non AOI region fixations
  aoi_entries <- aoi_entries[aoi_entries$string != "000",]

  aoi_entries$AOI <- rowSums(aoi_entries[, -((ncol(aoi_entries) - 3):ncol(aoi_entries))]) # just the AOIs, remove all others


  aoi_trial_out <- data.frame(pID = ppt_val,
                              trial = trial_val,
                              AOI = aoi_entries$AOI,
                              start = aoi_entries$start,
                              end = aoi_entries$end,
                              duration = aoi_entries$end - aoi_entries$start)

  aoi_trial_out$entry_n <- as.numeric(rownames(aoi_trial_out))

  #replace values with AOI names if given
  if(!is.null(AOI_names)) {
    aoi_trial_out$AOI <- AOI_names[aoi_trial_out$AOI]

  }

  rownames(aoi_trial_out) <- NULL

  return(aoi_trial_out)

}

