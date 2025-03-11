.check_data_format <- function(data) {
  
  if (length(intersect(colnames(data), c("pID", "trial", "x", "y", "time"))) < 5) {
    stop("The input data does not have the columns expected by eyetools. These are: pID, trial, x, y, time")
  }
  
}

.check_pID_values <- function(data, pID_values) {
  
  if (length(intersect(data$pID,pID_values)) != length(pID_values)){
    stop("At least one value supplied to parameter 'participant_values' was not found in column 'pID'")
  }
}

.check_trial_values <- function(data, trial_values) {
  
  if (length(intersect(data$trial,trial_values)) != length(trial_values)){
    stop("At least one value supplied to parameter 'trial_values' was not found in column 'trial'")
  }
}

.select_pID_values <- function(data, pID_values = NULL, allow_random = TRUE, allow_multiple = TRUE) {

  if(!is.null(pID_values)) {
    .check_pID_values(data, pID_values)
    data <- data[data$pID %in% pID_values,]
  } else if (allow_random == TRUE && allow_multiple == FALSE) {
    # get a random sample from the trial list
    pID_list <- unique(data$pID)
    
    if (length(pID_list)>1) {
      rand_pID <- sample(pID_list,1)
      message(paste0("Multiple pIDs detected: randomly sampled - pID:", rand_pID))
      data <- data[data$pID==rand_pID,]
    }
  }
}

.select_trial_values <- function(data, trial_values = NULL, allow_random = TRUE, allow_multiple = TRUE) {

  if(!is.null(trial_values) && !is.numeric(trial_values)) stop("'trial_values' parameter expected as numeric values")
  
  if(!is.null(trial_values)) {
    .check_trial_values(data, trial_values)
    data <- data[data$trial %in% trial_values,]
  } else if (allow_random == TRUE && allow_multiple == FALSE) {
    # get a random sample from the trial list
    trial_list <- unique(data$trial)
    
    if (length(trial_list)>1) {
      rand_trial <- sample(trial_list,1)
      message(paste0("Multiple trials detected: randomly sampled - trial:", rand_trial))
      data <- data[data$trial==rand_trial,]
    }
  }
}