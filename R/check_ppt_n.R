.check_data_format <- function(data) {
  
  if (length(intersect(colnames(data), c("pID", "trial", "x", "y", "time"))) < 5) {
    stop("The input data does not have the columns expected by eyetools. These are: pID, trial, x, y, time")
  }
  
}

.check_participant_parameter <- function(data, participant_values) {
  
  if (length(intersect(data$pID,participant_values)) != length(participant_values)){
    stop("At least one value supplied to parameter 'participant_values' was not found in column 'pID'")
  }
}

.check_trial_parameter <- function(data, trial_values) {
  
  if (length(intersect(data$trial,trial_values)) != length(trial_values)){
    stop("At least one value supplied to parameter 'trial_values' was not found in column 'trial'")
  }
}