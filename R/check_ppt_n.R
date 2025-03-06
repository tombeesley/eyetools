
.check_ppt_n_in <- function(data) {

    if(is.null(data$pID)) {

      #this checks for repeated non-consecutive values of trial or time. If they are non-consecutive, error because it is likely
      #it should have the participant_col specified

      trial_rep <- duplicated(rle(data$trial)$values)
      
      if (sum(trial_rep) > 0) stop("The data contain duplicated non-consecutive trials, which suggests multi-participant data. Ensure a 'pID' column in used to define the data from different participants.")

      participant_col = "participant_col"
      data <- cbind(data, participant_col = c("NOT A VALID ID")) # just assign a value
    }

  return(list(participant_col, data))
}

.check_ppt_n_out <- function(data) {
  if(!is.null(data[['participant_col']])) {

    if(data[['participant_col']][1] == "NOT A VALID ID") {
      data[['participant_col']] <- NULL
    }
  }

  return(data)
}

.check_data_format <- function(data) {
  if (length(intersect(colnames(data), c("pID", "trial", "x", "y", "time"))) < 5) {
    
    stop("The input data does not have the columns expected by eyetools. These are: pID, trial, x, y, time")
    
  }
  
}