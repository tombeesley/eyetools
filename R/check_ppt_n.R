
.check_ppt_n_in <- function(participant_ID, data) {

  if (participant_ID == 'participant_ID') {

    if(is.null(data[['participant_ID']])) {

      #this checks for repeated non-consecutive values of trial or time. If they are non-consecutive, error because it is likely
      #it should have the participant_ID specified
      trial_rep <- duplicated(rle(data$trial)$values)
      time_rep <- duplicated(rle(data$time)$values)

      if (sum(trial_rep) > 0) stop("multiple duplicated trials detected. Have you forgotten to specify the participant_ID?")
      if (sum(time_rep) > 0) stop("multiple duplicated timepoints detected. Have you forgotten to specify the participant_ID?")

      participant_ID = "participant_ID"
      data <- cbind(data, participant_ID = c("NOT A VALID ID")) # just assign a value
    }
  } else {
    if(is.null(data[[participant_ID]])) stop(paste0("No participant identifier column called '", participant_ID, "' detected"))
  }

  return(list(participant_ID, data))
}

.check_ppt_n_out <- function(data) {
  if(!is.null(data[['participant_ID']])) {

    if(data[['participant_ID']][1] == "NOT A VALID ID") {
      data[['participant_ID']] <- NULL
    }
  }

  return(data)
}
