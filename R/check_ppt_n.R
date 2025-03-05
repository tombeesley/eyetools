
.check_ppt_n_in <- function(participant_col, data) {

  if (participant_col == 'participant_col') {

    if(is.null(data[['participant_col']])) {

      #this checks for repeated non-consecutive values of trial or time. If they are non-consecutive, error because it is likely
      #it should have the participant_col specified

      trial_rep <- duplicated(rle(data$trial)$values)
      #time_rep <- duplicated(rle(data$time)$values)

      if (sum(trial_rep) > 0) stop("multiple duplicated trials detected. Have you forgotten to specify the participant_col?")

      participant_col = "participant_col"
      data <- cbind(data, participant_col = c("NOT A VALID ID")) # just assign a value
    }
  } else {
    if(is.null(data[[participant_col]])) stop(paste0("No participant identifier column called '", participant_col, "' detected"))
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
