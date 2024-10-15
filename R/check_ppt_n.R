#' Checks for whether data is multi or single participant
#'
#' Two internal functions for processing data to detect participant_ID values
#'
#' @NoRd
#'

.check_ppt_n_in <- function(participant_ID, data) {

  if (participant_ID == 'participant_ID') {
    if(is.null(data[['participant_ID']])) {
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
