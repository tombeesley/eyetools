#' Smoothing of raw data
#'
#' A wrapper for the stats::loess function, with default parameters suitable for smoothing raw eye data
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param span From stats::loess. The parameter α which controls the degree of smoothing.
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
#' @return a dataframe of the same shape as the input data
#' @export
#'
#' @examples
#'
#' smoother(example_raw_WM)
#'
#' @importFrom stats loess predict na.exclude
#'

smoother <- function(data, span = 0.1, participant_ID = "participant_ID") {

  #first check for multiple/single ppt data
  if (participant_ID == 'participant_ID') {
    if(is.null(data[['participant_ID']])) {
      participant_ID = "participant_ID"
      data <- cbind(data, participant_ID = c("NOT A VALID ID")) # just assign a value
    }
  } else {
    if(is.null(data[[participant_ID]])) stop(paste0("No participant identifier column called '", participant_ID, "' detected"))
  }

  internal_smooth <- function(data, span) {

    data_s <- split(data, data$trial)
    by_trial_data <- lapply(data_s, smoother_trial)
    flat_trials <- do.call(rbind.data.frame, by_trial_data)
    return(flat_trials)
  }

  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_smooth, span)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  if (out[['participant_ID']][1] == "NOT A VALID ID") out[['participant_ID']] <- NULL

  return(out)

}


smoother_trial <- function(data, span = 0.1){

  loess_x <- loess(x ~ time, data = data, span = span, na.action = na.exclude)
  loess_y <- loess(y ~ time, data = data, span = span, na.action = na.exclude)

  data$x <- predict(loess_x)
  data$y <- predict(loess_y)

  return(data)

}
