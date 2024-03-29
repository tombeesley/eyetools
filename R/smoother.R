#' Smoothing of raw data
#'
#' A wrapper for the stats::loess function, with default parameters suitable for smoothing raw eye data
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param span From stats::loess. The parameter α which controls the degree of smoothing.
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom stats loess predict na.exclude
#'

smoother <- function(data, span = 0.1) {

  data_s <- split(data, data$trial)
  by_trial_data <- lapply(data_s, smoother_trial)
  flat_trials <- do.call(rbind.data.frame, by_trial_data)
  return(flat_trials)

}


smoother_trial <- function(data, span = 0.1){

  loess_x <- loess(x ~ time, data = data, span = span, na.action = na.exclude)
  loess_y <- loess(y ~ time, data = data, span = span, na.action = na.exclude)

  data$x <- predict(loess_x)
  data$y <- predict(loess_y)

  return(data)

}
