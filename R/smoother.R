#' Smoothing of raw data
#'
#' A wrapper for the stats::loess function, with default parameters suitable for smoothing raw eye data
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#'

smoother <- function(data, span = 0.1) {

  smoother_trial <- function(data, span = 0.1){

    loess_x <- loess(x ~ time, data = data, span = span, na.action = na.exclude)
    loess_y <- loess(y ~ time, data = data, span = span, na.action = na.exclude)

    data$x <- predict(loess_x)
    data$y <- predict(loess_y)

    return(data)

  }

  data_s <- split(data, data$trial)
  by_trial_data <- lapply(data_s, smoother_trial)
  flat_trials <- do.call(rbind.data.frame, by_trial_data)
  return(flat_trials)

}
