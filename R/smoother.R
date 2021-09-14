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

  loess_x <- loess(x ~ time, data = data, span = span, na.action = na.exclude)
  loess_y <- loess(y ~ time, data = data, span = span, na.action = na.exclude)

  data$x <- predict(loess_x)
  data$y <- predict(loess_y)

  return(data)

}
