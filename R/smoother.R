#' Smoothing of raw data
#'
#' A wrapper for the stats::loess function, with default parameters suitable for smoothing raw eye data
#'
#' Analyses data separately for each unique combination of values in `pID` and `trial`.
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param span From stats::loess. The parameter alpha which controls the degree of smoothing.
#' @param plot whether to plot the raw and smoothed plot for inspection
#' @return a dataframe of the same shape as the input data
#' @export
#'
#' @examples
#' data <- combine_eyes(HCL)
#'
#' smoother(data)
#'
#' #with an inspection plot
#' smoother(data, span = .02, plot = TRUE)
#'
#' @importFrom stats loess predict na.exclude
#' @import ggplot2
#'

smoother <- function(data, span = 0.05, plot = FALSE) {
  
  raw <- data
  
  internal_smooth <- function(data, span) {

    data_s <- split(data, data$trial)
    by_trial_data <- lapply(data_s, smoother_trial, span)
    flat_trials <- do.call(rbind.data.frame, by_trial_data)
    return(flat_trials)
  }

  data <- split(data, data$pID)
  out <- lapply(data, internal_smooth, span)
  out <- do.call(rbind.data.frame, out)
  rownames(out) <- NULL

  if (plot) {

    smooth <- out

    ppt <- sample(unique(raw$pID), 1) #sample one participant
    trials <- sample(unique(raw$trial), 2) #sample two trials

    raw <- raw[raw$pID == ppt,]
    raw <- raw[raw$trial %in% trials,]
    smooth <- smooth[smooth$pID == ppt,]
    smooth <- smooth[smooth$trial %in% trials,]

    ####
    coord <- NULL
    raw_long <- reshape(raw, direction = "long", varying = list(c("x", "y")), v.names = "coord", timevar = "axis")
    smooth_long <- reshape(smooth, direction = "long", varying = list(c("x", "y")), v.names = "coord", timevar = "axis")

    raw_long[raw_long$axis == 1,]$axis <- "x"
    raw_long[raw_long$axis == 2,]$axis <- "y"
    smooth_long[smooth_long$axis == 1,]$axis <- "x"
    smooth_long[smooth_long$axis == 2,]$axis <- "y"
    ####

    message(paste("Showing trials:", paste(trials, collapse = ", "), "for participant", ppt))

    to_plot <- ggplot() +
      geom_line(data = raw_long,
                aes(x = time, y = coord),
                colour = "red") +
      geom_line(data = smooth_long,
                aes(x = time, y = coord),
                colour = "blue") +
      facet_wrap("trial ~ axis", scales = "free") +
      theme_minimal()

    plot(to_plot)
  }

  return(out)

}


smoother_trial <- function(data, span){

  loess_x <- loess(x ~ time, data = data, span = span, na.action = na.exclude)
  loess_y <- loess(y ~ time, data = data, span = span, na.action = na.exclude)

  data$x <- predict(loess_x)
  data$y <- predict(loess_y)

  return(data)

}
