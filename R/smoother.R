#' Smoothing of raw data
#'
#' A wrapper for the stats::loess function, with default parameters suitable for smoothing raw eye data
#'
#' It can take either single participant data or multiple participants where there is a variable for unique participant identification.
#' The function looks for an identifier named `participant_ID` by default and will treat this as multiple-participant data as default,
#' if not it is handled as single participant data, or the participant_ID needs to be specified
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param span From stats::loess. The parameter alpha which controls the degree of smoothing.
#' @param plot whether to plot the raw and smoothed plot for inspection
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
#' @return a dataframe of the same shape as the input data
#' @export
#'
#' @examples
#' data <- combine_eyes(HCL)
#'
#' smoother(data, participant_ID = "pNum")
#'
#' #with an inspection plot
#' smoother(data, span = .02, participant_ID = "pNum", plot = TRUE)
#'
#' @importFrom stats loess predict na.exclude
#'

smoother <- function(data, span = 0.1, plot = FALSE, participant_ID = "participant_ID") {

  #first check for multiple/single ppt data
  test <- .check_ppt_n_in(participant_ID, data)
  participant_ID <- test[[1]]
  data <- test[[2]]

  internal_smooth <- function(data, span) {

    data_s <- split(data, data$trial)
    by_trial_data <- lapply(data_s, smoother_trial, span)
    flat_trials <- do.call(rbind.data.frame, by_trial_data)
    return(flat_trials)
  }

  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_smooth, span)
  out <- do.call("rbind.data.frame", out)
  rownames(out) <- NULL

  out <- .check_ppt_n_out(out)
#browser()
  if (plot) {

    raw <- test[[2]]
    raw$participant_ID <- raw[,participant_ID]
    smooth <- out
    smooth$participant_ID <- smooth[,participant_ID]

    ppt <- sample(unique(raw$participant_ID), 1) #sample one participant
    trials <- sample(unique(raw$trial), 2) #sample two trials

    raw <- raw[raw$participant_ID == ppt,]
    raw <- raw[raw$trial %in% trials,]
    smooth <- smooth[smooth$participant_ID == ppt,]
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

  data <- .check_ppt_n_out(data)

  return(data)

}
