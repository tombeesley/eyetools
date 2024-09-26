#' A battery of metrics and plots to compare the two algorithms (dispersion and VTI)
#'
#' A tool for comparing the two different algorithms present in this package. This function is useful for assessing the data as well as exploring which algorithm is likely to fit data more appropriately.
#' The raw data is run through both algorithms (using the same specified dispersion tolerances, etc.) before making comparisons of the underlying data.
#'
#' @param data A dataframe with raw data (time, x, y, trial) for one participant
#' @param plot_fixations Whether to plot the detected fixations. default as TRUE
#' @param print_summary Whether to print the summary table. default as TRUE
#' @param sample_rate sample rate of the eye-tracker. If default of NULL, then it will be computed from the timestamp data and the number of samples. Supplied to the VTI algorithm
#' @param threshold velocity threshold (degrees of VA / sec) to be used for identifying saccades. Supplied to the VTI algorithm
#' @param min_dur Minimum duration (in milliseconds) of period over which fixations are assessed. Supplied to both algorithms.
#' @param min_dur_sac Minimum duration (in milliseconds) for saccades to be determined. Supplied to the VTI algorithm
#' @param disp_tol Maximum tolerance (in pixels) for the dispersion of values allowed over fixation period. Supplied to both algorithms
#' @param run_interp include a call to eyetools::interpolate on each trial. Supplied to the VTI algorithm
#' @param NA_tol the proportion of NAs tolerated within any window of samples that is evaluated as a fixation. Supplied to the dispersion algorithm
#' @param smooth include a call to eyetools::smoother on each trial. Supplied to the VTI algorithm
#'
#' @return a list of the fixation data, correlation output, and data used for plotting
#' @export
#'
#' @examples
#' compare_algorithms(eyetools::example_raw_WM[eyetools::example_raw_WM$trial %in% c(20:23),])
#'
#' # the default output can be suppressed
#' compare_algorithms(example_raw_WM[example_raw_WM$trial == 16,], print_summary = FALSE)
#'
#' @importFrom stats cor.test reshape time
#' @import ggplot2
#'

compare_algorithms <- function(data, plot_fixations = TRUE, print_summary = TRUE, sample_rate = NULL, threshold = 100, min_dur = 150, min_dur_sac = 20, disp_tol = 100, NA_tol = .25, run_interp = TRUE, smooth = FALSE) {

  #separate into trials
  data_split <- split(data, data$trial)

  data_list <- pbapply::pblapply(data_split, get_fixations, sample_rate, threshold, min_dur, min_dur_sac, disp_tol, NA_tol, run_interp, smooth)

  data_list_temp <- data_list[[1]]
  # get the data from comparing the two algorithms
  dataout <- lapply(data_list, summarise_comparisons)

  #get fixation data
  data_fix <- do.call(rbind, lapply(dataout, `[[`, 1))
  row.names(data_fix) <- NULL # remove the row names

  #get the correlations
  data_corr <- lapply(dataout, `[[`, 2)

  #get plot data
  data_plot <- do.call(rbind, lapply(dataout, `[[`, 3))

  #create plot
  plot <- ggplot(data_plot,
         aes(time, name, group = event_n)) +
    geom_line(linewidth=10) +
    facet_wrap(~trial, dir="v") +
    theme_bw()

  if (plot_fixations) {
        plot(plot)

      }

  data_list_out <- list()
  data_list_out[["fixations"]] <- data_fix
  data_list_out[["correlations"]] <- data_corr
  data_list_out[["plot"]] <- data_plot

  if (print_summary) {
    print(data_fix)

  }

return(data_list_out)

}

get_fixations <- function(data, sample_rate, threshold, min_dur, min_dur_sac, disp_tol, NA_tol, run_interp, smooth) {

  # run both algorithms usign the same parameters
  data_vti <- fixation_VTI(data, sample_rate = sample_rate, threshold = threshold, min_dur = min_dur, min_dur_sac = min_dur_sac, disp_tol = disp_tol, run_interp = run_interp, smooth = smooth, progress = FALSE)
  data_disp <- fixation_dispersion(data, min_dur = min_dur, disp_tol = disp_tol, NA_tol = NA_tol, progress = FALSE)

  # set time to begin at 0 for each trial
  data$time <- data$time - min(data$time)

  fix_store <- data.frame(fixations_vti = summarise_fixations(data_vti, data),
                          fixations_disp = summarise_fixations(data_disp, data))


  fix_store$time = 1:nrow(fix_store)-1

  fix_store <- merge(data, fix_store)

  list_store <- list()

  list_store[[1]] <- fix_store
  list_store[[2]] <- data_vti
  list_store[[3]] <- data_disp

  return(list_store)
}

summarise_fixations <- function(dataIn, data) {

  fixations <- rep(0, max(data$time))

  for (i in 1:nrow(dataIn)) {
    fixations[between(1:length(fixations), dataIn$start[i], dataIn$end[i])] <- 1
  }

  return(fixations)

}

summarise_comparisons <- function(dataIn) {
  out <- list()

  out$desc <-  data.frame(algorithm = c("vti", "dispersion"))

  out[['desc']]$trial <- unique(dataIn[[2]]$trial)

  ### calculate percentage of data classified as a fixation
  vti_percent <- sum(dataIn[[1]]$fixations_vti)*100/nrow(dataIn[[1]])
  disp_percent <- sum(dataIn[[1]]$fixations_disp)*100/nrow(dataIn[[1]])

  out[['desc']]$percent <- c(vti_percent, disp = disp_percent)

  ### get number of fixations detected
  out[['desc']]$fix_n <- c(max(dataIn[[2]]$fix_n), max(dataIn[[3]]$fix_n))

  #get correlation between algorithms
  correlation <- cor.test(dataIn[[1]]$fixations_vti, dataIn[[1]]$fixations_disp)
  correlation$data.name <- "VTI algorithm and dispersion algorithm"

  out[['corr']] <- correlation

  out[['desc']]$corr.r <-  correlation$estimate
  out[['desc']]$corr.p <-  correlation$p.value
  out[['desc']]$corr.t <-  correlation$statistic

  # create plot data
  data_to_plot <- reshape(dataIn[[1]], direction = "long", list(5:6), v.names = "value", timevar = NULL)
  data_to_plot[grepl("\\.1", rownames(data_to_plot)),]$id <- "fixations_vti"
  data_to_plot[grepl("\\.2", rownames(data_to_plot)),]$id <- "fixations_disp"
  #add in name variable and remove unnecessary ID
  data_to_plot$name <- data_to_plot$id
  data_to_plot$id <- NULL
  #reorder cols
  data_to_plot <- data_to_plot[,c(1:4,6,5)]
  #arrange by time for easier reading
  #data_to_plot <- arrange(data_to_plot, time)
  data_to_plot$name <- factor(data_to_plot$name)

  data_to_plot$value = ifelse(data_to_plot$value == 1 & data_to_plot$name == "fixations_disp", 2, data_to_plot$value)

  data_to_plot$event_n <- c(1,cumsum(abs(diff(data_to_plot$value)))+1) # get event numbers

  data_to_plot <- data_to_plot[data_to_plot$value != 0,]

  out[['plot']] <- data_to_plot


  return(out)
}
