#' Plot of raw data over time
#'
#' A tool for visualising the timecourse of raw data over a single trial. If data from multiple trials are present, then
#' a single trial will be sampled at random. Alternatively, the trial_number can be specified. Data can be plotted across the whole
#' trial, or can be split into bins to present distinct plots for each time window.
#'
#' @param data A dataframe with raw data. If multiple trials are used, then one trial is sampled at random.
#' @param trial_number can be used to select a particular trial within the data
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param bg_image The filepath of an image to be added to the plot, for example to show a screenshot of the task.
#' @param res resolution of the display to be shown, as a vector (xmin, xmax, ymin, ymax)
#' @param flip_y reverse the y axis coordinates (useful if origin is top of the screen)
#' @param plot_header display the header title text which explains graphical features of the plot.
#' @param bin_time if wanting to split data into bins, the time (in ms) for each bin of data to be displayed
#' @param bin_range if wanting to split data into bins, the first and last bin to be display, e.g., c(1,5)
#'
#'
#' @return a plot of the raw data representing changes over time
#' @export
#'
#' @examples
#' data <- combine_eyes(HCL)
#'
#' # plot the raw data
#' plot_seq(data = data[data$pNum == 118,])
#'
#' # with AOIs
#' plot_seq(data = data[data$pNum == 118,], AOIs = HCL_AOIs)
#'
#' # plot raw data with bins
#' plot_seq(data = data[data$pNum == 118,], bin_time = 500)
#'
#' @import ggplot2
#' @importFrom utils head
#' @importFrom stats median
#'
#'

plot_seq <- function(data = NULL,
                     trial_number = NULL,
                     AOIs = NULL,
                     bg_image = NULL,
                     res = c(0,1920,0,1080),
                     flip_y = FALSE,
                     plot_header = FALSE,
                     bin_time = NULL,
                     bin_range = NULL) {


  # if there is a particular trial number specified
  if (!is.null(trial_number)){
    data <- data[data$trial==trial_number,]
    if (nrow(data)==0){
      stop("With specified trial number no data found.")
    }

  } else {
    # get a random sample from the trial list
    trial_list <- unique(data$trial)

    if (length(trial_list)>1) {
      rand_trial <- sample(trial_list,1)
      message(paste0("Multiple trials detected: randomly sampled - trial:", rand_trial))
      data <- data[data$trial==rand_trial,]
    }
  }

  data$time <- data$time - data$time[1] # start trial timestamps at 0

  if (is.null(bin_time)==FALSE){
    data$bin <- ceiling(data$time/bin_time)
    data$bin[1] <- 1
    if (is.null(bin_range)==FALSE){
      data <- data[data$bin >= head(bin_range, 1) & data$bin <= tail(bin_range,1),]
    }
    bin_end <- data$bin*bin_time
    data$bin_end <- bin_end
      # data$bin_start <- data$bin_end-bin_time
      # data$bin_name <- as.factor(paste0(data$bin_start, "-", data$bin_end))

  }

  final_g <- ggplot(data = data)

  #create breaks based on resolution
  if (is.null(res)==FALSE) {
    # creates breaks based on quarters. Might look messy with some resolutions
    breaks_x = round(seq(res[1],res[2],(res[2]-res[1])/4),0)
    breaks_y = round(seq(res[3],res[4],(res[4]-res[3])/4),0)
  }

  if (is.null(res)==FALSE && flip_y==FALSE) {
    final_g <- final_g +
      scale_x_continuous(limits = res[1:2],
                         breaks = breaks_x) +
      scale_y_continuous(limits = res[3:4],
                         breaks = breaks_y)
  } else if (is.null(res)==FALSE && flip_y==TRUE) {
    final_g <- final_g +
      scale_x_continuous(limits = res[1:2],
                         breaks = breaks_x) +
      scale_y_reverse(limits = res[4:3],
                      breaks = breaks_y)
  }

  # PLOT BACKGROUND IMAGE
  if (is.null(bg_image)==FALSE) final_g <- add_BGimg(bg_image, res, final_g)

  # PLOT gridlines

  # major gridlines are just the breaks_*
  # minor are [0:34 + half the diff
minor_breaks_x <- breaks_x[0:4] + ((res[2]-res[1])/8)
minor_breaks_y <- breaks_y[0:4] + ((res[4]-res[3])/8)

  final_g <-
    final_g +
    geom_vline(xintercept = breaks_x, colour = "grey", alpha = .5) +
    geom_hline(yintercept = breaks_y, colour = "grey", alpha = .5) +
    geom_vline(xintercept = minor_breaks_x, colour = "lightgrey", alpha = .5) +
    geom_hline(yintercept = minor_breaks_y, colour = "lightgrey", alpha = .5)

  # PLOT AOIs
  if (is.null(AOIs)==FALSE) final_g <- add_AOIs(AOIs, final_g)

  # add raw data
  if (is.null(data)==FALSE) final_g <- add_raw_time_seq(data, final_g)

  final_g <-
    final_g +
    theme_minimal() +
    coord_fixed() +
    viridis::scale_colour_viridis(breaks = c(min(final_g$data$time),
                                             max(final_g$data$time)),
                                  labels = c("start", "end")) +
    theme(legend.position = "bottom",
          #panel.ontop=TRUE,
          panel.background = element_rect(fill = NA, colour = NA)) +#,
         # panel.grid.major = element_line(linewidth = .25),
          #panel.grid.minor = element_line(linewidth = .125)) +
    labs(y = "Vertical coordinate (pixels)",
         x = "Horizontal coordinate (pixels)")

  # add descriptive titles
  if (plot_header==TRUE){
    final_g <-
      final_g +
      labs(title = "eyetools::plot_seq()",
           subtitle = "Raw data shown as dots; \nAOIs shown as blue regions")
  }

  # setting axes limits and reversing y

  # add facet
  if (is.null(bin_time)==FALSE){
    final_g <-
      final_g +
      facet_wrap(vars(bin_end))
  }



  return(final_g)


}

# function to add raw data
add_raw_time_seq <- function(dataIn, ggplot_in){

  x <- dataIn$x
  y <- dataIn$y

  ggplot_in <-
    ggplot_in +
    geom_point(data = dataIn,
               aes(x = x, y = y, colour = time),
               shape = 4,
               size = 3,
               alpha = .5,
               na.rm = TRUE)

  return(ggplot_in)
}
