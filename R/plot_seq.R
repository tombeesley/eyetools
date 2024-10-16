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
      stop("Error: trial number error? No data found")
    }

  } else {
    # get a random sample from the trial list
    trial_list <- unique(data$trial)

    if (length(trial_list)>1) {
      rand_trial <- sample(trial_list,1)
      print(paste0("Multiple trials detected: randomly sampled - trial:", rand_trial))
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
    data$bin_end <- data$bin*bin_time
    # data$bin_start <- data$bin_end-bin_time
    # data$bin_name <- as.factor(paste0(data$bin_start, "-", data$bin_end))

  }

  final_g <- ggplot(data = data)

  # PLOT BACKGROUND IMAGE
  if (is.null(bg_image)==FALSE) final_g <- add_BGimg(bg_image, res, final_g)

  # PLOT AOIs
  if (is.null(AOIs)==FALSE) final_g <- add_AOIs(AOIs, final_g)

  # add raw data
  if (is.null(data)==FALSE) final_g <- add_raw_time_seq(data, final_g)

  final_g <-
    final_g +
    theme_classic(base_size = 16) +
    theme(panel.background = element_rect(fill = "#E0E0E0"),
          panel.border = element_rect(colour = "black",
                                      fill = NA,
                                      size = 4)) +
    scale_colour_gradient2(low = "light yellow",
                           mid = "orange",
                           high = "dark red",
                           midpoint = median(data$time)) +
    coord_fixed() +
    guides(size = "none") +
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

  if (is.null(res)==FALSE) {
    # creates breaks based on quarters. Might look messy with some resolutions
    breaks_x = round(seq(res[1],res[2],(res[2]-res[1])/4),0)
    breaks_y = round(seq(res[3],res[4],(res[4]-res[3])/4),0)
  }

  if (is.null(res)==FALSE & flip_y==FALSE) {
    final_g <- final_g +
      scale_x_continuous(limits = res[1:2],
                         breaks = breaks_x) +
      scale_y_continuous(limits = res[3:4],
                         breaks = breaks_y)
  } else if (is.null(res)==FALSE & flip_y==TRUE) {
    final_g <- final_g +
      scale_x_continuous(limits = res[1:2],
                         breaks = breaks_x) +
      scale_y_reverse(limits = res[4:3],
                      breaks = breaks_y)
  }

  # add facet
  if (is.null(bin_time)==FALSE){
    final_g <-
      final_g +
      facet_wrap(vars(bin_end)) +
      theme(strip.text.x = element_text(size = 8, margin = unit(c(1,1,1,1), "points")),
            strip.background = element_rect(size = 0),
            panel.border = element_rect(size = 0),
            axis.text.y = element_text(size = 8),
            axis.text.x = element_text(size = 8))
  }



  return(final_g)


}

# function to add raw data
add_raw_time_seq <- function(dataIn, ggplot_in){

  ggplot_in <-
    ggplot_in +
    geom_point(data = dataIn,
               aes(x = x, y = y, colour = time),
               size = 5,
               alpha = .4,
               na.rm = TRUE)

  return(ggplot_in)
}
