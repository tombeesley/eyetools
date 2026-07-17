#' Plot heatmap of raw data
#'
#' Plots a heatmap of raw data.
#'
#' @param data data in standard raw data form (time, x, y, trial)
#' @param pID_values specify particular values within 'pID' to plot data from certain participants
#' @param trial_values specify particular values within 'trial' to plot data from certain trials
#' @param bg_image The filepath of a PNG image to be added to the plot, for example to show a screenshot of the task.
#' @param flip_y reverse the y axis coordinates (useful if origin is top of the screen)
#' @param plot_type Specify the nature of the data displayed. Either "density" (default) or "hex" 
#' @param alpha_range a pair of values between 0 and 1. The first is a cut off, whereby lower values are not displayed. The second value sets the transparancy of the visible poitns. 
#' @param plot_header display the header title text which explains graphical features of the plot.
#'
#' @return a plot of the raw data
#' @export
#'
#' @examples
#' \donttest{
#' data <- combine_eyes(HCL)
#' # plot all trials data
#' plot_heatmap(data, pID_values = 118, alpha_range = c(0.3,0.8))
#'
#' #plot one trial
#' plot_heatmap(data, trial_values = 1)
#'
#' }
#' @import ggplot2
#' @import viridis
#' @importFrom magick image_read
#'

plot_heatmap <- function(data = NULL,
                         pID_values = NULL,
                         trial_values = NULL,
                         bg_image = NULL,
                         flip_y = FALSE,
                         plot_type = "density",
                         alpha_range = c(0.1,0.8),
                         plot_header = FALSE) {

  if(max(alpha_range) > 1 || min(alpha_range) < 0) stop("alpha_control values must be between 0 and 1")
  if(length(alpha_range))

  # check pID_values or select random pID
  data <- .select_pID_values(data, pID_values, allow_random = FALSE)
  
  # check trial_values or select random trial
  data <- .select_trial_values(data, trial_values, allow_random = FALSE)

  #mitigate undefined global functions note
  x <- data$x
  y <- data$y
  ndensity <- NULL

  # add data
  final_g <- ggplot(data)

  # setting axes limits and reversing y
  res_x <- the$eyetracker_properties$screen_width_pixels
  res_y <- the$eyetracker_properties$screen_height_pixels
  
  # creates breaks based on quarters. Might look messy with some resolutions
  breaks_x <- round(seq(0,res_x,res_x/4),0)
  breaks_y <- round(seq(0,res_y,res_y/4),0)

  final_g <- final_g +
    scale_x_continuous(limits = c(0,res_x),
                       breaks = breaks_x)
  if (flip_y==TRUE) {
    final_g <- 
      final_g +
      scale_y_reverse(limits = c(res_y,0),
                      breaks = rev(breaks_y)) 
  } else {
    final_g <- 
      final_g +
      scale_y_continuous(limits = c(0,res_y),
                         breaks = breaks_y)
  }

  # PLOT BACKGROUND IMAGE
  if (is.null(bg_image)==FALSE) final_g <- add_BGimg(bg_image, flip_y, final_g)
  
  # PLOT gridlines
  
  # major gridlines are just the breaks_*
  # minor are [0:34 + half the diff
  minor_breaks_x <- breaks_x[0:4] + (res_x/8)
  minor_breaks_y <- breaks_y[0:4] + (res_y/8)

  final_g <-
    final_g +
    geom_vline(xintercept = breaks_x, colour = "grey", alpha = .5) +
    geom_hline(yintercept = breaks_y, colour = "grey", alpha = .5) +
    geom_vline(xintercept = minor_breaks_x, colour = "lightgrey", alpha = .5) +
    geom_hline(yintercept = minor_breaks_y, colour = "lightgrey", alpha = .5)

  #plot data on top
  if (plot_type == "density") {
    final_g <- final_g +
      stat_density2d(geom="tile", 
                     aes(x, y, fill = after_stat(ndensity), alpha=ifelse(after_stat(ndensity) < alpha_range[1], 0, 1)),
                     contour = FALSE) 
    
  } else if (plot_type == "hex") {
  
    final_g <- final_g +
      stat_bin_hex(aes(x, y, alpha=ifelse(after_stat(ndensity) < alpha_range[1], 0, 1)), 
                   bins = 40)

  }

  final_g <-
    final_g +
    scale_alpha_continuous(range=c(0,alpha_range[2]),guide="none") +
    scale_fill_viridis(option = "B") +
    theme_minimal() +
    coord_fixed() +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = NA, colour = NA)) +
    labs(y = "Vertical coordinate (pixels)",
         x = "Horizontal coordinate (pixels)",
         fill = "Frequency")

  # add descriptive titles
  if (plot_header==TRUE){
    final_g <-
      final_g +
      labs(title = "eyetools::plot_heatmap()")
  }


  return(final_g)


}



