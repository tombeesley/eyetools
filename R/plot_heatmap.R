#' Plot heatmap of raw data
#'
#' A tool for visualising a heatmap of raw data.
#'
#' @param data data in standard raw data form (time, x, y, trial)
#' @param trial_number can be used to select particular trials within the data
#' @param bg_image The filepath of an image to be added to the plot, for example to show a screenshot of the task.
#' @param res resolution of the display to be shown, as a vector (xmin, xmax, ymin, ymax)
#' @param flip_y reverse the y axis coordinates (useful if origin is top of the screen)
#' @param alpha_control a single value to determine how much of the heatmap to obscure. Between 0 and 1. Lower values include more data in the heatmap
#' @param plot_header display the header title text which explains graphical features of the plot.
#'
#' @return a plot of the raw data
#' @export
#'
#' @examples
#' \donttest{
#' data <- combine_eyes(HCL)
#' data <- data[data$pNum == 118,]
#' # plot all trials data
#' plot_heatmap(data, alpha_control = .01)
#'
#' #plot one trial
#' plot_heatmap(data, trial_number = 1)
#'
#' }
#' @import ggplot2
#' @import viridis
#' @importFrom magick image_read
#'

plot_heatmap <- function(data = NULL,
                         trial_number = NULL,
                         bg_image = NULL,
                         res = c(0,1920,0,1080),
                         flip_y = FALSE,
                         alpha_control = 0.1,
                         plot_header = FALSE) {

  if(alpha_control > 1 || alpha_control < 0) stop("alpha_control must be between 0 and 1")

  if(!is.null(trial_number) && !is.numeric(trial_number)) stop("trial_number input expected as numeric values")


  if(!is.null(trial_number)) {
    data <- data[data$trial %in% trial_number,]
    if(nrow(data) == 0) stop("no trial found for fixation data. Check the data has the trials requested")
  }

  #mitigate undefined global functions note
  x <- data$x
  y <- data$y
  ndensity <- NULL

  # add data
  final_g <- ggplot(data)

  # setting axes limits and reversing y

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

  #plot data on top
  final_g <- final_g +
    stat_density2d(geom="tile",
                   aes(x, y, fill = after_stat(ndensity),
                       #alpha=cut(after_stat(density),breaks=c(0,1e-6,Inf))),
                       alpha=ifelse(after_stat(ndensity) < alpha_control, 0, 1)),
                   contour = FALSE) +
    scale_alpha_continuous(range=c(0,1),guide="none") +
    scale_fill_viridis_b()

  final_g <-
    final_g +
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


# function to add background image
add_BGimg <- function(bg_image_in, res, ggplot_in){
  img <- magick::image_read(bg_image_in)
  ggplot_in <-
    ggplot_in +
    annotation_raster(img,
                      xmin = res[1],
                      xmax = res[2],
                      ymin = res[3],
                      ymax = res[4])
  return(ggplot_in)

}
