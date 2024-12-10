#' Plot heatmap of raw data
#'
#' A tool for visualising a heatmap of raw data.
#'
#' @param data data in standard raw data form (time, x, y, trial)
#' @param trial_number can be used to select particular trials within the data
#' @param bg_image The filepath of an image to be added to the plot, for example to show a screenshot of the task.
#' @param res resolution of the display to be shown, as a vector (xmin, xmax, ymin, ymax)
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
#' plot_heatmap(data[data$pNum == 118,], alpha_control = .01)
#'
#' #plot one trial
#' plot_heatmap(data, trial_number = 1)
#'
#' }
#' @import ggplot2
#' @importFrom magick image_read
#'

plot_heatmap <- function(data = NULL,
                         trial_number = NULL,
                         bg_image = NULL,
                         res = c(0,1920,0,1080),
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

  # PLOT BACKGROUND IMAGE
  if (is.null(bg_image)==FALSE) final_g <- add_BGimg(bg_image, res, final_g)

  #plot data on top
  final_g <- final_g +
    stat_density2d(geom="tile",
                   aes(x, y, fill = after_stat(ndensity),
                       #alpha=cut(after_stat(density),breaks=c(0,1e-6,Inf))),
                       alpha=ifelse(after_stat(ndensity) < alpha_control, 0, 1)),
                   contour = FALSE) +
    scale_alpha_continuous(range=c(0,1),guide="none") +
    scale_fill_viridis_b() +
    lims(x = c(res[1],res[2]), y = c(res[3], res[4]))

  final_g <-
    final_g +
    theme_classic(base_size = 16) +
    theme(panel.background = element_rect(fill = "#E0E0E0"),
          panel.border = element_rect(colour = "black",
                                      fill = NA,
                                      size = 4)) +
    coord_fixed() +
    guides(size = "none") +
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
