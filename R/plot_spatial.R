#' Plot raw data and fixations
#'
#' A tool for visualising raw eye-data, processed fixations, and saccades. Can use all three data types together and independently. Fixations can be labeled
#' in the order they were made. Can overlay areas of interest (AOIs) and customise the resolution.
#'
#' @param raw_data data in standard raw data form (time, x, y, trial)
#' @param fix_data data output from fixation function
#' @param sac_data data output from saccade function
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height). If using circular AOIs, then the 3rd column is used for the radius and the height should be set to NA.
#' @param trial_number can be used to select particular trials within the data
#' @param bg_image The filepath of an image to be added to the plot, for example to show a screenshot of the task.
#' @param res resolution of the display to be shown, as a vector (xmin, xmax, ymin, ymax)
#' @param flip_y reverse the y axis coordinates (useful if origin is top of the screen)
#' @param show_fix_order label the fixations in the order they were made
#' @param plot_header display the header title text which explains graphical features of the plot.
#'
#' @return a plot of the raw data
#' @export
#'
#' @examples
#' \donttest{
#' data <- combine_eyes(HCL)
#' data <- data[data$pNum == 118,]
#' # plot the raw data
#' plot_spatial(raw_data = data)
#'
#' # plot both raw and fixation data together
#' plot_spatial(raw_data = data, fix_data = fixation_dispersion(data))
#'
#' #plot one trial
#' plot_spatial(raw_data = data, fix_data = fixation_dispersion(data), trial_number = 1)
#'
#' }
#' @import ggplot2
#' @import ggforce
#' @importFrom magick image_read
#'

plot_spatial <- function(raw_data = NULL,
                         fix_data = NULL,
                         sac_data = NULL,
                         AOIs = NULL,
                         trial_number = NULL,
                         bg_image = NULL,
                         res = c(0,1920,0,1080),
                         flip_y = FALSE,
                         show_fix_order = TRUE,
                         plot_header = FALSE) {

  if(!is.null(trial_number) && !is.numeric(trial_number)) stop("trial_number input expected as numeric values")

  final_g <- ggplot()

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

  # PLOT AOIs
  if (is.null(AOIs)==FALSE) final_g <- add_AOIs(AOIs, final_g)

  # add raw data
  if (is.null(raw_data)==FALSE) {

    if(!is.null(trial_number)) {
      raw_data <- raw_data[raw_data$trial %in% trial_number,]
      if(nrow(raw_data) == 0) stop("no trial found for raw data. Check the data has the trials")
    }

    final_g <- add_raw(raw_data, final_g)
  }

  # PLOT FIXATION DATA
  if (is.null(fix_data)==FALSE) {

    if(!is.null(trial_number)) {
      fix_data <- fix_data[fix_data$trial %in% trial_number,]
      if(nrow(fix_data) == 0) stop("no trial found for fixation data. Check the data has the trials")
    }

    fix_data$fix_n <- seq_len(nrow(fix_data))

    x <- fix_data$x
    y <- fix_data$y
    disp_tol <- fix_data$disp_tol
    duration <- fix_data$duration
    fix_n <- fix_data$fix_n

    final_g <-
      final_g +
      geom_circle(data = fix_data,
                  aes(x0 = x, y0 = y, r = disp_tol/2, fill = duration),
                  alpha = .4)
    if (show_fix_order == TRUE) {

      final_g <-
        final_g +
        geom_label(data = fix_data,
                   aes(x = x, y = y, label = fix_n),
                   hjust = 1,
                   vjust = 1,
                   size = 4) +
        viridis::scale_fill_viridis(breaks = c(min(duration),
                                               max(duration)),
                                    labels = c("low", "high"))

    }


  }

  # PLOT SACCADE DATA
  if (is.null(sac_data)==FALSE){

    if(!is.null(trial_number)) {
      sac_data <- sac_data[sac_data$trial %in% trial_number,]
      if(nrow(sac_data) == 0) stop("no trial found for saccade data. Check the data has the trials")
    }

    origin_x <- sac_data$origin_x
    origin_y <- sac_data$origin_y
    terminal_x <- sac_data$terminal_x
    terminal_y <- sac_data$terminal_y

    final_g <-
      final_g +
      geom_segment(data = sac_data,
                   aes(x = origin_x, y = origin_y, xend = terminal_x, yend = terminal_y),
                   colour = "blue",
                   arrow = arrow(length = unit(0.5, "cm")),
                   lineend = "round",
                   linejoin = "mitre",
                   size = 1)
  }

  final_g <-
    final_g +
    theme_minimal() +
    coord_fixed() +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = NA, colour = NA)) +
    labs(y = "Vertical coordinate (pixels)",
         x = "Horizontal coordinate (pixels)")

  # add descriptive titles
  if (plot_header==TRUE){
    final_g <-
      final_g +
      labs(title = "eyetools::plot_spatial()",
           subtitle = "Raw data shown as dots; Fixations shown as circles (fill = duration); \nFixation size reflects dispersion of raw data; \nAOIs shown as blue regions")
  }



  return(final_g)


}

# function to add raw data
add_raw <- function(dataIn, ggplot_in){

  x <- dataIn$x
  y <- dataIn$y

  ggplot_in <-
    ggplot_in +
    geom_point(data = dataIn,
               aes(x = x, y = y),
               #size = 1,
               shape = 4,
               alpha = .5,
               na.rm = TRUE)

  return(ggplot_in)
}

# function to add AOIs
add_AOIs <- function(AOIs, ggplot_in){

  x <- AOIs$x
  y <- AOIs$y
  width_radius <- AOIs$width_radius
  height <- AOIs$height

  rect_AOIs <- AOIs[!is.na(AOIs$height),]
  circle_AOIs <- AOIs[is.na(AOIs$height),] # those with NAs in height column

  # add any rectangle AOIs
  if (is.null(rect_AOIs)==FALSE) {
    ggplot_in <-
      ggplot_in +
      geom_tile(data = rect_AOIs,
                aes(x = x, y = y, width = width_radius, height = height),
                colour = "dark blue",
                fill = "blue",
                alpha = .1)
  }

  # add any circle AOIs
  if (is.null(circle_AOIs)==FALSE) {
    ggplot_in <-
      ggplot_in +
      geom_circle(data = circle_AOIs,
                  aes(x0 = x, y0 = y, r = width_radius),
                  colour = "dark blue",
                  fill = "blue",
                  alpha = .1)
  }

  return(ggplot_in)

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
