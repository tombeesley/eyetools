#' Plot raw data and fixations
#'
#' A tool for visualising raw eye-data, processed fixations, and saccades. Can all three data types. Fixations can be labeled
#' in the order they were made. Can overlay areas of interest (AOIs) and customise the resolution.
#'
#' @param raw_data data in standard raw data form (time, x, y, trial)
#' @param fix_data data output from fixation function
#' @param sac_data data output from saccade function
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width/diameter, height/NA).
#' @param res resolution of the display to be shown, as a vector (xmin, xmax, ymin, ymax)
#' @param flip_y reverse the y axis coordinates (useful if origin is top of the screen)
#' @param show_fix_order label the fixations in the order they were made
#' @param plot_header display the header title text which explains graphical features of the plot.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import dplyr
#' @import ggforce
#'
#'

spatial_plot <- function(raw_data = NULL,
                         fix_data = NULL,
                         sac_data = NULL,
                         AOIs = NULL,
                         res = c(0,1920,0,1080),
                         flip_y = FALSE,
                         show_fix_order = TRUE,
                         plot_header = FALSE) {


  final_g <- ggplot()
  # PLOT AOIs
  if (is.null(AOIs)==FALSE) {

    rect_AOIs <- AOI_regions[!is.na(AOI_regions$height),]
    circle_AOIs <- AOI_regions[is.na(AOI_regions$height),] # those with NAs in height column

    # add any rectangle AOIs
    if (is.null(rect_AOIs)==FALSE) {
      final_g <- final_g +
        geom_tile(data = AOIs,
                  aes(x = x, y = y, width = width, height = height),
                  colour = "dark blue",
                  fill = "blue",
                  alpha = .1)
    }

    # add any circle AOIs
    if (is.null(circle_AOIs)==FALSE) {
      final_g <- final_g +
        geom_tile(data = AOIs,
                  aes(x = x, y = y, width = width, height = height),
                  colour = "dark blue",
                  fill = "blue",
                  alpha = .1)
    }


  }



  # PLOT RAW DATA
  if (is.null(raw_data)==FALSE) {

    final_g <-
      final_g +
      geom_point(data = raw_data,
                 aes(x = x, y = y),
                 size = 1,
                 na.rm = TRUE)

  }

  # PLOT FIXATION DATA
  if (is.null(fix_data)==FALSE) {

    fix_data <- mutate(fix_data, fix_n = 1:n())

    final_g <-
      final_g +
      geom_circle(data = fix_data,
                 aes(x0 = x, y0 = y, r = disp_tol/2, fill = duration),
                 alpha = .2)
    if (show_fix_order == TRUE) {

      final_g <-
        final_g +
        geom_label(data = fix_data,
                   aes(x = x, y = y, label = fix_n),
                   hjust = 1,
                   vjust = 1,
                   size = 4)

    }


  }

  # PLOT SACCADE DATA
  if (is.null(sac_data)==FALSE){

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
    theme_classic(base_size = 16) +
    theme(panel.background = element_rect(fill = "#E0E0E0"),
          panel.border = element_rect(colour = "black",
                                      fill = NA,
                                      size = 4)) +
    scale_fill_continuous(low = "yellow", high = "red") +
    coord_fixed() +
    guides(size = "none") +
    labs(y = "Vertical coordinate (pixels)",
         x = "Horizontal coordinate (pixels)")

  # add descriptive titles
  if (plot_header==TRUE){
    final_g <-
      final_g +
      labs(title = "eyetools::spatial_plot()",
           subtitle = "Raw data shown as dots; Fixations shown as circles (fill = duration); \nFixation size reflects dispersion of raw data; \nAOIs shown as blue regions")
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



return(final_g)


}
