#' Plot raw data and fixations
#'
#' A tool for visualising raw eye-data and processed fixations. Fixations can be labelled
#' in the order they were made.
#'
#' @param raw_data data in standard raw data form (time, x, y, trial)
#' @param fix_data data output from fixation function (start, end, x, y, duration, trial)
#' @param res resolution of the display to be shown, as a vector (xmin, xmax, ymin, ymax)
#' @param flip_y reverse the y axis coordinates (useful if origin is top of the screen)
#' @param show_fix_order label the fixations in the order they were made
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @import dplyr
#'
#'

spatial_plot <- function(raw_data = NULL,
                         fix_data = NULL,
                         aoi = NULL,
                         res = NULL,
                         flip_y = FALSE,
                         show_fix_order = TRUE) {


  final_g <- ggplot()
  # PLOT AOIs
  if (is.null(aoi)==FALSE) {

    # need to split the AOI array here and plot separately tiles (squares) and points (circles)
    final_g <- final_g +
      geom_tile(data = aoi,
                aes(x = x, y = y, width = width, height = height),
                colour = "dark blue",
                fill = "blue",
                alpha = .1)


  }



  # PLOT RAW DATA
  if (is.null(raw_data)==FALSE) {

    final_g <- final_g +
      geom_point(data = raw_data,
                 aes(x = x, y = y),
                 size = .5)

  }

  # PLOT FIXATION DATA
  if (is.null(fix_data)==FALSE) {

    fix_data <- mutate(fix_data, fix_n = 1:n())

    final_g <- final_g +
      geom_point(data = fix_data,
                 aes(x = x, y = y, size = dur),
                 colour = "red",
                 alpha = .2)
    if (show_fix_order == TRUE) {

      final_g <- final_g +
        geom_label(data = fix_data,
                   aes(x = x, y = y, label = fix_n),
                   hjust = 1,
                   vjust = 1,
                   size = 4)

    }


  }

  final_g <- final_g +
    theme_classic(base_size = 18) +
    theme(panel.background = element_rect(fill = "#E0E0E0"),
          panel.border = element_rect(colour = "black",
                                      fill = NA,
                                      size = 4)) +
    scale_size_continuous(range = c(5,30)) +
    guides(size = FALSE) +
    labs(title = "eyetools::spatial_plot",
         subtitle = "Points are raw data; Circles are fixations (size = duration); blue regions are AOIs",
         y = "Vertical coordinate (pixels)",
         x = "Horizontal coordinate (pixels)")

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
