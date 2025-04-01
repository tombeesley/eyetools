#' Plot raw data and fixations
#'
#' A tool for visualising raw eye-data, processed fixations, and saccades. Can use all three data types together and independently. Fixations can be labeled
#' in the order they were made. Can overlay areas of interest (AOIs) and customise the resolution.
#'
#' @param raw_data data in standard raw data form (time, x, y, trial)
#' @param fix_data data output from fixation function
#' @param sac_data data output from saccade function
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height). If using circular AOIs, then the 3rd column is used for the radius and the height should be set to NA.
#' @param pID_values specify particular values within 'pID' to plot data from certain participants
#' @param trial_values specify particular values within 'trial' to plot data from certain trials
#' @param bg_image The filepath of a PNG image to be added to the plot, for example to show a screenshot of the task.
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
#' # plot the raw data
#' plot_spatial(raw_data = data, pID_values = 118)
#'
#' # plot both raw and fixation data together
#' plot_spatial(raw_data = data, fix_data = fixation_dispersion(data), pID_values = 118)
#'
#' #plot one trial
#' plot_spatial(raw_data = data, fix_data = fixation_dispersion(data), trial_values = 6)
#'
#' }
#' @import ggplot2
#' @import ggforce
#' @import viridis
#' @importFrom magick image_read
#'

plot_spatial <- function(raw_data = NULL,
                         fix_data = NULL,
                         sac_data = NULL,
                         AOIs = NULL,
                         pID_values = NULL,
                         trial_values = NULL,
                         bg_image = NULL,
                         res = c(0,1920,0,1080),
                         flip_y = FALSE,
                         show_fix_order = TRUE,
                         plot_header = FALSE) {

  if(!is.null(trial_values) && !is.numeric(trial_values)) stop("trial_values input expected as numeric values")
  
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
    
    raw_data <- .select_pID_values(raw_data, pID_values, allow_random = FALSE)
    raw_data <- .select_trial_values(raw_data, trial_values, allow_random = FALSE)
    
    final_g <- add_raw(raw_data, final_g)
  }

  # PLOT FIXATION DATA
  if (is.null(fix_data)==FALSE) {

    fix_data <- .select_pID_values(fix_data, pID_values, allow_random = FALSE)
    fix_data <- .select_trial_values(fix_data, trial_values, allow_random = FALSE)

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
                  alpha = .4) + 
      scale_fill_viridis(breaks = c(min(duration),
                                    max(duration)),
                         labels = c("low", "high"))
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

    sac_data <- .select_pID_values(sac_data, pID_values, allow_random = FALSE)
    sac_data <- .select_trial_values(sac_data, trial_values, allow_random = FALSE)

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
               shape = 16,
               size = 3,
               alpha = .5,
               na.rm = TRUE)

  return(ggplot_in)
}

