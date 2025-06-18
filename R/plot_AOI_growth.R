#' Plots absolute or proportional time spent in AOIs over time
#'
#' A visualisation tool for plotting the changes in defined AOI regions across a single trial time.
#'
#' @param data raw data in standard raw data form (time, x, y, trial)
#' @param pID_values specify particular values within 'pID' to plot data from certain participants
#' @param trial_values can be used to select particular trials within the data
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height)
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc. To omit AOIs from the plot, use NA in relevant vector position
#' @param type either "abs" (absolute) or "prop" (proportion)
#' @param plot_time_not_in_AOI boolean as to whether to include proportion of time spent outside AOIs
#'
#' @return a plot of the raw data
#' @export
#'
#' @examples
#'
#' \donttest{
#' data <- combine_eyes(HCL)
#' data <- interpolate(data)
#' # plot absolute and then proportional
#' plot_AOI_growth(data = data, AOIs = HCL_AOIs, type = "abs")
#' plot_AOI_growth(data = data, AOIs = HCL_AOIs, type = "prop")
#' }
#'
#' @import ggplot2
#' @import viridis
#' @importFrom zoo na.locf
#' @importFrom stats ave


plot_AOI_growth <- function(data = NULL, pID_values = NULL, trial_values = NULL, AOIs = NULL, AOI_names = NULL, type = "abs", plot_time_not_in_AOI = FALSE) {

  #error catches
  if(!is.null(trial_values) && !is.numeric(trial_values)) stop("trial_number input expected as numeric values")
  
  if(is.null(data[['x']]) || is.null(data[['y']])) stop("No x or y variables detected")
  if(!(type %in% c("abs", "prop"))) stop("type should be 'abs' or 'prop'.")
  if(is.null(AOIs)) {stop("Dataframe of Areas of Interest must be specified using AOIs =") }
  else {
    if(!is.null(AOI_names)) {
      if(nrow(AOIs) != length(AOI_names)) stop("AOI_names is not the same length as the number of AOIs detected")
    }
  }

  # check pID_values or select random pID
  data <- .select_pID_values(data, pID_values, allow_random = TRUE)
  
  # check trial_values or select random trial
  data <- .select_trial_values(data, trial_values, allow_random = TRUE)
  
  
  in_AOI <- NULL
  data$in_AOI <- apply(data, 1, function(row) {
    x <- as.numeric(row["x"])
    y <- as.numeric(row["y"])

    # Loop through each row in AOIs to find the correct AOI
    for (i in 1:nrow(AOIs)) {
      x_range <- c(AOIs$x[i] - AOIs$width_radius[i] / 2,
                   AOIs$x[i] + AOIs$width_radius[i] / 2)
      y_range <- c(AOIs$y[i] - AOIs$height[i] / 2,
                   AOIs$y[i] + AOIs$height[i] / 2)

      if(is.na(x) || is.na(y)) return(NA) #early exit if no data present

      # Check if x and y fall within the AOI range
      if (x >= x_range[1] && x <= x_range[2] &&
          y >= y_range[1] && y <= y_range[2]) {
        if(is.null(AOI_names)) return(paste0("AOI_", i))
        else return(AOI_names[i])
      }
    }
    # Return "out of AOI" if no AOI matches
    return("out of AOI")
  })

  time_diff <- c(0, diff(data$time))
  data$time_diff = time_diff

  # Calculate cumulative time differences for each group of in_AOI
  data <- data[order(data$in_AOI, data$time), ]  # Ensure data is ordered correctly for cumulative sum
  data$time_diff <- ave(data$time_diff, data$in_AOI, FUN = cumsum)

  # Fill in missing combinations of (pID, time, trial, in_AOI)
  all_combinations <- expand.grid(pID = unique(data$pID),
                                  time = unique(data$time),
                                  trial = unique(data$trial),
                                  in_AOI = unique(data$in_AOI))
  data <- merge(all_combinations, data, by = c("pID", "time", "trial", "in_AOI"), all.x = TRUE)


  # Fill missing time_diff values
  data$time_diff[is.na(data$time_diff) & data$time == 0] <- 0
  data <- data[order(data$in_AOI, data$time), ]  # Ensure data is ordered for filling
  data$time_diff <- ave(data$time_diff, data$in_AOI, FUN = function(x) zoo::na.locf(x, na.rm = FALSE))
#browser()
  # Calculate proportion
  prop <- data$time_diff / data$time
  data$prop <- prop

  #remove the out of AOI
  if (plot_time_not_in_AOI == FALSE) data <- data[data$in_AOI != "out of AOI",]
  data <- na.omit(data)

  if(type == "prop") {
  plot <- ggplot(data, aes(time, prop, colour = in_AOI, group = in_AOI)) +
    labs(x = "Time", y = "Proportion of Time", colour = "Area of Interest")
  }

  if(type == "abs") {
    plot <- ggplot(data, aes(time, time_diff, colour = in_AOI, group = in_AOI)) +
      labs(x = "Time in Trial", y = "Cumulative Time on AOI", colour = "Area of Interest")
  }

  plot +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    scale_color_viridis_d()
}

