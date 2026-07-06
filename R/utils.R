.check_data_format <- function(data) {
  
  if (length(intersect(colnames(data), c("pID", "trial", "x", "y", "time"))) < 5) {
    stop("The input data does not have the columns expected by eyetools. These are: pID, trial, x, y, time")
  }
  
}

.check_pID_values <- function(data, pID_values) {
  
  if (length(intersect(data$pID,pID_values)) != length(pID_values)){
    stop("At least one value supplied to parameter 'participant_values' was not found in column 'pID'")
  }
}

.check_trial_values <- function(data, trial_values) {
  
  if (length(intersect(data$trial,trial_values)) != length(trial_values)){
    stop("At least one value supplied to parameter 'trial_values' was not found in column 'trial'")
  }
}

# This is a utility function for estimating the sample_rate in Hz based off the timestamps given.
.estimate_sample_rate <- function(data) {
  
  # estimate sample rate
  
  trial <- split(data, data$trial)
  sample_rates <- sapply(trial, function(data) {
    
    # estimate sample rate (ms) from difference between timestamps
    time <- data$time - data$time[1] # start trial timestamps at 0
    sample_rate <- mean(diff(time)) #difference between timestamps, expressing ms per sample
    sample_rate
    
  })
  #average sample rate across all trials
  sample_rate <- 1000/mean(sample_rates)
  the$eyetracker_properties$sample_frequency <- sample_rate
  message(paste0("Eye tracker frequency estimated at:", sample_rate, "Hz"))
}


.select_pID_values <- function(data, pID_values = NULL, allow_random = TRUE) {

  if(!is.null(pID_values)) {
    .check_pID_values(data, pID_values)
    data <- data[data$pID %in% pID_values,]
  } else if (allow_random == TRUE) {
    # get a random sample from the trial list
    pID_list <- unique(data$pID)
    
    if (length(pID_list)>1) {
      rand_pID <- sample(pID_list,1)
      message(paste0("Multiple pIDs detected: randomly sampled - pID:", rand_pID))
      data <- data[data$pID==rand_pID,]
    }
  } else {
    return(data) # no selection made - return data as is. 
  }
}

.select_trial_values <- function(data, trial_values = NULL, allow_random = TRUE) {

  if(!is.null(trial_values) && !is.numeric(trial_values)) stop("'trial_values' parameter expects numeric values")
  
  if(!is.null(trial_values)) {
    .check_trial_values(data, trial_values)
    data <- data[data$trial %in% trial_values,]
  } else if (allow_random == TRUE) {
    # get a random sample from the trial list
    trial_list <- unique(data$trial)
    
    if (length(trial_list)>1) {
      rand_trial <- sample(trial_list,1)
      message(paste0("Multiple trials detected: randomly sampled - trial:", rand_trial))
      data <- data[data$trial==rand_trial,]
    }
  } else {
    return(data) # no selection made - return data as is. 
  }
}

# function to add background image
#' @import png
#' @import abind
add_BGimg <- function(bg_image_in, res, flip_y, ggplot_in){
  
  im <- png::readPNG(bg_image_in)
  if (dim(im)[3] == 3) im <- abind::abind(im, matrix(1, ncol=ncol(im), nrow=nrow(im)))
  im2 <- matrix(grDevices::rgb(im[,,1],im[,,2],im[,,3], im[,,4] * 0.5), nrow=dim(im)[1]) ## you can change 0.5 to change the alpha
  
  if (flip_y == TRUE) {
    res[4] <- -res[4] # shifts the background image to the flipped axis
  }
  
  ggplot_in <-
    ggplot_in +
    annotation_custom(grid::rasterGrob(im2),
                      xmin = res[1],
                      xmax = res[2],
                      ymin = res[3],
                      ymax = res[4])
  
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
                linewidth = 1,
                fill = "red",
                alpha = .2)
  }
  
  # add any circle AOIs
  if (is.null(circle_AOIs)==FALSE) {
    ggplot_in <-
      ggplot_in +
      geom_circle(data = circle_AOIs,
                  aes(x0 = x, y0 = y, r = width_radius),
                  colour = "dark blue",
                  linewidth = 1,
                  fill = "red",
                  alpha = .2)
  }
  
  return(ggplot_in)
  
}