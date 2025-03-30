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
add_BGimg <- function(bg_image_in, res, ggplot_in){
  
  library(png)
  library(grid)
  
  im <- readPNG(bg_image_in)
  im2 <- matrix(rgb(im[,,1],im[,,2],im[,,3], im[,,4] * 0.5), nrow=dim(im)[1]) ## you can change 0.5 to change the alpa
  
  ggplot_in <-
    ggplot_in +
    annotation_custom(rasterGrob(im2),
                      xmin = res[1],
                      xmax = res[2],
                      ymin = res[3],
                      ymax = res[4])
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