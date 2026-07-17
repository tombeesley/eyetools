#' Compute visual angle from distance metrics
#'
#' Takes a single value or vector of distances and returns the visual angle equivalent. 
#'
#' @param vector vector of distances (or single distance)
#' @param dist_type default is "cm". Specify "pixel" for conversion from pixel values.
#'
#'
#' @return an equivalent-sized object to the input
#' @export
#'
#' @examples
#' # calculate visual angle for stimulus of 5cm
#' dist_to_visual_angle(5)
#'
#' # calculate visual angle of stimuli 2cm and 10cm width
#' dist_to_visual_angle(c(2,10))
#'
#' # calculate visual angle of 150 pixel wide
#' dist_to_visual_angle(150, dist_type = "pixels")
#'

dist_to_visual_angle <- function(vector, dist_type = "cm") {

  if (dist_type == "pixel") {
    # works out pixels per cm (assumes width==height)
    pix_per_cm_width  <- the$eyetracker_properties$screen_width_pixels/the$eyetracker_properties$screen_width_cm
    pix_per_cm_height  <- the$eyetracker_properties$screen_height_pixels/the$eyetracker_properties$screen_height_cm
    
    pix_per_cm <- mean(c(pix_per_cm_width, pix_per_cm_height))
    
    # convert the input vector to cm units
    vector <- vector/pix_per_cm
  }

  rad <- 2*atan(vector/(2*the$eyetracker_properties$viewing_distance_cm))
  ang = rad*(180/pi)
  return(ang)

}
