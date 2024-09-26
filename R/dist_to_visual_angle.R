#' Compute visual angle from distance metrics
#'
#' Takes a single value or vector of distances and returns the visual angle equivalent.
#'
#' @param vector vector of distances (or single distance)
#' @param dist_type default is "cm". Specify "pixel" for conversion from pixel values.
#' @param view_dist_cm viewing distance in cm. Default of 60cm.
#' @param screen_width_cm used in conversion of pixel values. Default is 51 cm (24" monitor).
#' @param screen_width_pixels used in conversion of pixel values. Default is 1920 pixels.
#'
#'
#' @return an equivalent-sized object to the input
#' @export
#'
#' @examples
#' # calculate visual angle for stimulus of 5cm
#' dist_to_visual_angle(5)
#'
#' # calculate visual angle of stimuli 2 and 10cm width at 50 cm viewing angle
#' dist_to_visual_angle(c(2,10), view_dist_cm = 50)
#'
#' # calculate visual angle of 150 pixel wide
#' dist_to_visual_angle(150, dist_type = "pixels")
#'

dist_to_visual_angle <- function(vector,
                                 dist_type = "cm",
                                 view_dist_cm = 60,
                                 screen_width_cm = 51,
                                 screen_width_pixels = 1920) {

  if (dist_type == "pixel") {
    # works out pixels per cm (assumes width==height)
    pix_per_cm  <- screen_width_pixels/screen_width_cm

    # convert the input vector to cm units
    vector <- vector/pix_per_cm
  }

  rad <- 2*atan(vector/(2*view_dist_cm))
  ang = rad*(180/pi)
  return(ang)

}
