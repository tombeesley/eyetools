the <- new.env(parent = emptyenv())
the$eyetracker_properties <- 
  list(sample_frequency = NULL, 
       viewing_distance_cm = 60,
       screen_width_cm = 53,
       screen_height_cm = 30,
       screen_width_pixels = 1920,
       screen_height_pixels = 1080)

#' Report my favorite letters
#' @export
mfl2 <- function() {
  the$favorite_letters
}

#' Change my favorite letters
#' @export
set_mfl2 <- function(l = letters[24:26]) {
  old <- the$favorite_letters
  the$favorite_letters <- l
  invisible(old)
}