#' Set the properties of the eye-tracker that was used in recording the data 
#' 
#' This function is used to specify the properties of the eye-tracker used to record the data. 
#' Users can update one or more parameters. Unnamed parameters retain previous values. 
#' If these are not set by the user, then default parameters are set and a warning is issued in every call to relevant functions
#'
#'
#' @param sample_frequency Numeric frequency.
#' @param viewing_distance_cm Distance in cm.
#' @param screen_width_cm Width in cm.
#' @param screen_height_cm Height in cm.
#' @param screen_width_pixels Width in pixels.
#' @param screen_height_pixels Height in pixels.
#' @export
tracker_properties_set <- function(sample_frequency = NULL,
                                   viewing_distance_cm = NULL,
                                   screen_width_cm = NULL,
                                   screen_height_cm = NULL,
                                   screen_width_pixels = NULL,
                                   screen_height_pixels = NULL) {
  
  # 1. Capture all passed arguments into a list
  all_args <- mget(names(formals()), sys.frame(sys.nframe()))
  
  # 2. Filter out the NULL ones (the ones the user didn't change)
  new_args <- Filter(Negate(is.null), all_args)
  
  # 3. If they passed nothing, just return the list of possible parameters
  if (length(new_args) == 0) {
    message("Available parameters you can change:")
    return(names(the$eyetracker_properties))
  }
  
  # 4. Merge changes and save
  the$eyetracker_properties <- utils::modifyList(the$eyetracker_properties, new_args)
  
  invisible(the$eyetracker_properties)
}