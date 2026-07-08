#' Get Current Eyetracker Properties
#'
#' Returns a list containing the current global eyetracker settings.
#'
#' @return A named list of the current tracker parameters.
#' @export
tracker_properties_get <- function() {
  the$eyetracker_properties
}