#' Get messgaes stored in TOBII-generated HDF5 files
#'
#' A function to get the message event files from a TOBII-generated hdf5 file to dataframe.
#' Used when a Psychopy experiment uses the io.sendMessageEvent() to record events
#'
#' @param filename the hdf5 file generated from TOBII
#'
#' @return A dataframe of message events as recorded by TOBII eye trackers
#' @export
#'
#' @import hdf5r
#'
#' @examples
#' \dontrun{
#' raw_data <- hdf5_get_event("example_TOBII.hdf5")
#' }

hdf5_get_event <- function(filename) {

  file.h5 <- H5File$new(filename, mode="a") # 'a' mode is open

  #get the tracking data
  eyetracking_data <- file.h5[['data_collection']][['events']][['experiment']][['MessageEvent']]

  eyetracking_data <- eyetracking_data[1:eyetracking_data$dims]

  eyetracking_data$filter_id <- NULL
  eyetracking_data$confidence_interval <- NULL
  eyetracking_data$experiment_id <- NULL
  eyetracking_data$session_id <- NULL
  eyetracking_data$device_id <- NULL
  eyetracking_data$type <- NULL
  eyetracking_data$msg_offset <- NULL

  return(eyetracking_data)
}


