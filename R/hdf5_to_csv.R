#' hdf5_to_csv
#'
#' A function to convert TOBII-generated hdf5 files to csv
#'
#' @param filename the hdf5 file generated from TOBII
#'
#' @return A list of csv files collected from the eyetracker content, if only one eyetracking event is present, return this as a csv file
#' @export
#'
#' @import hdf5r
#'
#' @examples
#' \dontrun{
#' raw_data <- hdf5_to_csv("example_TOBII.hdf5")
#' }

hdf5_to_csv <- function(filename) {

  file.h5 <- H5File$new(filename, mode="a") # 'a' mode is open

  #get the tracking data
  eyetracking_data <- file.h5[['data_collection']][['events']][['eyetracker']]

  data_out <- list()

  for (name in names(eyetracking_data)) {

    temp <- eyetracking_data[[name]]

    if (temp$dims != 0) {
      temp <- temp[1:temp$dims]
    } else {
      temp <- NULL
    }

    data_out[[name]] <- temp

  }

  if (length(data_out) == 0) {
    error_call("No data detected")
  }

  #return just the dataframe if only one event is detected, otherwise return as a list
  if (length(data_out) == 1) {
    data_out <- data_out[[1]]
  }

  return(data_out)
}
