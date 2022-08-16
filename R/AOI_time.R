#' Area of interest analysis at the trial level
#'
#' Analyses total time on defined AOI regions across trials. Currently only works with fixation data as the input.
#'
#' @param data A dataframe with fixation data (from fix_dispersion)
#' @param AOIs A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height).
#' @param AOI_names An optional vector of AOI names to replace the default "AOI_1", "AOI_2", etc.
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#'

AOI_time <- function(data, AOIs, AOI_names = NULL) {

  # dataframe to hold AOI entry results
  # columns are trial, AOI time * number of AOIs

  time_data <- sapply(split(data, data$trial), AOI_trial_process, AOIs = AOIs)

  time_data <- cbind(unique(data$trial), t(time_data))

  if (is.null(AOI_names)==FALSE) {
    AOI_name_text <- AOI_names
  }
  else {
    AOI_name_text <- sprintf("AOI_%s",1:nrow(AOIs))
  }

  colnames(time_data) <- c("trial", AOI_name_text)
  return(data.frame(time_data))

}


AOI_trial_process <- function(trial_data, AOIs) {

  aoi_time_sums <- data.frame(matrix(nrow = 1, ncol = nrow(AOIs)))

    for (a in 1:nrow(AOIs)) {

      if (sum(!is.na(AOIs[a,])) == 4) {
        # square AOI
        xy_hits <- (between(trial_data$x, AOIs[a,1]-AOIs[a,3]/2, AOIs[a,1]+AOIs[a,3]/2) &
                      between(trial_data$y, AOIs[a,2]-AOIs[a,4]/2, AOIs[a,2]+AOIs[a,4]/2))
      } else if (sum(!is.na(AOIs[a,])) == 3) {
        # circle AOI
        xy_hits <- sqrt((AOIs[a,1]-trial_data$x)^2+(AOIs[a,2]-trial_data$y)^2) < AOIs[a,3]
      } else {
        # report error message of bad AOI definition

      }

      # convert hits into data on time and entries
      aoi_time_sums[a] <- sum(xy_hits*trial_data$dur) # sum the valid AOI hits

    }

  return(aoi_time_sums)

}
