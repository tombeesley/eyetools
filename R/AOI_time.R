#' Area of interest analysis at the trial level
#'
#' Analyses time on AOI regions
#'
#' @param data A dataframe with fixation data (from fix_dispersion)
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#'

AOI_entries <- function(data, AOIs) {

  # dataframe to hold AOI entry results
  AOI_total <- data.frame(matrix(nrow = nrow(AOIs), ncol = 3))

  for (a in 1:nrow(AOIs)) {

    if (sum(!is.na(AOIs[a,])) == 4) {
      # square AOI
      xy_hits <- (between(data$x, AOI[a,1]-AOI[a,3]/2, AOI[a,1]+AOI[a,3]/2) &
                    between(data$y, AOI[a,2]-AOI[a,4]/2, AOI[a,2]+AOI[a,4]/2))
    } else if (sum(!is.na(AOIs[a,])) == 3) {
      # circle AOI
      xy_hits <- sqrt((AOI[a,1]-data$x)^2+(AOI[a,2]-data$y)^2) < AOI[a,3]/2
    } else {
      # report error message of bad AOI definition

    }

    # convert hits into data on time and entries

    # apply this function over trials, as that variable exists within the input data anyway

  }

}
