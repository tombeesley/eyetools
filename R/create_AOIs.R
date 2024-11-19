#' Create a blank data frame for populating with AOIs
#'
#'
#' @param num_AOIs number of AOIs, setting the number of rows
#'
#' @return a dataframe in the standard format required for eyetools
#' @export
#'
#' @examples
#' # create a data frame with 4 AOIs
#' create_AOIs(4)
#'

create_AOIs <- function(num_AOIs) {

  return_AOIs <- data.frame(matrix(nrow = num_AOIs, ncol = 4))
  colnames(return_AOIs) <- c("x", "y", "width_radius", "height")
  return(return_AOIs)

}
