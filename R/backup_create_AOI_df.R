#' Create a blank data frame for populating with AOIs
#'
#'
#' @param num_AOIs number of AOIs, setting the number of rows
#' @param shape whether the AOI is rectangular ("rect") or circular ("circ")
#' @param AOI_data a list of data for each AOI, ordered by x, y, width_radius, and height
#'
#' @return a dataframe in the standard format required for eyetools
#' @export
#'
#' @examples
#' # create an empty data frame with 3 rectangular shaped AOIs
#' create_AOI_df(3, shape = "rect")
#'
#' # create an AOI dataframe with data
#' create_AOI_df(3, shape = "rect",
#'               AOI_data = list(c(460,840,400,300), c(1460,840,400,300), c(960,270,300,500)))
#' # creating data for circular AOIs
#' create_AOI_df(3, shape = "circ",
#'               AOI_data = list(c(460,840,400), c(1460,840,400), c(960,270,300)))

create_AOI_df <- function(num_AOIs = 3, shape = "rect", AOI_data = NULL) {

  if (shape == "rect") {
    return_AOIs <- data.frame(matrix(nrow = num_AOIs, ncol = 4))
  }
  else if (shape == "circ") {
    return_AOIs <- data.frame(matrix(nrow = num_AOIs, ncol = 3))
  }
  else {
    stop("invalid shape name. Use either 'rect' or 'circ'")
  }

  if(!is_empty(AOI_data)) {

    AOI_data <- unlist(AOI_data)
    AOI_data <- matrix(AOI_data, ncol = ncol(return_AOIs), byrow = TRUE)
    return_AOIs <- data.frame(AOI_data)

  }

  if (shape == "rect") colnames(return_AOIs) <- c("x", "y", "width_radius", "height")
  if (shape == "circ") colnames(return_AOIs) <- c("x", "y", "width_radius")

  return(return_AOIs)

}
