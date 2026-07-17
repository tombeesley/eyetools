#' Create a blank data frame for populating with AOIs
#' 
#' Used to define a set of AOIs for use across various functions within the eyetools package. 
#'
#' @param num_AOIs number of AOIs, setting the number of rows
#' @param AOI_data a list containing data for each AOI, ordered by x, y, width_radius, and height (NA if circular)
#' @param AOI_names a vector of names for the AOIs specified in AOI_data
#'
#' @return an AOI dataframe in the format required for several AOI functions in eyetools
#' @export
#'
#' @examples
#' # create an empty data frame with 3 AOIs
#' create_AOI_df(3)
#'
#' # create an AOI dataframe with data, the second of which is circular, with names
#' create_AOI_df(num_AOIs = 3, 
#'               AOI_data = list(c(460,840,400,300), c(1460,840,400,NA), c(960,270,300,500)),
#'               AOI_names = c("AOI_1", "AOI_2", "AOI_3"))

create_AOI_df <- function(num_AOIs = 3, AOI_data = NULL, AOI_names = NULL) {

  return_AOIs <- data.frame(matrix(nrow = num_AOIs, ncol = 5))

  if(!is_empty(AOI_data)) {
    
    if (length(AOI_data) != num_AOIs) {
      stop("The number of AOIs specified in num_AOIs is different to the number of elements in AOI_data")
    }
    
    AOI_data <- unlist(AOI_data)
    AOI_data <- matrix(AOI_data, ncol = 4, byrow = TRUE)
    return_AOIs[,2:5] <- data.frame(AOI_data)

  }
  
  if(!is_empty(AOI_names)) {
    
    if (length(AOI_names) != num_AOIs) {
      stop("The number of AOIs specified in num_AOIs is different to the number of elements in AOI_names")
    }
    
    return_AOIs[,1] <- AOI_names
    
  } else {
    return_AOIs[,1] <- sprintf("AOI_%s",1:num_AOIs)
    
  }
  
  colnames(return_AOIs) <- c("name", "x", "y", "width_radius", "height")

  return(return_AOIs)

}
