#' Interpolation of missing data (NAs)
#'
#' Extends the zoo::na.approx and zoo::na.spline functions to include a report which provides
#' the proportion of missing data before and after the interpolation process. This is handy
#' for evaluating the effectiveness of the different methods and parameters.
#'
#' @param data dataframe with columns time, x, y, trial (the standardised raw data form for eyeproc)
#' @param maxgap maximum number of consecutive NAs to fill. Any longer gaps will be left unchanged (see zoo package)
#' @param method use 'na.approx' (linear interpolation) or 'na.spline' (cubic spline interpolation)
#' @param report default is FALSE. If TRUE, then the return value is a list containing the returned data frame and the report.
#'
#' @return
#' @export
#'
#' @examples interpolate(example_raw_2, method = "approx", maxgap = 50, report = TRUE)
#'
interpolate <- function(data, maxgap = 25, method = "approx", report = FALSE) {

  # PRE-INTERP summary of missing data
  if (report) {
    pre_missing <- mean((is.na(data$x) | is.na(data$y)))
  }

  # interpolation process
  if (method == "approx") {
    data <- mutate(data, across(c(x,y), na.approx, maxgap = maxgap, na.rm = FALSE))
  } else if (method == "spline") {
    data <- mutate(data, across(c(x,y), na.spline, maxgap = maxgap, na.rm = FALSE))
  } else {
    return("Error: 'method' not recognised. Use 'approx' or 'spline'")
  }

  # POST-INTERP summary of missing data
  if (report) {
    post_missing <- mean((is.na(data$x) | is.na(data$y)))
  }

  # return
  if (report) {
    report_return <- tibble(missing_perc_before = pre_missing,
                            missing_perc_after = post_missing)
    return(list(data, report_return))
  } else {
    return(data)
  }
}
