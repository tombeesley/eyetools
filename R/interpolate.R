#' Interpolation of missing data (NAs)
#'
#' Extends the zoo::na.approx and zoo::na.spline functions to include a report which provides
#' the proportion of missing data before and after the interpolation process. This is handy
#' for evaluating the effectiveness of the repair.
#'
#' @param data dataframe with columns time, x, y, trial (the standardised raw data form for eyeproc)
#' @param maxgap maximum number of consecutive NAs to fill. Any longer gaps will be left unchanged (see zoo package)
#' @param method "approx" for linear interpolation or "spline" for cubic spline interpolation
#' @param report default is FALSE. If TRUE, then the return value is a list containing the returned data frame and the report.
#'
#' @return a dataframe of the same shape of the input data
#' @export
#'
#' @examples interpolate(example_raw_fix, maxgap = 20)
#' @examples interpolate(example_raw_fix, method = "approx", maxgap = 50, report = TRUE)
#'
#' @importFrom zoo na.approx
#' @importFrom zoo na.spline
#' @importFrom rlang .data
#'
interpolate <- function(data, maxgap = 25, method = "approx", report = FALSE) {

  # PRE-INTERP summary of missing data
  if (report) {
    pre_missing <- mean((is.na(data$x) | is.na(data$y)))
  }

  # interpolation process
  if (method %in% c("approx", "spline")) {

    # Split the data by 'trial'
    data_split <- split(data, data$trial)

    # Function to apply na interpolation on both x and y columns
    interpolate_na <- function(df) {
      df$x <- get(paste0("na.", method))(df$x, maxgap = 25, na.rm = FALSE)
      df$y <- get(paste0("na.", method))(df$y, maxgap = 25, na.rm = FALSE)
      return(df)
    }

    # Apply the interpolation function to each trial's data
    data_split <- lapply(data_split, interpolate_na)

    # Recombine the split data back into a single dataframe
    data <- do.call(rbind, data_split)

  } else {
    stop("'method' not recognised. Use 'approx' or 'spline'")
  }

  # POST-INTERP summary of missing data
  if (report) {
    post_missing <- mean((is.na(data$x) | is.na(data$y)))
  }

  # return
  if (report) {
    report_return <- data.frame(missing_perc_before = pre_missing,
                            missing_perc_after = post_missing)
    return(list(data, report_return))
  } else {
    return(data)
  }
}
