#' Interpolation of missing data (NAs)
#'
#' Extends the zoo::na.approx and zoo::na.spline functions to include a report which provides
#' the proportion of missing data before and after the interpolation process. This is handy
#' for evaluating the effectiveness of the repair.
#'
#' It can take either single participant data or multiple participants where there is a variable for unique participant identification.
#' The function looks for an identifier named `participant_ID` by default and will treat this as multiple-participant data as default,
#' if not it is handled as single participant data, or the participant_ID needs to be specified
#'
#' @param data dataframe with columns time, x, y, trial (the standardised raw data form for eyeproc)
#' @param maxgap maximum time gap of consecutive trackloss to fill (in ms). Any longer gaps will be left unchanged (see zoo package)
#' @param sample_rate Optional sample rate of the eye-tracker (Hz) for use with data. If not supplied, the sample rate will be estimated from the time column and the number of samples.
#' @param method "approx" for linear interpolation or "spline" for cubic spline interpolation
#' @param report default is FALSE. If TRUE, then the return value is a list containing the returned data frame and the report.
#' @param participant_ID the variable that determines the participant identifier. If no column present, assumes a single participant
#'
#' @return a dataframe of the same shape of the input data
#' @export
#'
#' @examples
#' data <- combine_eyes(HCL)
#' interpolate(data, maxgap = 8000, participant_ID = "pNum")
#'
#' @importFrom zoo na.approx
#' @importFrom zoo na.spline
#' @importFrom rlang .data
#'
interpolate <- function(data, maxgap = 8000, method = "approx", sample_rate = NULL, report = FALSE, participant_ID = "participant_ID") {

  if(is.null(data$x) || is.null(data$y)) {
    stop("Columns 'x' or 'y' not found.")
  }

  #first check for multiple/single ppt data
  test <- .check_ppt_n_in(participant_ID, data)
  participant_ID <- test[[1]]
  data <- test[[2]]

  internal_interpolate <- function(data, maxgap, method, sample_rate, report) {


    # PRE-INTERP summary of missing data
    if (report) {
      pre_missing <- mean((is.na(data$x) | is.na(data$y)))
    }

    # interpolation process
    # estimate sample rate
    if (is.null(sample_rate)==TRUE){
      trial <- split(data, data$trial)
      sample_rates <- sapply(trial, function(data) {

        # estimate sample rate (ms) from difference between timestamps
        time <- data$time - data$time[1] # start trial timestamps at 0
        sample_rate <- mean(diff(time)) #difference between timestamps, expressing ms per sample
        sample_rate

      })
    } else {
      sample_rate <- 1000/sample_rate # express in ms per sample
      sample_rate
    }

    #average sample rate across all trials
    sample_rate <- mean(sample_rates)

    maxgap <- maxgap/sample_rate #expressed in rows rather than time
    maxgap <- ceiling(maxgap) #round up to nearest integer


    if (method %in% c("approx", "spline")) {

      # Split the data by 'trial'
      data_split <- split(data, data$trial)

      # Function to apply na interpolation on both x and y columns
      interpolate_na <- function(df) {
        if (method == "approx") {
          df$x <- na.approx(df$x, maxgap = maxgap, na.rm = FALSE)
          df$y <- na.approx(df$y, maxgap = maxgap, na.rm = FALSE)
        }
        if (method == "spline") {
          df$x <- na.spline(df$x, maxgap = maxgap, na.rm = FALSE) + 0*na.approx(df$x, maxgap = maxgap, na.rm = FALSE) #suppress extrapolation behaviour
          df$y <- na.spline(df$y, maxgap = maxgap, na.rm = FALSE) + 0*na.approx(df$y, maxgap = maxgap, na.rm = FALSE)
        }
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
  data <- split(data, data[[participant_ID]])
  out <- lapply(data, internal_interpolate, maxgap, method, sample_rate, report)

  if (report) {

    report <- do.call(rbind, lapply(out, function(data, i) {

      data[[2]]

    }))

    report[[participant_ID]] <- rownames(report)
    rownames(report) <- NULL
    report <- report[,c(participant_ID, "missing_perc_before", "missing_perc_after")]

    data <- do.call(rbind, lapply(out, function(data, i)
    { data[[1]] }
    )
    )

    data$id <- rownames(data)
    rownames(data) <- NULL

    out <- list(data, report)
    out[[1]] <- .check_ppt_n_out(out[[1]])

  } else {
    out <- do.call("rbind.data.frame", out)
    rownames(out) <- NULL
    out <- .check_ppt_n_out(out)
  }

  return(out)

}
