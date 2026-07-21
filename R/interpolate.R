#' Interpolation of missing data (NAs)
#'
#' Extends the zoo::na.approx and zoo::na.spline functions to include a report which provides
#' the proportion of missing data before and after the interpolation process. This is handy
#' for evaluating the effectiveness of the repair.
#'
#' Analyses data separately for each unique combination of values in `pID` and `trial`.
#'
#' @param data A dataframe with raw data (pID, time, x, y, trial), the standardised raw data form for eyetools
#' @param maxgap maximum time gap of consecutive trackloss to fill (in ms). Any longer gaps will be left unchanged (see zoo package)
#' @param vel_threshold the maximum velocity that is tolerated between start and end points of periods of missing data
#' @param method "approx" for linear interpolation or "spline" for cubic spline interpolation
#' @param report default is FALSE. If TRUE, then the return value is a list containing the returned data frame and the report.
#' @param progress Display a progress bar
#'
#' @return a dataframe of the same shape of the input data
#' @export
#'
#' @examples
#' data <- combine_eyes(HCL)
#' interpolate(data, maxgap = 150)
#'
#' @importFrom zoo na.approx
#' @importFrom zoo na.spline
#' @importFrom rlang .data
#'
interpolate <- function(data, vel_threshold = 35, maxgap = 150, method = "approx", report = FALSE, progress = TRUE) {

  # check data format
  .check_monocular_data_format(data)
  
  data <- .make_NA_consistent(data, interpolation_running = TRUE)

  internal_interpolate <- function(data, maxgap, method, report) {

    # PRE-INTERP summary of missing data
    if (report) {
      pre_missing <- mean((is.na(data$x) | is.na(data$y)))
    }

    # interpolation process
    # estimate sample rate
    if (is.null(the$eyetracker_properties$sample_frequency)) .estimate_sample_rate(data)

    maxgap <- maxgap/(1000/the$eyetracker_properties$sample_frequency) #expressed in rows rather than time
    maxgap <- ceiling(maxgap) #round up to nearest integer


    if (method %in% c("approx", "spline")) {

      # Split the data by pID and trial
      data_split <- split(data, ~ pID + trial)

      # Function to apply na interpolation on both x and y columns
      interpolate_na <- function(df) {
        
        # create lag and lead values, which will be used for interpolation periods
        lead_lag <- function(v, n) { # https://stackoverflow.com/questions/56807120/lag-and-lead-in-base-r
          if (n > 0) c(rep(NA, n), head(v, length(v) - n))
          else c(tail(v, length(v) - abs(n)), rep(NA, abs(n)))
        }
        
        df[,c('prev_x', 'prev_y', 'next_x', 'next_y')] <- c(lead_lag(df$x,1), lead_lag(df$y,1), lead_lag(df$x,-1), lead_lag(df$y,-1))
        
        # add a column that identifies periods of NA
        df$is_na <- as.integer(is.na(df$x))
        
        if (method == "approx") {
          df$x_i <- na.approx(df$x, maxgap = maxgap, na.rm = FALSE)
          df$y_i <- na.approx(df$y, maxgap = maxgap, na.rm = FALSE)
        }
        if (method == "spline") {
          df$x_i <- na.spline(df$x, maxgap = maxgap, na.rm = FALSE) + 0*na.approx(df$x, maxgap = maxgap, na.rm = FALSE) #suppress extrapolation behaviour
          df$y_i  <- na.spline(df$y, maxgap = maxgap, na.rm = FALSE) + 0*na.approx(df$y, maxgap = maxgap, na.rm = FALSE)
        }
        
        # for each NA period calculate max velocity 
        # Create a grouping variable for consecutive TRUE runs
        df$run_id <- cumsum(!df$is_na)
        df$run_id[df$is_na==0] <- 0 # make all the non NA periods 0, to make the next split much quicker
        
        # Split the dataframe into a list of 
        na_periods <- split(df, ~run_id)
        
        # for each element of the list, if the is.na is 1, calculate the velocity
        # if velocity is below threshold, make x = x_i and y = y_i
        # put the df back together and remove unnecessary columns
        
        vel_check <- function(na_df) {
          
          #print(sum(na_df$is_na))
          if (sum(na_df$is_na) > 0 & !is.na(na_df[1,'x_i'])) {
            p1 <- c(na_df[1,'prev_x'], na_df[1,'prev_y']) # start point of interpolation
            p2 <- c(na_df[nrow(na_df),'next_x'], na_df[nrow(na_df),'next_y']) # end point of interpolation
            na_distance <- sqrt(sum((p1 - p2)^2)) # Euclidean pixel distance
            na_distance <- dist_to_visual_angle(na_distance, dist_type = "pixel") # visual angle of distance
            na_duration <- nrow(na_df)*(1000/the$eyetracker_properties$sample_frequency) # duration of na period in ms
            na_vel <- na_distance*(1000/na_duration) # degrees per second
            if ((na_vel<vel_threshold) == TRUE) { # TRUE if velocity of na period is below threshold
              na_df[,c('x', 'y')] <- na_df[,c('x_i', 'y_i')] 
            } 
          }
          
          na_df <- na_df[,c('pID', 'time', 'trial', 'x', 'y')] # return the 5 standard columns
          
          return(na_df)
          
        }
        
        df <- lapply(na_periods, vel_check)
        df <- do.call(rbind, df) # recombine
        df <- df[order(df$time),] # sort the data by time

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
  data <- split(data, data$pID)
  
  if(progress) {
    out <- pbapply::pblapply(data, internal_interpolate, maxgap, method, report)
  } else {
    out <- lapply(data, internal_interpolate, maxgap, method, report)
  }
  

  if (report) {

    report <- do.call(rbind, lapply(out, function(data, i) {

      data[[2]]

    }))

    report[["pID"]] <- rownames(report)
    rownames(report) <- NULL
    report <- report[,c("pID", "missing_perc_before", "missing_perc_after")]

    data <- do.call(rbind, lapply(out, function(data, i)
    { data[[1]] }
    )
    )

    data$id <- rownames(data)
    rownames(data) <- NULL

    out <- list(data, report)
    # out[[1]] <- .check_ppt_n_out(out[[1]])

  } else {
    out <- do.call("rbind.data.frame", out)
    rownames(out) <- NULL
    # out <- .check_ppt_n_out(out)
  }

  return(out)

}
