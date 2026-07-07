#' Read Eyelink ASC Sample Data
#'
#' Reads gaze sample data from an EyeLink ASC file, parses binocular or
#' monocular recordings, segments samples into trials based on gaps in the
#' timestamp sequence, and returns a tidied data frame with time reset to
#' zero at the start of each trial.
#'
#' @param file Character string. Path to the EyeLink ASC (.asc) file to read.
#' @param recording Character string. Type of recording to parse, either
#'   \code{"binocular"} or \code{"monocular"}. Defaults to
#'   \code{c("binocular", "monocular")}, with \code{"binocular"} used if
#'   unspecified.
#' @param pID Participant identifier to attach to every row of the output.
#'   Required; the function will error if left as \code{NA}.
#' @param frequency Numeric. Sampling frequency in Hz of the recording
#'   (e.g. 500, 1000). Used to determine the inter-sample gap threshold for
#'   trial segmentation. The function will estimate the frequency as the median 
#'   difference between samples if left as \code{NA}.
#'
#' @return A data frame with one row per gaze sample, with columns:
#'   \describe{
#'     \item{pID}{Participant ID, as supplied.}
#'     \item{trial}{Integer trial number, incremented whenever the gap
#'       between consecutive samples exceeds twice the expected sampling
#'       interval.}
#'     \item{left_x, left_y, right_x, right_y}{Gaze coordinates for each eye
#'       (binocular recordings only).}
#'     \item{x, y}{Gaze coordinates (monocular recordings only).}
#'     \item{time}{Sample timestamp, reset to zero at the start of each
#'       trial.}
#'   }
#'
#' @details
#' Lines in the ASC file are treated as gaze samples if they begin with a
#' digit. Columns are split on whitespace, EyeLink's missing-value marker
#' (\code{"."}) is converted to \code{NA}, and all columns are coerced to
#' numeric. For binocular recordings the first 7 columns are expected
#' (time, left x/y/pupil, right x/y/pupil); for monocular recordings the
#' first 4 columns are expected (time, x, y, pupil).
#'
#' @examples
#' \dontrun{
#' samples <- read_eyelink_samples(
#'   file = "participant01.asc",
#'   recording = "binocular",
#'   pID = "P01",
#'   frequency = 1000
#' )
#' }
#'
#' @export


read_eyelink_samples <- function(file,
                                 recording = c("binocular", "monocular"),
                                 pID = NA,
                                 frequency = NA) {
  
  # Validate arguments
  recording <- match.arg(recording)
  
  if(is.na(pID)) stop("Please specify participant ID (pID)")
  
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  
  # Read ASC file as raw lines
  asc <- readLines(file, warn = FALSE)
  
  # Keep only gaze sample lines (timestamp at start)
  samples <- asc[grepl("^[0-9]", asc)]
  
  if (length(samples) == 0) {
    stop("No gaze samples found in file")
  }
  
  # Split columns (handles tabs and spaces)
  samples <- strsplit(samples, "[[:space:]]+")
  
  # Convert to data frame
  samples <- as.data.frame(
    do.call(rbind, samples),
    stringsAsFactors = FALSE
  )
  
  # Replace EyeLink missing values
  samples[samples == "."] <- NA

  
  detected <- if (ncol(samples) >= 7) "binocular" else if (ncol(samples) >= 4) "monocular" else NA
  
  if (is.na(detected)) {
    stop("Unrecognized sample format: found ", ncol(samples), " columns")
  }
  
  if (detected != recording) {
    stop(
      "Data appears to be ", detected,
      " but recording = \"", recording, "\" was specified."
    )
  }
  
  # Assign column names
  if (recording == "binocular") {
    
    samples <- samples[, 1:7]
    
    colnames(samples) <- c(
      "time",
      "left_x",
      "left_y",
      "left_pupil",
      "right_x",
      "right_y",
      "right_pupil"
    )
    
  } else {
    
    samples <- samples[, 1:4]
    
    colnames(samples) <- c(
      "time",
      "x",
      "y",
      "pupil"
    )
  }
  
  # Convert to numeric
  samples[] <- lapply(samples, as.numeric)
    
  if ("diff" %in% names(samples)) {
    d <- samples$diff
  } else {
    d <- c(diff(samples$time), NA)
  }
  
  if(is.na(frequency)) { 
    frequency <- 1000/median(diff(samples$time))
    warning(paste0("Frequency estimated as ", frequency, "Hz. Enter frequency as argument if inaccurate."))
  }
  
  gap <- 1000/frequency
  
  samples$trial <- 1 + cumsum(
    c(FALSE, head(d, -1) > 2*gap)
  )
  
  # Reset time to zero within each trial
  samples$time <- ave(
    samples$time,
    samples$trial,
    FUN = function(x) x - x[1]
  )

  
  rownames(samples) <- NULL
  
  samples$pID <- pID
  
  if (recording == "binocular") {
    sample_reordered <- data.frame(
      pID = samples$pID,
      trial = samples$trial,
      left_x = samples$left_x,
      left_y = samples$left_y,
      right_x = samples$right_x,
      right_y = samples$right_y,
      time = samples$time
    )}
  
  if (recording == "monocular") {
    sample_reordered <- data.frame(
      pID = samples$pID,
      trial = samples$trial,
      x = samples$x,
      y = samples$y,
      time = samples$time
    )}
  
  sample_reordered
}
