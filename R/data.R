#' Example eye data
#'
#' A dataset containing raw data samples for 72 trials of a simple learning experiment,
#' which several clearly defined AOIs producing several distinct fixations each trial.
#' This is the format raw data needs to be in in order to work with the functions
#' within the "Processing" step of the workflow.
#'
#' @format A data frame with 77,669 rows and 4 variables:
#' \describe{
#'   \item{time}{timestamp of the sample}
#'   \item{x}{x coordinate of the eye position}
#'   \item{y}{y coordinate of the eye position}
#'   \item{trial}{trial number}
#'   ...
#' }
#'
"example_raw_psy"

#' Example eye data
#'
#' A dataset containing raw data samples for 100 trials of a simple visual search task,
#' in which saccades were made (left or right) from a central location on the screen each trial.
#' This is the format raw data needs to be in in order to work with the functions
#' within the "Processing" step of the workflow.
#'
#' @format A data frame with 32608 rows and 4 variables:
#' \describe{
#'   \item{time}{timestamp of the sample}
#'   \item{x}{x coordinate of the eye position}
#'   \item{y}{y coordinate of the eye position}
#'   \item{trial}{trial number}
#'   ...
#' }
#'
"example_raw_sac"
