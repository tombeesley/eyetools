#' Example eye data from a simple contingency learning task
#'
#' A dataset containing raw data samples for 72 trials of a simple learning experiment,
#' with several clearly defined AOIs on the screen, producing many distinct fixations each trial. Data were collected from a 300 Hz Tobii eye-tracker.
#' This is an example of the format raw data needs to be in in order to work with the functions
#' in eyetools.
#'
#' @format A data frame with 77,669 rows and 4 variables:
#' \describe{
#'   \item{time}{timestamp of the sample}
#'   \item{x}{x coordinate of the eye}
#'   \item{y}{y coordinate of the eye}
#'   \item{trial}{trial number}
#'   ...
#' }
#'
"example_raw_fix"

#' Example eye data visual search
#'
#' A dataset containing raw data samples for 100 trials of a simple visual search task,
#' in which saccades were made (left or right) from a central location on the screen each trial. Data were collected from a 300 Hz Tobii eye-tracker.
#' This is an example of the format raw data needs to be in in order to work with the functions
#' in eyetools.
#'
#' @format A data frame with 32608 rows and 4 variables:
#' \describe{
#'   \item{time}{timestamp of the sample}
#'   \item{x}{x coordinate of the eye}
#'   \item{y}{y coordinate of the eye}
#'   \item{trial}{trial number}
#'   ...
#' }
#'
"example_raw_sac"

#' Example binocular eye data
#'
#' A dataset containing raw data samples for 10 trials. The data are from both eyes,
#' and therefore this dataset is to be used as an example that can be used for the "combine_eyes" function.
#'
#' @format A data frame with X rows and 4 variables:
#' \describe{
#'   \item{time}{timestamp of the sample}
#'   \item{left_x}{x coordinate of the left eye}
#'   \item{left_y}{y coordinate of the left eye}
#'   \item{right_x}{x coordinate of the right eye}
#'   \item{right_y}{y coordinate of the right eye}
#'   \item{trial}{trial number}
#'   ...
#' }
#'
"example_raw_binocular"

#' Example eye data from working memory task
#'
#' A dataset containing raw data samples for 100 trials of a working memory task. The screen contained
#' four objects (coloured shapes), positioned at the corners of a square and participants spent 2 seconds encoding the stimuli.  Data were collected from a 120 Hz Tobii eye-tracker.
#' This is an example of the format raw data needs to be in in order to work with the functions
#' in eyetools.
#'
#' @format A data frame with 24205 rows and 4 variables:
#' \describe{
#'   \item{time}{timestamp of the sample}
#'   \item{x}{x coordinate of the eye}
#'   \item{y}{y coordinate of the eye}
#'   \item{trial}{trial number}
#'   ...
#' }
#'
"example_raw_WM"
