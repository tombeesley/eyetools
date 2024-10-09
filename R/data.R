#' Example eye data from a simple contingency learning task
#'
#' A dataset containing raw data samples for 72 trials of a simple learning experiment,
#' with several clearly defined AOIs on the screen, producing many distinct fixations each trial.
#' Data were collected from a 300 Hz Tobii eye-tracker.
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
#' in which saccades were made (left or right) from a central location on the screen each trial.
#' Data were collected from a 300 Hz Tobii eye-tracker.
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
#' four objects (coloured shapes), positioned at the corners of a square and participants spent 2 seconds encoding the stimuli.
#' Data were collected from a 120 Hz Tobii eye-tracker.
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

#' Example AOIs for use with example_raw_WM
#'
#' This dataframe contains 4 rectangular areas of interest (AOIs), set out for use with the example_raw_WM dataset.
#' Values are in pixels.
#'
#' @format A data frame with 4 rows and 4 variables:
#' \describe{
#'   \item{x}{centred x coordinate of the AOI}
#'   \item{y}{centred y coordinate of the AOI}
#'   \item{width_radius}{either the width of the AOI, or the radius for circular AOIs}
#'   \item{height}{the height of the AOI; should be NA for circular AOIs}
#'   ...
#' }
#'
"AOIs_WM"

#' Example dataset from a dataset that includes counterbalancing
#'
#' This dataframe contains 96 trials where the predictive cue is counterbalanced and presented either on the left or the right.
#'
#' @format A data frame with 80,577 rows and 10 variables:
#' \describe{
#'   \item{time}{timestamp of the sample}
#'   \item{x}{x coordinate of the eye}
#'   \item{y}{y coordinate of the eye}
#'   \item{trial}{trial number}
#'   \item{P_cue}{Are these necessary columns?}
#'   \item{NP_cue}{Are these necessary columns?}
#'   \item{cue_order}{whether the predictive cue os presented on the left (1) or the right (2)}
#'   \item{correct_out}{Are these necessary columns?}
#'   \item{accuracy}{accuracy of selecting the correct cue}
#'   \item{RT}{response time}
#'   ...
#' }
#'
"example_counterbalance"

#' Example dataset from that contains binocular eye data from three participants from a simple contingency learning task
#' (the data are from Beesley, Nguyen, Pearson, & Le Pelley, 2015). In this task there are two stimuli that appear simultaneously
#' on each trial (to the left and right of the screen).
#' Participants look at these cues and then make a decision by selecting an "outcome response" button.
#'
#' The dataset contains data from two participants and the first six trials of the study.
#'
#' @format A dataframe of 31,041 observations and seven variables
#' \describe{
#'   \item{pNum}{participant number}
#'   \item{time}{timestamp of the sample (milliseconds)}
#'   \item{left_x}{x coordinate of the left eye}
#'   \item{left_y}{y coordinate of the left eye}
#'   \item{right_x}{x coordinate of the right eye}
#'   \item{right_y}{y coordinate of the right eye}
#'   \item{trial}{trial number}
#'   ...
#' }
"HCL"

#' Example dataset of behavioural data to compliment dataset HCL. This contains information on stimuli
#' (such as the side the predictive cue was presented on) as well as response data, including accuracy and response times
#'
#' @format A dataframe of 12 observations and eight variables
#' \describe{
#'   \item{pNum}{participant number}
#'   \item{trial}{trial number}
#'   \item{P_cue}{Are these necessary columns?}
#'   \item{NP_cue}{Are these necessary columns?}
#'   \item{cue_order}{whether the predictive cue os presented on the left (1) or the right (2)}
#'   \item{correct_out}{NAre these necessary columns?}
#'   \item{accuracy}{response accuracy}
#'   \item{RT}{response time in milliseconds}
#'   ...
#' }
"HCL_behavioural"
