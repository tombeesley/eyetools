#' Example dataset from that contains binocular eye data from two participants from a simple contingency learning task
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

#' Example dataset of behavioural data to complement dataset HCL. This contains information on stimuli
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

#' Example AOIs for use with HCL
#'
#' This dataframe contains three rectangular areas of interest (AOIs), set out for use with the HCL dataset.
#' Values are in pixels.
#'
#' @format A data frame with 3 rows and 4 variables:
#' \describe{
#'   \item{x}{centred x coordinate of the AOI}
#'   \item{y}{centred y coordinate of the AOI}
#'   \item{width_radius}{either the width of the AOI, or the radius for circular AOIs}
#'   \item{height}{the height of the AOI; should be NA for circular AOIs}
#'   ...
#' }
#'
"HCL_AOIs"
