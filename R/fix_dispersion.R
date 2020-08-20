#' Fixation detection by dispersion method
#'
#' Detects fixations according to the method proposed by S & G
#'
#'
#' @param data dataframe with columns time, x, y, trial (the standardised raw data form for eyeproc)
#' @param
#' @param
#' @param
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom magrittr %>%
#' @import dplyr
#' @import purrr
#' @importFrom rlang .data
#'
fix_dispersion <- function(data, min_dur = 150, disp_tol = 30, report = FALSE) {

  # might be best to turn the data into a list by trial and
  # process it by applying another function over it





}

trial_level_process <- function(data) {

  vals <- slice_sample(100)

  return(vals)


}


# OLD MATLAB METHOD FOR DISPERSION FIXATIONS

# curFirst = 1; curLast = ceil(DurThresh/Interval);
#
# newFix = false; fixCnt = 0;
# while curLast <= size(EGdata,1)
#
#   Win = EGdata(curFirst:curLast,:);
#
#   dsp = [abs(max(Win(:,1)) - min(Win(:,1))) abs(max(Win(:,2)) - min(Win(:,2)))];
#
#   if dsp <= DispThresh
#     % increase window
#     curLast = curLast + 1;
#     newFix = true;
#   else
#     if newFix == true
#       % record as end of fixation
#       fixCnt = fixCnt + 1;
#       fixStore(fixCnt,1:2) = mean(Win(:,1:2),1); % X&Y
#       fixStore(fixCnt,3) = size(Win,1)*Interval; % duration
#       fixStore(fixCnt,4:5) = [Win(1,4) Win(end,4)]; % start/end timestamps
#       curFirst = curLast + 1;
#       curLast = curFirst + ceil(DurThresh/Interval);
#       newFix = false;
#     else
#       % move window to right and start again
#       curFirst = curFirst + 1;
#       curLast = curLast + 1;
#     end
#   end
# end
