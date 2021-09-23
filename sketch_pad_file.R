library(tidyverse)
library(devtools)
library(profvis)
library(microbenchmark)

# What is this mess?
# This script is where I stick bits of code or notes I need to keep when testing out the package
# It isn't critical to the package in any way and can be ignored!!!

# Note to do 10/02/2021

# Need to add some data that contains example AOIs
# Need to fix spatial plot to allow for circular AOIs
# AOI detection needs to report if bad AOI definition used

# used for testing the speed of fixation functions
times <- microbenchmark(fix_dispersion(t1_raw), fix_dispersion_SLOW(t1_raw), times = 5)

profvis ({
  a <- eyetools::fix_dispersion(example_raw_psy)
})

# d <-
#   read_csv("EAS01_P106_EG.csv",
#            col_names = c("time", "x", "y", "trial"),
#            cols(.default = col_double())) %>%
#   mutate(across(c(x,y), ~ifelse(is.nan(.), NA, .))) # convert NaN to NA


# get raw data
t_raw <- filter(example_raw_sac, trial %in% c(11:15))

# # process fixations
t_fix <- eyetools::fix_dispersion(t_raw)

t_interpolate <- interpolate(t_raw)

t_smoothed <- smoother(t_interpolate)

t_sac_new <- VTI_saccade(t_smoothed, sample_rate = NULL, threshold = 100)

# flatten trial list

t <- do.call(rbind.data.frame,t_sac_new)



# # process saccades (old version - from "saccades" package)
# t1_sac <- detect_saccades(t1_raw, lambda = 15)
#
# # number the events
# t1_sac <-
#   cbind(t1_sac,
#         event_n = c(1,cumsum(abs(diff(t1_sac$saccade)))+1))
#
# # plot a subset
# t1_g <-
#   t1_sac %>%
#   filter(between(event_n, 1, 10)) %>%
#   mutate(event_n = as.factor(event_n))
#
# t1_g %>%
#   ggplot(aes(x = x, y = y)) +
#   geom_point(aes(colour = event_n,
#                  shape = saccade),
#              size = 6) +
#   scale_color_discrete()
#
# # how many samples do saccades typically have
# t1_sac %>%
#   filter(saccade == TRUE) %>%
#   group_by(event_n) %>%
#   summarise(n_samples = n()) %>%
#   ggplot(aes(n_samples)) +
#   geom_histogram()
#
#
# # summarise saccade data
# rl <- rle(t1_sac$saccade)
# ends <- cumsum(rl$lengths)
# starts <- c(1,ends[1:length(ends)-1]+1)
#
# data.frame(saccade = rl$values,
#            ts_start = starts,
#            ts_end = ends)

# temp VTI function





# AOI analysis testing

AOIs <- data.frame(matrix(nrow = 3, ncol = 4))
colnames(AOIs) <- c("x", "y", "width", "height")

AOIs[1,] <- c(960, 600, 300, 300) # X, Y, W, H - square
AOIs[2,] <- c(260, 600, 300, 300) # X, Y, W, H - square
AOIs[3,] <- c(1660, 600, 300, 300) # X, Y, W, H - square
#AOIs[3,] <- c(960, 560, 700, NA) # X, Y, D - circle

# testing the detection in AOIs
# square AOI detect
xy_hits <- (between(t1_fix$x, AOIs[1,1]-AOIs[1,3]/2, AOIs[1,1]+AOIs[1,3]/2) &
              between(t1_fix$y, AOIs[1,2]-AOIs[1,4]/2, AOIs[1,2]+AOIs[1,4]/2))

# circle AOI detect
sqrt((AOIs[3,1]-t1_fix$x)^2+(AOIs[3,2]-t1_fix$y)^2) < AOIs[3,3]/2

spatial_plot(raw_data = t1_raw,
             fix_data = t1_fix,
             aoi = AOIs,
             res = c(0,1920,0,1080),
             show_fix_order = TRUE,
             flip_y = TRUE)


