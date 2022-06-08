library(tidyverse)
library(devtools)
library(profvis)
library(microbenchmark)
library(patchwork)

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


# get raw data
t_raw <- filter(example_raw_sac, trial %in% c(2))

# # process fixations
t_fix <- fix_dispersion(t_raw,disp_tol = 150, min_dur = 100)

raw_plot <- spatial_plot(raw_data = t_raw, plot_header = TRUE)
fix_plot <- spatial_plot(raw_data = t_raw, fix_data = t_fix)

raw_plot/fix_plot

t_interpolate <- interpolate(t_raw)

t_smoothed <- smoother(t_interpolate)

a <- VTI_saccade(t_smoothed, sample_rate = NULL, threshold = 150)

# flatten trial list

t <- do.call(rbind.data.frame,t_sac_new)




# AOI analysis testing

AOIs <- data.frame(matrix(nrow = 3, ncol = 4))
colnames(AOIs) <- c("x", "y", "width", "height")

AOIs[1,] <- c(960, 540, 300, 300) # X, Y, W, H - square
AOIs[2,] <- c(200, 540, 300, 300) # X, Y, W, H - square
AOIs[3,] <- c(1720, 540, 300, 300) # X, Y, W, H - square
#AOIs[3,] <- c(960, 560, 700, NA) # X, Y, D - circle

# testing the detection in AOIs
# square AOI detect
xy_hits <- (between(t1_fix$x, AOIs[1,1]-AOIs[1,3]/2, AOIs[1,1]+AOIs[1,3]/2) &
              between(t1_fix$y, AOIs[1,2]-AOIs[1,4]/2, AOIs[1,2]+AOIs[1,4]/2))

# circle AOI detect
sqrt((AOIs[3,1]-t1_fix$x)^2+(AOIs[3,2]-t1_fix$y)^2) < AOIs[3,3]/2

spatial_plot(raw_data = NULL,
             fix_data = t_fix,
             #AOIs = AOIs,
             res = c(0,1920,0,1080),
             show_fix_order = FALSE,
             flip_y = TRUE)


AOI_time(t_fix, AOIs)

d <- readRDS("data/data_pilot.RDS")

example_two_eyes_raw <-
  d %>%
  filter(trial <=10)

usethis::use_data(example_two_eyes_raw, overwrite = TRUE)

d <- example_two_eyes_raw[585:605,]

example_two_eyes_raw
d <- combine_eyes(example_two_eyes_raw, method = "best_eye")


angle <- function(a,b){




}


