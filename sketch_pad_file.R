library(tidyverse)
library(devtools)
library(profvis)
library(microbenchmark)
library(patchwork)
library(rdist)

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


# get raw data for just one trial
library(tidyverse)

example_mac_error

fix_dispersion(example_mac_error)

t_raw <- example_raw_wm
t_raw <- filter(example_raw_wm, between(trial, 1, 220))

# # process fixations
#fix_dispersion(t_raw, disp_tol = 100, min_dur = 150)

t_interpolate <- interpolate(t_raw)

t_smoothed <- smoother(t_interpolate)

t_sac <- VTI_saccade(t_smoothed, sample_rate = NULL, threshold = 150)

spatial_plot(raw_data = t_raw, fix_data = t_fix,sac_data = t_sac)

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

ggplot() +
  geom_line(aes(x = 1:30,
                y = seq(0.1,3,.1)),
                     size = 2,
                     linetype = "round") +
  coord_cartesian(ylim = c(0,30))

# trying to draw a screenshot under a spatial plot
d_raw <- example_raw_WM
d_raw <- d_raw[d_raw$trial==10,] # take just one trial
d_fix <- fix_dispersion(d_raw)
d_sac <- VTI_saccade(d_raw)
spatial_plot(raw_data = d_raw,
             fix_data = d_fix,
             sac_data = d_sac)

# add some AOI regions and screenshot

spatial_plot(raw_data = d_raw,
             fix_data = d_fix,
             sac_data = d_sac,
             AOIs = eyetools::AOIs_WM,
             bg_image = "inst/images/screenshot.jpg")


