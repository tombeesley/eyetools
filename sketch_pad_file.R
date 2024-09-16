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




data <- rbind(example_raw_WM[example_raw_WM$trial %in% c(3,10),])

data_i <- interpolate(data)

data_i_s <- smoother(data_i)

t_sac <- VTI_saccade(data_i_s, threshold = 150)

t_fix <- fix_dispersion(data_i_s, disp_tol = 100, min_dur = 150)
t_fix_s <- fix_inverse_saccade(data_i_s, disp_tol = 100, min_dur = 150)

t_fix$TT <- 1

dP <- spatial_plot(raw_data = data_i,
                   fix_data = t_fix)

dV <- spatial_plot(raw_data = data_i,
                   fix_data = t_fix_s)

dP + dV

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
d_raw <- d_raw[d_raw$trial<=5,] # take a few trials
d_fix <- fix_dispersion(d_raw)

AOI_seq(data = d_fix, AOIs = AOIs_WM) # use fixation data
AOI_seq(data = d_raw, AOIs = AOIs_WM) # use raw data

AOI_time(fix_data = d_fix, AOIs = AOIs_WM) # use fix data
AOI_time(raw_data = d_raw, AOIs = AOIs_WM, sample_rate = 120) # use raw data
AOI_time(raw_data = d_raw, AOIs = AOIs_WM) # use raw data, no sample rate

d_sac <- VTI_saccade(d_raw)
spatial_plot(raw_data = d_raw,
             fix_data = d_fix,
             AOIs = AOIs_WM,
             bg_image = "WM_background.png")

d_raw$time

d_raw_N <- d_raw %>% filter(time == 61000)


spatial_plot(raw_data = d_raw_N,
             AOIs = AOIs_WM)

AOI_in <- eyetools::AOIs_WM
AOI_in[4,4] <- NA


# add AOI regions and screenshot
seq_plot(raw_data = d_raw,
         trial_number = 5,
         AOIs = AOI_in,
         bin_time = 100,
         bin_range = c(1,7))

ggplot() +
  geom_point(data = d_raw,
             colour = time,
             aes(x = x, y = y),
             size = 1,
             na.rm = TRUE)


AOI_time(fix_data = d_fix, AOIs = AOIs_WM)

