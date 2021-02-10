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

d <-
  read_csv("EAS01_P106_EG.csv",
           col_names = c("time", "x", "y", "trial"),
           cols(.default = col_double())) %>%
  mutate(across(c(x,y), ~ifelse(is.nan(.), NA, .))) # convert NaN to NA

d_test <- filter(d, trial == 158)

fix <- fix_dispersion(d) # process fixations


# t1_raw <- filter(example_raw_psy, trial %in% 1:20)
#t1_raw <- t1_raw[1:200,]
t1_fix <- eyetools::fix_dispersion(t1_raw)


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

