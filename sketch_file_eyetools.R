library(tidyverse)

d <- combine_eyes(HCL)

d <- smoother(interpolate(d))

p_d <- filter(d, pID == 118)

saccade_VTI(p_d)

load("dist_vel_test.RData")

plot(data$x)
plot(data$y)
