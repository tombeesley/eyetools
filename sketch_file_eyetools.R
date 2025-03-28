library(tidyverse)

d <- combine_eyes(HCL)

d <- interpolate(d, participant_ID = "pNum")

d <- smoother(d, participant_ID = "pNum")

p_d <- d %>% filter(pNum == "118")

saccade_VTI(p_d)

load("dist_vel_test.RData")

plot(data$x)
plot(data$y)

x <- data$x
y <- data$y

data <- data %>% select(pID, trial, time, x, y)

data$time <- data$time - data$time[1] # start trial timestamps at 0

d <- as.matrix(dist(cbind(x,y)))

d_diag <- diag(d[2:nrow(d),])

data <- cbind(data,
              distance = c(NA,d_diag))

data$distance <- dist_to_visual_angle(data$distance, dist_type = "pixel") # convert to VisAng


####
# in 860 ms distance was 1111 pixels


packageurl <- "https://cran.r-project.org/src/contrib/Archive/eyetools/eyetools_0.7.2.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
li