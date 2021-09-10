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

# get raw data
t1_raw <- filter(example_raw_sac, trial == 2)

# process fixations
t1_fix <- eyetools::fix_dispersion(t1_raw)

# process saccades
t1_sac <- detect_saccades(t1_raw, lambda = 15)

# number the events
t1_sac <-
  cbind(t1_sac,
        event_n = c(1,cumsum(abs(diff(t1_sac$saccade)))+1))

# plot a subset
t1_g <-
  t1_sac %>%
  filter(between(event_n, 1, 10)) %>%
  mutate(event_n = as.factor(event_n))

t1_g %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = event_n,
                 shape = saccade),
             size = 6) +
  scale_color_discrete()

# how many samples do saccades typically have
t1_sac %>%
  filter(saccade == TRUE) %>%
  group_by(event_n) %>%
  summarise(n_samples = n()) %>%
  ggplot(aes(n_samples)) +
  geom_histogram()


# summarise saccade data
rl <- rle(t1_sac$saccade)
ends <- cumsum(rl$lengths)
starts <- c(1,ends[1:length(ends)-1]+1)



data.frame(saccade = rl$values,
           ts_start = starts,
           ts_end = ends)

# temp VTI function

VTI_saccade <- function(data){

  x <- data$x
  y <- data$y

  d <- as.matrix(dist(cbind(x,y)))

  d_diag <- diag(d[2:nrow(d),])

  data_new <- cbind(data,
                    distance = c(NA,d_diag))


}





dist_to_visual_angle <- function(vector, dist_type = "cm", view_dist_cm = 60, screen_width_cm = 51, screen_width_pixels = 1920) {

  if (dist_type == "pixel") {
    # works out pixels per cm (assumes width==height)
    pix_per_cm  <- screen_width_pixels/screen_width_cm

    # convert the input vector to cm units
    vector <- vector/pix_per_cm
  }

  rad <- 2*atan(vector/(2*view_dist_cm))
  ang = rad*(180/pi)
  return(ang)

}

t1_raw <- interpolate(t1_raw)

t1_sac_new <- VTI_saccade(t1_raw)

t1_sac_new$distance <- pixels_to_visual_angle(t1_sac_new$distance)

t1_sac_new$vel <- t1_sac_new$distance*300 # visual angle per second

t1_sac_new[2:nrow(t1_sac_new),] %>%
  ggplot(aes(x = time, y = smooth(vel))) +
  geom_point()




# AOI analysis

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


