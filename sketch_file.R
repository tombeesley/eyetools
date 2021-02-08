library(tidyverse)
library(devtools)
library(profvis)
library(microbenchmark)

# fixing up some example data from the saccades experiments

# d <- read_csv("inst/extdata/stim_P101_Exp6.csv", col_names = FALSE)
# d <- read_csv("inst/extdata/dec_C1_P104_Exp6.csv", col_names = FALSE)
# d <- read_csv("data/example_raw_d1.csv", col_names = TRUE)
#
# colnames(d) <- c("time", "x", "y", "trial")
#
# example_raw_psy <- example_raw_psy %>%
#   mutate(across(c(x,y), ~na_if(.,"NaN"))) %>%
#   filter(trial <= 100)
#
# usethis::use_data(example_raw_psy, overwrite = TRUE)


times <- microbenchmark(fix_dispersion(t1_raw), fix_dispersion_SLOW(t1_raw), times = 5)

profvis ({
  a <- eyetools::fix_dispersion(example_raw_psy)
})

t1_raw <- filter(example_raw_psy, trial %in% 1:20)
#t1_raw <- t1_raw[1:200,]
t1_fix <- eyetools::fix_dispersion(t1_raw)



AOIs <- data.frame(matrix(nrow = 3, ncol = 4))
colnames(AOIs) <- c("x", "y", "width", "height")

AOIs[1,] <- c(960, 600, 300, 300) # X, Y, W, H - square
AOIs[2,] <- c(260, 600, 300, 300) # X, Y, W, H - square
AOIs[3,] <- c(1660, 600, 300, 300) # X, Y, W, H - square
#AOIs[3,] <- c(960, 560, 700, NA) # X, Y, D - circle

# plot AOIs
ggplot() + geom_tile(data = AOIs, aes(x = x, y = y, width = width, height = height))


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

