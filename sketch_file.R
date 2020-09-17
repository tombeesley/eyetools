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

t1_raw <- filter(example_raw_psy, trial == 5)
#t1_raw <- t1_raw[1:200,]
t1_fix <- eyetools::fix_dispersion(example_raw_psy)

spatial_plot(raw_data = t1_raw,
             fix_data = t1_fix,
             res = c(0,1920,0,1080),
             show_fix_order = TRUE,
             flip_y = TRUE)


