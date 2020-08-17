# library(tidyverse)
# library(devtools)

# # fixing up some example data from the saccades experiments

d <- read_csv("inst/extdata/stim_P103.csv", col_names = FALSE)
d <- read_csv("data/example_raw_d1.csv", col_names = TRUE)

colnames(d) <- c("time", "x", "y", "trial")

example_raw_3 <- d %>%
  mutate(d, across(c(x,y), ~na_if(.,0))) %>%
  filter(trial <= 100)

usethis::use_data(example_raw_1,example_raw_2,example_raw_3)

# running the function
load_all()

eye_repair(d1)
