library(tidyverse)
library(devtools)

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

check()

interpRes <- interpolate(example_raw_2, method = "approx", maxgap = 100, report = TRUE)
interpolate(example_raw_3, method = "approx", maxgap = 25)
interpolate(example_raw_3, method = "spline", maxgap = 5)
interpolate(example_raw_3, method = "blah")

example_raw_1 <- mutate(example_raw_1, across(c(x,y), na.approx, maxgap = 50, na.rm = TRUE))


interpRes[2]

tidyr
