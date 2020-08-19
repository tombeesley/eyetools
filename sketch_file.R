library(tidyverse)
library(devtools)
library(zoo)

# # fixing up some example data from the saccades experiments
#
# d <- read_csv("inst/extdata/stim_P103.csv", col_names = FALSE)
# d <- read_csv("data/example_raw_d1.csv", col_names = TRUE)
#
# colnames(d) <- c("time", "x", "y", "trial")
#
# example_raw_3 <- d %>%
#   mutate(d, across(c(x,y), ~na_if(.,0))) %>%
#   filter(trial <= 100)
#
# usethis::use_data(example_raw_1,example_raw_2,example_raw_3)


load_all()

check()

interpolate(example_raw_2, method = "approx", maxgap = 40, report = TRUE)
interpolate(example_raw_1, method = "spline", maxgap = 40, report = TRUE)
interpolate(example_raw_3, method = "blah")

example_raw_1 <- mutate(example_raw_1, across(c(x,y), na.approx, maxgap = 50, na.rm = TRUE))



