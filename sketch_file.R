library(tidyverse)
library(devtools)

# fixing up some example data from the saccades experiments

d <- read_csv("inst/extdata/stim_P101_Exp6.csv", col_names = FALSE)
d <- read_csv("inst/extdata/dec_C1_P104_Exp6.csv", col_names = FALSE)
#d <- read_csv("data/example_raw_d1.csv", col_names = TRUE)

colnames(d) <- c("time", "x", "y", "trial")

example_raw_psy <- d %>%
  mutate(d, across(c(x,y), ~na_if(.,0))) %>%
  filter(trial <= 100)

usethis::use_data(example_raw_psy, overwrite = TRUE)



load_all()

check()





