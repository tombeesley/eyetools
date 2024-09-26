fix_d <- fixation_dispersion(example_raw_WM)

# a first, basic (but CONFIRMATORY) test
testthat::expect_vector(
  {AOI_seq(fix_d, eyetools::AOIs_WM)},
  size = 427
)


