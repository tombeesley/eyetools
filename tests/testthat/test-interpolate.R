temp <- data.frame(time = c(0,1,2,3), x = c(1,2,NA, 4), y = c(1,2,NA,4), trial = c(1,1,1,1))


test_that("no NA present", {
  expect_equal(sum(is.na(interpolate(temp))),
               0)
})


