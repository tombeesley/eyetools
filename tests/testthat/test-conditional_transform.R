test_that(
  "conditional transform expects necessary columns", {
  expect_error(
    conditional_transform(data.frame(time = NA, x = NA, y = NA), cond_column = "cue_order", cond_values = 2),
    "missing column names"
  )}
)

temp <- data.frame(time = c(0,0), x = c(10,10), y = c(10,10), cue_order = c(1,2), trial = c(1,2))

test_that(
  "conditional transform returns an equal dataframe", {
  expect_equal(
    dim(conditional_transform(temp, flip = "x", cond_column = "cue_order", cond_values = 2)),
    dim(temp)
      )}
)

test_that(
  "conditional transform returns an equal dataframe", {
    expect_error(
      conditional_transform(temp, flip = "x", cond_column = "cue_order", cond_values = 2, resolution_x = 5),
      "screen resolution is smaller"
    )}
)

