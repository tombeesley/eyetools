# Plot raw data and fixations

A tool for visualising raw eye-data, processed fixations, and saccades.
Can use all three data types together and independently. Fixations can
be labeled in the order they were made. Can overlay areas of interest
(AOIs) and customise the resolution.

## Usage

``` r
plot_spatial(
  raw_data = NULL,
  fix_data = NULL,
  sac_data = NULL,
  AOIs = NULL,
  pID_values = NULL,
  trial_values = NULL,
  bg_image = NULL,
  flip_y = FALSE,
  show_fix_order = TRUE,
  plot_header = FALSE
)
```

## Arguments

- raw_data:

  data in standard raw data form (time, x, y, trial)

- fix_data:

  data output from fixation function

- sac_data:

  data output from saccade function

- AOIs:

  A dataframe of areas of interest (AOIs), with one row per AOI (name,
  x, y, width_radius, height). If using circular AOIs, then the column
  width_radius is used for the radius and the height should be set to
  NA.

- pID_values:

  specify particular values within 'pID' to plot data from certain
  participants

- trial_values:

  specify particular values within 'trial' to plot data from certain
  trials

- bg_image:

  The filepath of a PNG image to be added to the plot, for example to
  show a screenshot of the task.

- flip_y:

  reverse the y axis coordinates (useful if origin is top of the screen)

- show_fix_order:

  label the fixations in the order they were made

- plot_header:

  display the header title text which explains graphical features of the
  plot.

## Value

a plot of the raw data

## Examples

``` r
# \donttest{
data <- combine_eyes(HCL)
# plot the raw data
plot_spatial(raw_data = data, pID_values = 118)


# plot both raw and fixation data together
plot_spatial(raw_data = data, fix_data = fixation_dispersion(data), pID_values = 118)


#plot one trial
plot_spatial(raw_data = data, fix_data = fixation_dispersion(data), trial_values = 6)


# }
```
