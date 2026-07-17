# Set the properties of the eye-tracker that was used in recording the data

This function is used to specify the properties of the eye-tracker used
to record the data. Users can update one or more parameters. Unnamed
parameters retain previous values. If these are not set by the user,
then default parameters are set and a warning is issued in every call to
relevant functions

## Usage

``` r
tracker_properties_set(
  sample_frequency = NULL,
  viewing_distance_cm = NULL,
  screen_width_cm = NULL,
  screen_height_cm = NULL,
  screen_width_pixels = NULL,
  screen_height_pixels = NULL
)
```

## Arguments

- sample_frequency:

  Numeric frequency.

- viewing_distance_cm:

  Distance in cm.

- screen_width_cm:

  Width in cm.

- screen_height_cm:

  Height in cm.

- screen_width_pixels:

  Width in pixels.

- screen_height_pixels:

  Height in pixels.
