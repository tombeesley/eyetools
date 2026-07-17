# Read Eyelink ASC Sample Data

Reads gaze sample data from an EyeLink ASC file, parses binocular or
monocular recordings, segments samples into trials based on gaps in the
timestamp sequence, and returns a tidied data frame with time reset to
zero at the start of each trial.

## Usage

``` r
ASC_to_df(
  file,
  recording = c("binocular", "monocular"),
  pID = NA,
  frequency = NA
)
```

## Arguments

- file:

  Character string. Path to the EyeLink ASC (.asc) file to read.

- recording:

  Character string. Type of recording to parse, either `"binocular"` or
  `"monocular"`. Defaults to `c("binocular", "monocular")`, with
  `"binocular"` used if unspecified.

- pID:

  Participant identifier to attach to every row of the output. Required;
  the function will error if left as `NA`.

- frequency:

  Numeric. Sampling frequency in Hz of the recording (e.g. 500, 1000).
  Used to determine the inter-sample gap threshold for trial
  segmentation. The function will estimate the frequency as the median
  difference between samples if left as `NA`.

## Value

A data frame with one row per gaze sample, with columns:

- pID:

  Participant ID, as supplied.

- trial:

  Integer trial number, incremented whenever the gap between consecutive
  samples exceeds twice the expected sampling interval.

- left_x, left_y, right_x, right_y:

  Gaze coordinates for each eye (binocular recordings only).

- x, y:

  Gaze coordinates (monocular recordings only).

- time:

  Sample timestamp, reset to zero at the start of each trial.

## Details

Lines in the ASC file are treated as gaze samples if they begin with a
digit. Columns are split on whitespace, EyeLink's missing-value marker
(`"."`) is converted to `NA`, and all columns are coerced to numeric.
For binocular recordings the first 7 columns are expected (time, left
x/y/pupil, right x/y/pupil); for monocular recordings the first 4
columns are expected (time, x, y, pupil).

## Examples

``` r
if (FALSE) { # \dontrun{
samples <- read_eyelink_samples(
  file = "participant01.asc",
  recording = "binocular",
  pID = "P01",
  frequency = 1000
)
} # }
```
