# Convert TOBII-generated HDF5 files to dataframe

A function to convert TOBII-generated hdf5 files to a dataframe

## Usage

``` r
hdf5_to_df(filename)
```

## Arguments

- filename:

  the hdf5 file generated from TOBII

## Value

A list of dataframes collected from the eyetracker content, if only one
eyetracking event is present, return this as a single dataframe

## Examples

``` r
if (FALSE) { # \dontrun{
raw_data <- hdf5_to_df("example_TOBII.hdf5")
} # }
```
