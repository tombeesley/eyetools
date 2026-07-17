# Get messages stored in TOBII-generated HDF5 files

A function to get the message event files from a TOBII-generated hdf5
file to dataframe. Used when a Psychopy experiment uses the
io.sendMessageEvent() to record events

## Usage

``` r
hdf5_get_event(filename)
```

## Arguments

- filename:

  the hdf5 file generated from TOBII

## Value

A dataframe of message events as recorded by TOBII eye trackers

## Examples

``` r
if (FALSE) { # \dontrun{
raw_data <- hdf5_get_event("example_TOBII.hdf5")
} # }
```
