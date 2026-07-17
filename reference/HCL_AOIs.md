# Example AOIs for use with HCL

This dataframe contains three rectangular areas of interest (AOIs), set
out for use with the HCL dataset. Values are in pixels. This is the
standard AOI format required by many functions in the package. A
template dataframe can be created using create_AOI_df()

## Usage

``` r
HCL_AOIs
```

## Format

A data frame with 3 rows and 5 variables:

- name:

  custom name for the AOI

- x:

  centred x coordinate of the AOI

- y:

  centred y coordinate of the AOI

- width_radius:

  either the width of the AOI, or the radius for circular AOIs

- height:

  the height of the AOI; should be NA for circular AOIs
