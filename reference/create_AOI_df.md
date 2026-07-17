# Create a blank data frame for populating with AOIs

Used to define a set of AOIs for use across various functions within the
eyetools package.

## Usage

``` r
create_AOI_df(num_AOIs = 3, AOI_data = NULL, AOI_names = NULL)
```

## Arguments

- num_AOIs:

  number of AOIs, setting the number of rows

- AOI_data:

  a list containing data for each AOI, ordered by x, y, width_radius,
  and height (NA if circular)

- AOI_names:

  a vector of names for the AOIs specified in AOI_data

## Value

an AOI dataframe in the format required for several AOI functions in
eyetools

## Examples

``` r
# create an empty data frame with 3 AOIs
create_AOI_df(3)
#>    name  x  y width_radius height
#> 1 AOI_1 NA NA           NA     NA
#> 2 AOI_2 NA NA           NA     NA
#> 3 AOI_3 NA NA           NA     NA

# create an AOI dataframe with data, the second of which is circular, with names
create_AOI_df(num_AOIs = 3, 
              AOI_data = list(c(460,840,400,300), c(1460,840,400,NA), c(960,270,300,500)),
              AOI_names = c("AOI_1", "AOI_2", "AOI_3"))
#>    name    x   y width_radius height
#> 1 AOI_1  460 840          400    300
#> 2 AOI_2 1460 840          400     NA
#> 3 AOI_3  960 270          300    500
```
