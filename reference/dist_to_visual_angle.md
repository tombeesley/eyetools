# Compute visual angle from distance metrics

Takes a single value or vector of distances and returns the visual angle
equivalent.

## Usage

``` r
dist_to_visual_angle(vector, dist_type = "cm")
```

## Arguments

- vector:

  vector of distances (or single distance)

- dist_type:

  default is "cm". Specify "pixel" for conversion from pixel values.

## Value

an equivalent-sized object to the input

## Examples

``` r
# calculate visual angle for stimulus of 5cm
dist_to_visual_angle(5)
#> [1] 4.771888

# calculate visual angle of stimuli 2cm and 10cm width
dist_to_visual_angle(c(2,10))
#> [1] 1.909683 9.527283

# calculate visual angle of 150 pixel wide
dist_to_visual_angle(150, dist_type = "pixels")
#> [1] 102.6804
```
