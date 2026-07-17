# Analysis of time spent in areas of interest

Analyses total time on defined AOI regions across trials. Works with
fixation and raw data as the input (must use one or the other, not
both).

## Usage

``` r
AOI_time(data, data_type = NULL, AOIs, as_prop = FALSE, trial_time = NULL)
```

## Arguments

- data:

  A dataframe of either fixation data (from fix_dispersion) or raw data

- data_type:

  Whether data is a fixation ("fix") or raw data ("raw")

- AOIs:

  A dataframe of areas of interest (AOIs), with one row per AOI (name,
  x, y, width_radius, height).

- as_prop:

  whether to return time in AOI as a proportion of the total time of
  trial

- trial_time:

  needed if as_prop is set to TRUE. a vector of the time taken in each
  trial. Equal to the length of x trials by y participants in the
  dataset

## Value

a dataframe containing the time on the passed AOIs for each trial. One
column for each AOI separated by trial.

## Details

Analyses data separately for each unique combination of values in `pID`
and `trial`. Returned values can be absolute time or proportion of time
over the period.

## Examples

``` r

# \donttest{
data <- combine_eyes(HCL)
fix_d <- fixation_dispersion(data)

# fixation data
AOI_time(data = fix_d, data_type = "fix", AOIs = HCL_AOIs)
#>    pID trial    AOI time
#> 1  118     1   left 3500
#> 2  118     2   left 1400
#> 3  118     3   left  576
#> 4  118     4   left 2563
#> 5  118     5   left  253
#> 6  118     6   left  789
#> 7  118     1  right 1598
#> 8  118     2  right 1292
#> 9  118     3  right  743
#> 10 118     4  right 3574
#> 11 118     5  right 1101
#> 12 118     6  right  909
#> 13 118     1 centre 6056
#> 14 118     2 centre 3924
#> 15 118     3 centre 2932
#> 16 118     4 centre 2609
#> 17 118     5 centre 2136
#> 18 118     6 centre 2564
#> 19 119     1   left 3656
#> 20 119     2   left  912
#> 21 119     3   left 1959
#> 22 119     4   left 4288
#> 23 119     5   left 4805
#> 24 119     6   left 3447
#> 25 119     1  right 1743
#> 26 119     2  right  880
#> 27 119     3  right 2152
#> 28 119     4  right 2043
#> 29 119     5  right 3270
#> 30 119     6  right 2920
#> 31 119     1 centre 2740
#> 32 119     2 centre 1944
#> 33 119     3 centre 2106
#> 34 119     4 centre 1686
#> 35 119     5 centre 2983
#> 36 119     6 centre 4962

#raw data
AOI_time(data = data, data_type = "raw", AOIs = HCL_AOIs)
#> Eye tracker frequency estimated at:300.052002080083Hz
#>    pID trial    AOI time
#> 1  118     1   left 3663
#> 2  118     2   left 1420
#> 3  118     3   left  593
#> 4  118     4   left 2580
#> 5  118     5   left  370
#> 6  118     6   left  817
#> 7  118     1  right 1723
#> 8  118     2  right 1373
#> 9  118     3  right  797
#> 10 118     4  right 3783
#> 11 118     5  right 1160
#> 12 118     6  right  927
#> 13 118     1 centre 6199
#> 14 118     2 centre 4179
#> 15 118     3 centre 3089
#> 16 118     4 centre 2903
#> 17 118     5 centre 2350
#> 18 118     6 centre 2916
#> 19 119     1   left 3926
#> 20 119     2   left  953
#> 21 119     3   left 2020
#> 22 119     4   left 4376
#> 23 119     5   left 5066
#> 24 119     6   left 3513
#> 25 119     1  right 1813
#> 26 119     2  right 1046
#> 27 119     3  right 2333
#> 28 119     4  right 2120
#> 29 119     5  right 3723
#> 30 119     6  right 3153
#> 31 119     1 centre 2816
#> 32 119     2 centre 2166
#> 33 119     3 centre 2410
#> 34 119     4 centre 1770
#> 35 119     5 centre 3183
#> 36 119     6 centre 5056

#as proportional data
AOI_time(data = fix_d, data_type = "fix", AOIs = HCL_AOIs,
         as_prop = TRUE, trial_time = HCL_behavioural$RT)
#>    pID trial    AOI       time
#> 1  118     1   left 0.25993316
#> 2  118     1  right 0.11867805
#> 3  118     1 centre 0.44975863
#> 4  118     2   left 0.17958618
#> 5  118     2  right 0.16573239
#> 6  118     2 centre 0.50335441
#> 7  118     3   left 0.10936432
#> 8  118     3  right 0.14107238
#> 9  118     3 centre 0.55669477
#> 10 118     4   left 0.25859894
#> 11 118     4  right 0.36060579
#> 12 118     4 centre 0.26324021
#> 13 118     5   left 0.05718548
#> 14 118     5  right 0.24885855
#> 15 118     5 centre 0.48279915
#> 16 118     6   left 0.15102791
#> 17 118     6  right 0.17399793
#> 18 118     6 centre 0.49079285
#> 19 119     1   left 0.35980711
#> 20 119     1  right 0.17153823
#> 21 119     1 centre 0.26965850
#> 22 119     2   left 0.17986747
#> 23 119     2  right 0.17355633
#> 24 119     2 centre 0.38340170
#> 25 119     3   left 0.25872657
#> 26 119     3  right 0.28421623
#> 27 119     3 centre 0.27814097
#> 28 119     4   left 0.47366037
#> 29 119     4  right 0.22567354
#> 30 119     4 centre 0.18623866
#> 31 119     5   left 0.37422118
#> 32 119     5  right 0.25467290
#> 33 119     5 centre 0.23232087
#> 34 119     6   left 0.27193121
#> 35 119     6  right 0.23035658
#> 36 119     6 centre 0.39144841
# }
```
