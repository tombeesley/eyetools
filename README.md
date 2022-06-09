
# **eyetools**

## A set of tools for eye data processing, analysis and visualisation in R

**eyetools** is a package that provides a set of simple tools that will
facilitate common steps in the processing and analysis of eye data. It
is intended for use with data from psychological experiments. The idea
is to have a workflow which is aided by these functions, going from
processing of the raw data, to extraction of event related data (i.e.,
fixations, saccades), to summarising those data at the trial level
(e.g., time on areas of interest).

**Warning - still in experimental form! Please check results carefully**

to install: `devtools::install_github("tombeesley/eyetools")`

It is free to use under the GNU General Public Licence..

Vague “roadmap” for functions:

| order | process                                        | implemented function(s) | comment                                                                        |
|-------|------------------------------------------------|-------------------------|--------------------------------------------------------------------------------|
| 1\.   | combine binocular data                         | `combine_eyes()`        | works: either average or “best eye”                                            |
| 2\.   | interpolation                                  | `interpolate()`         | working and provides a summary report of repair                                |
| 3\.   | smoothing                                      | `smoother()`            | working                                                                        |
| 4\.   | dispersion-based fixations                     | `fix_dispersion()`      | working and pretty fast - needs thorough checking                              |
| 5\.   | area of interest analysis                      | `AOI_time()`            | working in basic form - needs thorough checking                                |
| 6\.   | Visualisations - heatmaps, fixation plots, etc | `spatial_plot()`        | provides a 2D plot of raw data and fixations                                   |
| 7\.   | Saccade detection                              | `VTI_saccade()`         | Working in basic form - provides summary of velocity, start/end, duration, etc |
| 8\.   | velocity-based fixations                       |                         |                                                                                |
| 9\.   | scan paths (OG to be prodded fiercely)         |                         |                                                                                |

## How to use eyetools (work in progress)

**Installation**

You can install eyetools using the following code:

``` r
if (!require(devtools)) {
  install.packages("devtools")
  library(devtools)
}
install_github("tombeesley/eyetools")
```

and then load it:

``` r
library(eyetools)
```

**The format of raw data**

Data needs to be in a particular format to be compatible with the
functions in eyetools. This format is 4 columns of data, with each row
representing a sample. The four columns are: time, x, y, and trial. You
can see an example with the built in data sets:

``` r
example_raw_sac
```

    ## # A tibble: 32,608 x 4
    ##     time     x     y trial
    ##    <dbl> <dbl> <dbl> <dbl>
    ##  1     0  940.  535.     1
    ##  2     3  940.  536.     1
    ##  3     7  936.  533.     1
    ##  4    10  939.  536.     1
    ##  5    13  944.  533.     1
    ##  6    17  939.  535.     1
    ##  7    20  938.  531.     1
    ##  8    23  939.  536.     1
    ##  9    27  940.  534.     1
    ## 10    30  942.  537.     1
    ## # ... with 32,598 more rows

**Repairing data**

Raw data will often contain missing samples, which we can attempt to
repair. eyetools has an `interpolation()` function you can use to do
this. It will produce a report of how succesful the repair was in terms
of the missing data before and after interpolation:

``` r
eyetools::interpolate(example_raw_sac, report = TRUE)
```

    ## [[1]]
    ## # A tibble: 32,608 x 4
    ##     time     x     y trial
    ##    <dbl> <dbl> <dbl> <dbl>
    ##  1     0  940.  535.     1
    ##  2     3  940.  536.     1
    ##  3     7  936.  533.     1
    ##  4    10  939.  536.     1
    ##  5    13  944.  533.     1
    ##  6    17  939.  535.     1
    ##  7    20  938.  531.     1
    ##  8    23  939.  536.     1
    ##  9    27  940.  534.     1
    ## 10    30  942.  537.     1
    ## # ... with 32,598 more rows
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##   missing_perc_before missing_perc_after
    ##                 <dbl>              <dbl>
    ## 1              0.0998             0.0952

``` r
raw_data <- eyetools::interpolate(example_raw_sac) # store as new object
```

We can also apply a smoothing function over the data, which is
particularly important for the analysis of saccadic velocities.

``` r
smooth_data <- eyetools::smoother(example_raw_sac) 
```

``` r
library(tidyverse)

r <- filter(raw_data, trial == 2)
s <- filter(smooth_data, trial == 2)

ggplot() +
  geom_line(data = r, 
            aes(x = time, y = y),
            colour = "red") +
  geom_line(data = s, 
            aes(x = time, y = y),
            colour = "blue")
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

**Processing fixations** …

**Assessing time on areas of interest** …

**Plotting data** …

*update notes:*

09/06/2022 - `VTI_saccade()` algorithm is working in a basic form

30/05/2022 - `combine_eyes()` added. Saccade algorithm work has started
and needs checking

08/02/2021 - `AOI_time()` added. Provides basic results using fixation
data as input.

17/09/2020 - `fix_dispersion()` and `spatial_plot()` added. Almost
useful…

19/08/2020 - `interpolate()` seems to work.

17/08/2020 - it doesn’t do ANYTHING at the moment. Don’t even think
about using it.
