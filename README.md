
<!-- 
&#10;README.md is generated from README.Rmd. Please edit README.Rmd 
&#10;If you use index.Rmd or README.Rmd it's your responsibility to knit the document to create the corresponding .md. pkgdown does not do this for you because it only touches files in the doc/ directory.
&#10;-->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

# **eyetools**

## A set of tools for eye data processing, analysis and visualisation in R

**eyetools** is a package that provides a set of simple tools that will
facilitate common steps in the processing and analysis of eye data. It
is intended for use with data from psychological experiments. The idea
is to have a workflow which is aided by these functions, going from
processing of the raw data, to extraction of event related data (i.e.,
fixations, saccades), to summarising those data at the trial level
(e.g., time on areas of interest).

For an indepth guide to using eyetools, see the [Get Started
page](https://tombeesley.github.io/eyetools/articles/eyetools.html).

It is free to use under the GNU General Public Licence.

**To install use `install.packages("eyetools")`**

Available functions in the latest CRAN version:

| Implemented functions     | Description                                                                                          |
|---------------------------|------------------------------------------------------------------------------------------------------|
| `AOI_seq()`               | Detect the sequence in which AOIs were entered in a trial                                            |
| `AOI_time()`              | Calculate time on AOIs; works with raw and fixation data                                             |
| `combine_eyes()`          | Combines binocular data (i.e., average or “best eye”) into monocular data                            |
| `compare_algorithms()`    | Provides a comparison between the dispersion and VTI fixation algorithms with correlations and plot  |
| `conditional_transform()` | Implements a single-axis flip for specific trials to normalise data with counterbalanced designs     |
| `fixation_dispersion()`   | Dispersion algorithm for fixation detection                                                          |
| `fixation_VTI()`          | An algorithm that subtracts saccadic periods for fixation detection                                  |
| `hdf5_to_df()`            | converts eyetracking data retrieved from TOBII eyetrackers to a dataframe                            |
| `interpolate()`           | Interpolates data across gaps; provides a summary report of repair                                   |
| `plot_seq()`              | provides a 2D plot of raw data for a single trial. Data can be split into time bins                  |
| `plot_spatial()`          | provides a 2D plot of raw data, fixations, saccades, and AOIs                                        |
| `saccade_VTI()`           | Velocity threshold algorithm for saccade detection. Provides summary of velocity, location, duration |
| `smoother()`              | smooths data for use in saccade algorithms                                                           |

**Development version:**

The above CRAN version is considered fairly stable and will only be
updated every few months. We work on new features in the development
version. This version should be considered very experimental and may
have bugs. You can install this using
`devtools::install_github("tombeesley/eyetools@0.X.X")` where 0.X.X is
the latest version.

The current development version is: 0.7.3

Additional functions that are only available in the latest development
version:

| Implemented functions | Description                                                 |
|-----------------------|-------------------------------------------------------------|
| `plot_AOI_growth()`   | Plots absolute or proportional time spent in AOIs over time |
| `AOI_time_binned()`   | Binned time analysis of area of interest entries            |
