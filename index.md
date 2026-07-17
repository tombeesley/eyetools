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

Changes are fairly significant between 0.9.x and 0.10.0, and likely to
break old code. See the changes in
[NEWS.md](https://github.com/tombeesley/eyetools/blob/master/NEWS.md)

Available functions in the latest CRAN version:

| Implemented functions | Description |
|----|----|
| [`AOI_seq()`](https://tombeesley.github.io/eyetools/reference/AOI_seq.md) | Detect the sequence in which AOIs were entered in a trial |
| [`AOI_time()`](https://tombeesley.github.io/eyetools/reference/AOI_time.md) | Calculate time on AOIs; works with raw and fixation data |
| [`AOI_time_binned()`](https://tombeesley.github.io/eyetools/reference/AOI_time_binned.md) | Binned time analysis of area of interest entries |
| `ASC_todf()` | Converts eyetracking data from EyeLink ASC files to a “eyetools ready” dataframe |
| [`combine_eyes()`](https://tombeesley.github.io/eyetools/reference/combine_eyes.md) | Combines binocular data (i.e., average or “best eye”) into monocular data |
| [`compare_algorithms()`](https://tombeesley.github.io/eyetools/reference/compare_algorithms.md) | Provides a comparison between the dispersion and VTI fixation algorithms with correlations and plot |
| [`conditional_transform()`](https://tombeesley.github.io/eyetools/reference/conditional_transform.md) | Implements a single-axis flip for specific trials to normalise data with counterbalanced designs |
| [`create_AOI_df()`](https://tombeesley.github.io/eyetools/reference/create_AOI_df.md). | Create and populate a data frame for specifying AOIs |
| [`dist_to_visual_angle()`](https://tombeesley.github.io/eyetools/reference/dist_to_visual_angle.md) | Convert pixel or cm distances into visual angle |
| `estimate_sample_rate()` | Estimate the sample rate of the tracker based on timestamps in the data |
| [`fixation_dispersion()`](https://tombeesley.github.io/eyetools/reference/fixation_dispersion.md) | Dispersion algorithm for fixation detection |
| [`fixation_VTI()`](https://tombeesley.github.io/eyetools/reference/fixation_VTI.md) | An algorithm that subtracts saccadic periods for fixation detection |
| [`hdf5_get_event()`](https://tombeesley.github.io/eyetools/reference/hdf5_get_event.md) | Extracts message event files from a TOBII-generated hdf5 files to dataframe |
| [`hdf5_to_df()`](https://tombeesley.github.io/eyetools/reference/hdf5_to_df.md) | Converts eyetracking data from Tobii hdf5 files to a “eyetools ready” dataframe |
| [`interpolate()`](https://tombeesley.github.io/eyetools/reference/interpolate.md) | Interpolates data across gaps; provides a summary report of repair |
| [`plot_AOI_growth()`](https://tombeesley.github.io/eyetools/reference/plot_AOI_growth.md) | Plots absolute or proportional time spent in AOIs over time |
| [`plot_heatmap()`](https://tombeesley.github.io/eyetools/reference/plot_heatmap.md) | Plots a heatmap of raw data. |
| [`plot_seq()`](https://tombeesley.github.io/eyetools/reference/plot_seq.md) | provides a 2D plot of raw data for a single trial. Data can be split into time bins |
| [`plot_spatial()`](https://tombeesley.github.io/eyetools/reference/plot_spatial.md) | provides a 2D plot of raw data, fixations, saccades, and AOIs |
| [`saccade_VTI()`](https://tombeesley.github.io/eyetools/reference/saccade_VTI.md) | Velocity threshold algorithm for saccade detection. Provides summary of velocity, location, duration |
| [`smoother()`](https://tombeesley.github.io/eyetools/reference/smoother.md) | smooths data for use in saccade algorithms |
| [`tracker_properties_get()`](https://tombeesley.github.io/eyetools/reference/tracker_properties_get.md) | Retrieves the current eye-tracker properties (used for data collection) |
| [`tracker_properties_set()`](https://tombeesley.github.io/eyetools/reference/tracker_properties_set.md) | Sets the current eye-tracker properties (used for data collection) |

**Development version:**

The above CRAN version is considered fairly stable and will only be
updated every few months. We work on new features in the development
version. This version should be considered very experimental and may
have bugs. You can install this using
`devtools::install_github("tombeesley/eyetools@0.X.X")` where 0.X.X is
the latest version.

#### The current development version is: 0.10.1

The main changes are:
