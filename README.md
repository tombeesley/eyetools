# **eyetools**

## A set of tools for eye data processing, analysis and visualisation in R

**eyetools** is a package that provides a set of simple tools that will facilitate common steps in the processing and analysis of eye data. It is intended for use with data from psychological experiments. The idea is to have a workflow which is aided by these functions, going from processing of the raw data, to extraction of event related data (i.e., fixations, saccades), to summarising those data at the trial level (e.g., time on areas of interest). 

**Currently of limited use and very experimental!**

to install: `devtools::install_github("tombeesley/eyetools")`

It is free to use under the GNU General Public Licence..

Vague "roadmap" for functions:

|order | process | implemented function(s) | comment
|-|-|-|-|
|1.| combine binocular data | `combine_eyes()` | works: either average or "best eye" |
|2.| interpolation | `interpolate()` | working and provides a summary report of repair |
|3.| smoothing | | |
|4.| dispersion-based fixations  | `fix_dispersion()` | working and pretty fast - needs thorough checking |
|5.| area of interest analysis   | `AOI_time()` | working in basic form - needs thorough checking |
|6.| Visualisations - heatmaps, fixation plots, etc  | `spatial_plot()` | provides a 2D plot of raw data and fixations |
|7.| saccade detection (angle, speed, start/end, timing)  | | |
|8.| velocity-based fixations (from saccades)  | | |
|9.| scan paths (OG to be prodded fiercely)  | | |

*update notes:*

30/05/2022 - `combine_eyes()` added. Saccade algorithm work has started and needs checking

08/02/2021 - `AOI_time()` added. Provides basic results using fixation data as input.

17/09/2020 - `fix_dispersion()` and `spatial_plot()` added. Almost useful...

19/08/2020 - `interpolate()` seems to work. 

17/08/2020 - it doesn't do ANYTHING at the moment. Don't even think about using it.
