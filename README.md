# **eyetools**

## A set of tools for eye data processing, analysis and visualisation in R

**eyetools** is a package that provides a set of simple tools that will facilitate common steps in the processing and analysis of eye data. It is intended for use with data from psychological experiments. The idea is to have a workflow which is aided by these functions, going from processing of the raw data, to extraction of event related data (i.e., fixations, saccades), to summarising those data at the trial level (e.g., time on areas of interest). 

**Currently of limited use and very experimental!**

It is free to use under the GNU General Public Licence. 

Vague "roadmap" for functions:

|order | process | implemented function(s) | comment
|-|-|-|-|
|1.| interpolation | `interpolate()` | working and provides a summary report of repair |
|2.| smoothing | | |
|3.| dispersion-based fixations  | `fix_dispersion()` | working and pretty fast - needs thorough checking |
|4.| area of interest analysis   | | |
|5.| heatmaps (some sort of wrapper for easy ploting commands)  | `spatial_plot()` | provides a 2D plot of raw data and fixations |
|6.| saccade detection (angle, speed, start/end, timing)  | | |
|7.| velocity-based fixations (from saccades)  | | |
|8.| scan paths (OG to be prodded fiercely)  | | |

*update notes:*

17/09/2020 - `fix_dispersion()` and `spatial_plot()` added. Almost useful...

19/08/2020 - `interpolate()` seems to work. 

17/08/2020 - it doesn't do ANYTHING at the moment. Don't even think about using it.
