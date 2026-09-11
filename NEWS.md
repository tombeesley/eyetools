# eyetools 0.10.1
* added amplitude, direction, and AOI information to the output of `saccade_VTI()`
* added a check to utils for unique NA values in a sample, which is corrected to NA for both x and y. This check is called in interpolate, and all fix/sac algorithms. 
* saccade_VTI now returns columns for amplitude and direction of saccade
* changed the parameter "threshold" to "vel_threshold" (velocity) in `fixation_VTI()` and `saccade_VTI()`
* fixed an issue with data output from `saccade_VTI` having the wrong data type

# eyetools 0.10.0
* stable version merged from 0.9.3

# eyetools 0.9.3
* added a on_attach "welcome" message when the package is loaded
* added ggrepel::geom_label_repel to `plot_spatial()`, to avoid overlapping labels for fixations
* fixed issue using a colour scale in `plot_spatial()` with only a single fixation
* fixed an issue with displaying a background image without alpha transparency layer
* fixed bug with `fixation_VTI()`
* fixed an issue with screenshot not displaying on plots when flip_y parameter is TRUE
* Added two functions to "set" and "get" the tracker properties, for storing global settings used by many functions
* updated the default screen width to 53cm (24 inch monitor)
* added ASC_to_df() function which will import the raw data from an EyeLink SR ASC file and convert to eyetools-ready data format. 
* removed AOI_names argument from all AOI_ functions. **The name of each AOI should be added to the AOI dataframe**. See the updated `create_AOI_df()`
* added a velocity threshold to the interpolate function, such that interpolation only occurs when there are no large changes in velocity detected.

# eyetools 0.9.2
* updated CRAN version

# eyetools 0.9.1
* fixed issue with AOI_seq() not working with more than 3 AOIs

# eyetools 0.9.0
* removed the need to specify a name for the participant column. eyetools now expects the column 'pID' in all input data
* all 'plot_' functions now have a 'pID_values' parameter to enable selection of certain participant data to plot
* renamed 'trial_number' to 'trial_values' for consistency with the above changes
* fixed an issue where colour scale for fixations was dependent on showing fixation label
* cosmetic changes made to plotting of AOIs 

# eyetools 0.8.1
* Improved `fixation_dispersion()` - now runs faster
* Fixed bug in AOI_seq handling trials with no fixations
* `AOI_time()` now returns data in long format

# eyetools 0.8.0
* renamed `hdf5_to_csv()` to `hdf5_to_df()` to accurately reflect operation
* added `hdf5_get_event()` to access messages stored in the TOBII generated hdf5
* updated the sample_rate estimation code in `*_VTI()`, `interpolate()`, and `AOI_time*()` functions 
* updated plot aesthetics for colour-blindness (using viridis), and improving flexibility of use
* Added `create_AOI_df()` for generating an empty dataframe of the required column names

# eyetools 0.7.3
* added `plot_AOI_growth()`
* fixed problem with `AOI_seq()` where it couldn't handle trials without fixations or entries
* added create_AOI_df() which will create a blank data frame for populating with AOIs

# eyetools 0.7.2
* updated function examples to \donttest where appropriate

# eyetools 0.7.1
* updated functions to not `print()`, instead uses `message()`

# eyetools 0.7.0
* added support for multi-participant data in most functions
* standardised expected data input to functions
* added optional parameter for proportion of time spent to `AOI_time()`
* fixed `smoother()` span parameter
* added plots to `smoother()`
* improved handling of variable order in all functions

# eyetools 0.6.1
* added new functions: `compare_algorithms()`, `conditional_transform()`, `fixation_VTI()`, `hdf5_to_csv()`

# eyetools 0.6.0

# eyetools 0.5.1

# eyetools 0.5.0
* added new function `seq_plot()`
* presents raw data with time component
* data can be presented in time windows

# eyetools 0.4.7

* added a new function `AOI_seq()`
* AOI_trial now works with raw data

# eyetools 0.4.6

# eyetools 0.4.5

# eyetools 0.4.4

# eyetools 0.4.3

* updated data files and documentation
* started process of using pkgdown
* fixed `combine_eyes()` to remove the column 'trial_phase'

# eyetools 0.4.2

* tidying up the dependencies

# eyetools 0.4.1

* added a progress bar (pbapply) to `fix_dispersion()`.
* fixed a bug in `VTI_saccade()` where it couldn't handle trials without saccades 

# eyetools 0.4.0

* Added a `NEWS.md` file to track changes to the package.
