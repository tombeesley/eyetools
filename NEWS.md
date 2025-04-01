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
