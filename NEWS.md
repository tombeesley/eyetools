# eyetools (development version)

# eyetools 0.7.3
* added plot_AOI_growth()
* fixed problem with AOI_seq() where it couldn't handle trials without fixations or entries

# eyetools 0.7.2
* updated function examples to \donttest where appropriate

# eyetools 0.7.1
* updated functions to not print(), instead uses message()

# eyetools 0.7.0
* added support for multi-participant data in most functions
* standardised expected data input to functions
* added optional parameter for proportion of time spent to AOI_time()
* fixed smoother() span parameter
* added plots to smoother()
* improved handling of variable order in all functions

# eyetools 0.6.1
* added new functions: compare_algorithms(), conditional_transform(), fixation_VTI(), hdf5_to_csv()

# eyetools 0.6.0

# eyetools 0.5.1

# eyetools 0.5.0
* added new function seq_plot()
* presents raw data with time component
* data can be presented in time windows

# eyetools 0.4.7

* added a new function AOI_seq()
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
