% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_spatial.R
\name{plot_spatial}
\alias{plot_spatial}
\title{Plot raw data and fixations}
\usage{
plot_spatial(
  raw_data = NULL,
  fix_data = NULL,
  sac_data = NULL,
  AOIs = NULL,
  trial_number = NULL,
  bg_image = NULL,
  res = c(0, 1920, 0, 1080),
  flip_y = FALSE,
  show_fix_order = TRUE,
  plot_header = FALSE
)
}
\arguments{
\item{raw_data}{data in standard raw data form (time, x, y, trial)}

\item{fix_data}{data output from fixation function}

\item{sac_data}{data output from saccade function}

\item{AOIs}{A dataframe of areas of interest (AOIs), with one row per AOI (x, y, width_radius, height). If using circular AOIs, then the 3rd column is used for the radius and the height should be set to NA.}

\item{trial_number}{can be used to select particular trials within the data}

\item{bg_image}{The filepath of an image to be added to the plot, for example to show a screenshot of the task.}

\item{res}{resolution of the display to be shown, as a vector (xmin, xmax, ymin, ymax)}

\item{flip_y}{reverse the y axis coordinates (useful if origin is top of the screen)}

\item{show_fix_order}{label the fixations in the order they were made}

\item{plot_header}{display the header title text which explains graphical features of the plot.}
}
\value{
a plot of the raw data
}
\description{
A tool for visualising raw eye-data, processed fixations, and saccades. Can use all three data types together and independently. Fixations can be labeled
in the order they were made. Can overlay areas of interest (AOIs) and customise the resolution.
}
\examples{
\donttest{
data <- combine_eyes(HCL)
data <- data[data$pNum == 118,]
# plot the raw data
plot_spatial(raw_data = data)

# plot both raw and fixation data together
plot_spatial(raw_data = data, fix_data = fixation_dispersion(data))

#plot one trial
plot_spatial(raw_data = data, fix_data = fixation_dispersion(data), trial_number = 1)

}
}
