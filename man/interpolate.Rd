% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolation.R
\name{interpolate}
\alias{interpolate}
\title{Interpolation of missing data (NAs)}
\usage{
interpolate(data, maxgap = 25, method = "approx", report = FALSE)
}
\arguments{
\item{data}{dataframe with columns time, x, y, trial (the standardised raw data form for eyeproc)}

\item{maxgap}{maximum number of consecutive NAs to fill. Any longer gaps will be left unchanged (see zoo package)}

\item{method}{use 'na.approx' (linear interpolation) or 'na.spline' (cubic spline interpolation)}

\item{report}{default is FALSE. If TRUE, then the return value is a list containing the returned data frame and the report.}
}
\value{

}
\description{
Extends the zoo::na.approx and zoo::na.spline functions to include a report which provides
the proportion of missing data before and after the interpolation process. This is handy
for evaluating the effectiveness of the different methods and parameters.
}
\examples{
interpolate(example_raw_2, method = "approx", maxgap = 50, report = TRUE)

}