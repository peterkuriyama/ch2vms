% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bin_data.R
\name{bin_data}
\alias{bin_data}
\title{Bin data into grid
Function to count number of tows in bins. Uses ggplot stat_bin2d functions.}
\usage{
bin_data(data, x_col = "trans_lon", y_col = "trans_lat", group = "year",
  grid_size = c(0.0909, 0.11))
}
\arguments{
\item{data}{Data frame to bin}

\item{x_col}{name of longitude column}

\item{y_col}{name of latitude column}
}
\description{
Bin data into grid
Function to count number of tows in bins. Uses ggplot stat_bin2d functions.
}
\examples{
bin_data(data = )
}

