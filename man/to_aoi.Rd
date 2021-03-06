% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{to_aoi}
\alias{to_aoi}
\title{Convert to Area of Interest}
\usage{
to_aoi(x)
}
\arguments{
\item{x}{An R object. Currently only atomic character vectors are accepted.}
}
\value{
An R object of class \code{aoi}
}
\description{
Convert an R object to an Area of Interest.
}
\examples{
to_aoi('South Carolina')
to_aoi('North Carolina')
}
