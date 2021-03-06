% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/register_siane.R
\name{register_siane}
\alias{register_siane}
\title{Register the map's path}
\usage{
register_siane(path)
}
\arguments{
\item{path}{: The path of the folder that contains the folder SIANE_CARTO_BASE_S_3M and the folder SIANE_CARTO_BASE_S_6M5.}
}
\value{
It returns the same path as a string.
}
\description{
A function that validates the path of Siane maps
}
\examples{
\dontrun{
obj <- register_siane("/home/ncarvalho/Downloads/")}
}
