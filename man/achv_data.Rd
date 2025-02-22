% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/achv_data.R
\docType{data}
\name{achv_data}
\alias{achv_data}
\title{Example Dataset: Program Indicators with results, targets and achievement}
\format{
A data frame with 12 rows and 6 variables:
\describe{
\item{operating_unit}{Operating unit name, character}
\item{program_area}{Program area name, character}
\item{indicator}{Indicator name, character}
\item{Result}{Result value, integer}
\item{Target}{Target value, integer}
\item{Achievement}{Achievement value, numeric}
}
}
\source{
Generated for demonstration purposes
}
\usage{
achv_data
}
\description{
This dataset contains example program indicators for two operating units, Minoria and Hyrule. It includes information on prevention, testing, and treatment indicators, with results, targets, and achievements.
}
\examples{
data(achv_data)
head(achv_data)
}
\keyword{datasets}
