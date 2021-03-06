% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{fars_map_state}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{the number of the state in the FARS data - coerced to an integer}

\item{year}{the year for which data is read}
}
\value{
a map of the state, with accident locations plottec
}
\description{
A function which uses the maps package to plot the locations accidents in the FARS dataset for a given year and state.
This function will return an error if the provided state number is invalid,
or if there are no accidents in the dataset for the given state and year combination.
}
\details{
Accidents with a longitude >900 or a latitude >90 have their lat/long set to NA
}
\examples{
\dontrun{fars_map_state('51','2014')}

}
