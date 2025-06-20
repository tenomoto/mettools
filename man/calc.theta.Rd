% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moist.R
\name{calc.theta}
\alias{calc.theta}
\title{potential temperature}
\usage{
calc.theta(T, w, p)
}
\arguments{
\item{T}{temperature K}

\item{w}{mixing ratio kg/kg}

\item{p}{pressure Pa}
}
\value{
potential temperature K
}
\description{
Calculate potential temperature
    Bolton (1980)
}
\examples{
theta <- calc.theta(300, 0.01, 850e2)
}
