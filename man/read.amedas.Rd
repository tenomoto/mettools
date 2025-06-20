% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amedas.R
\name{read.amedas}
\alias{read.amedas}
\title{read AMeDAS csv}
\usage{
read.amedas(fname, quality = 8, drop.consistency = TRUE)
}
\arguments{
\item{fname}{file name}

\item{quality}{rows with all quality flags greater than equal to
this value will be retained.}

\item{drop.consistency}{drop consistency flag fields (TRUE)}
}
\value{
data frame
}
\description{
read AMeDAS csv from JMA
}
\examples{
fpath <- system.file("extdata", "kyoto.csv", package="mettools")
df <- read.amedas(fpath)
}
