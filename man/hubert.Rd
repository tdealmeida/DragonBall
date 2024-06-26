% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hubert.R
\name{hubert}
\alias{hubert}
\title{Change Point Detection using hubert algorithm}
\usage{
hubert(data, alpha = 0.05)
}
\arguments{
\item{data}{Numeric vector containing the data to segment.}

\item{alpha}{Numeric value representing the alpha parameter for Hubert segmentation.}
}
\value{
A numeric vector containing the segmentation points.
}
\description{
Change Point Detection using hubert algorithm
}
\examples{
data <- c(rnorm(40,0,2),rnorm(60,5,2),rnorm(60,1,2),rnorm(40,5,2))
alpha <- 0.05
hubert(data, alpha)
}
