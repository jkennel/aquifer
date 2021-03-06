% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convolution_fft.R
\name{hantush_convolve}
\alias{hantush_convolve}
\title{hantush_convolve}
\usage{
hantush_convolve(radius, storativity, transmissivity, leakage, time,
  flow_rate, n_terms = 10)
}
\arguments{
\item{radius}{distance to monitoring location}

\item{storativity}{the storativity of the aquifer}

\item{transmissivity}{the transmissivity of the aquifer}

\item{leakage}{the leakage rate of the aquitard}

\item{time}{the time to output}

\item{flow_rate}{vector of flow rates}

\item{n_terms}{number of terms in the hantush well function approximation}
}
\value{
hantush solution for multiple pumping periods
}
\description{
Implementation of Hantush leaky aquifer solution
}
