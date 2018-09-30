\name{ppcc}
\alias{ppcc}
\title{The Probability Plot Correlation Coefficients Plot}
\usage{
ppcc(x, distribution = "weibull", minshape = 1, maxshape = 10, steps1 = 30, steps2 = 30, plots = FALSE, brks = 70, bandw = "nrd0", returncor = FALSE))
}
\description{
\code{ppcc()} takes a vector of data \code{x} and finds correlation coefficients between \code{x} and a guessed distribution for several different shape values. The function \code{ppcc()} returns a plot of the correlation coefficients against the shape parameters and return the highest correlation and the best estimated shape parameter. The supported distributions are: Weibull, Normal, Log-Normal, Poisson and Exponential.
}
\arguments{
\item{x}{The datavector to be analyzed.}
\item{distribution}{The guessed distribution. Supported distributions are: Weibull, Normal, Log-Normal, Poisson and Exponential. Default is set so Weibull.}
\item{minshape}{The minimum shape value to use. Default set to 1.}
\item{maxhsape}{Tha maximum shape value to use. Default set to 10.}
\item{steps1}{The total number of correlation coefficients to calculate between \code{minshape} and \code{maxshape}. \code{steps1} is not used in the Weibull case if the shape parameter is smaller than 1. Default set to 30.}
\item{steps2}{The same as \code{steps1}, but \code{steps2} is only used in the Weibull case if the shape parameter is smaller than 1. Default set to 30.}
\item{plots}{If set to \code{TRUE}; returns a histogram with the estimated density and the true density for the best estimated shape parameter. Default is set to \code{FALSE}.}
\item{brks}{The number of breaks in the histogram, which is returned when \code{plots} is set to \code{TRUE}. Default is set to 70.}
\item{bandw}{The smoothing bandwidth which \code{R} uses when estimating the density of \code{x}. Important: use the \item{ppcc()} function more than once with \code{plots = TRUE} and find the best bandwidth, if the results are very unusual. Default is set to "nrd0".}
\item{returncor}{If set to \code{TRUE}, hte \code{ppcc()} function returns a matrix with the used shape parameters and the corresponding correlation coefficients. Deafult is set to \code{FALSE}.}
}
\examples{
# Simulate a Weibull distributed dataset:
x = rweibull(1000, 2)
# Use the ppcc() function to find the best shape parameter:
ppcc(x)

# Simulate a Poisson distributed dataset:
x = rpois(1000, 3)
# Use th ppcc() function to estimate the best lambda:
ppcc(x, distibution = "poisson")
# Get a histogram with the estimated density and the true density for the best estimated lambda:
ppcc(x, distribution = "poisson", plots = TRUE)
}
