\name{sdplot}
\alias{sdplot}
\title{The Standard Deviation Plot}
\usage{
sdplot(x, nr = 0, divided = TRUE, return = FALSE)
}
\description{
\code{sdplot()} takes a vector of data, a list or a data frame \code{x} and finds the standard deviations for the different parts in the dataset. \code{sdplot()} is able to divide a vector of data into a self-chosen number of parts which are distributed "as equal as possible". The function \code{sdplot()} returns a plot of the standard deviations and the median of those.
}
\arguments{
\item{x}{The data to be analyzed. \code{x} needs to be a vector, a list or a data frame. \code{x} has to be bigger than 0 and smaller than \code{length(x)}.}
\item{nr}{The total number of parts, the datasat has to be divided in, if \code{x} is a vector.}
\item{divided}{Needs to be defined to tell the function whether the dataset \code{x} is already divided into parts (list or data frame) or not divided into parts (vector). If \code{divided} is set to \code{FALSE} the function divides the dataset into \code{nr} as equal as possible distributed parts. Default set to \code{TRUE}.}
\item{return}{If set to \code{TRUE}, the function \code{sdplot()} returns a vector of the estimated standard deviations. Default set to \code{FALSE}.}
}
\examples{
# Simulate a dataset from a Poisson distribution:
x = rpois(1000, 3)
# Use sdplot() to divide x into 20 parts and to find the different standard deviations:
sdplot(x, divided = FALSE, nr = 20)
# Also return the estimated standard deviations:
sdplot(x, divided = FALSE, nr = 20, return = TRUE)

# Define a list (or data frame) of data:
x = list(c(1:4), c(4:10), c(1,6,1,8,0,0,98,6), c(10,11,11,11,11,6,3))
# Use sdplot() to find the standard deviations for the different parts:
sdplot(x)
}
