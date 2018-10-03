\name{meanplot}
\alias{meanplot}
\title{The Mean Plot}
\usage{
meanplot(x, nr = 0, divided = TRUE, return = FALSE)
}
\description{
\code{meanplot()} takes a vector of data, a list or a data frame \code{x} and finds the mean for the different parts in the dataset. \code{meanplot()} is able to divide a vector of data into a self-chosen number of parts. The function \code{mean()} returns a plot of the means and the median of those.
}
\arguments{
\item{x}{The data to be analyzed. \code{x} needs to be a vector, a list or a data frame. \code{x} has to be bigger than 0 and smaller than \code{length(x)}.}
\item{nr}{The total number of parts, the datasat has to be divided in, if \code{x} is a vector.}
\item{divided}{Needs to be defined to tell the function whether the dataset \code{x} is already divided into parts (list or data frame) or not divided into parts (vector). If \code{divided} is set to \code{FALSE} the function divides the dataset into \code{nr} as equal as possible distributed parts. Default set to \code{TRUE}.}
\item{return}{If set to \code{TRUE}, the function \code{sdplot()} returns a vector of the estimated standard deviations. Default set to \code{FALSE}.}
}
\examples{
#Simulate a normal distributed data set:
x = dnorm(1000,0,1)
# Use meanplot() to divide x into 10 parts and to find the different means:
meanplot(x, divided = FALSE, nr = 10)
# Also return the estimated standard deviations:
meanplot(x, divided = FALSE, nr = 10, return = TRUE)
}
