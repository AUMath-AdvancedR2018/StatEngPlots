\name{sdplot}
\alias{sdplot}
\title{The Standard Deviation Plot}
\usage{
sdplot(x, nr = 0, divide = TRUE, divided = TRUE, return = FALSE)
}
\description{
\code{sdplot()} takes a vector of data, a list or a data frame \code{x} and finds the standard deviations for the different parts in the dataset. \code{sdplot()} is able to divide a vector of data into a self-chosen number of parts. The function \code{sdplot()} returns a plot of the standard deviations and the median of those. One more function is to find kind of a continous standard deviation plot, where the first \code{nr} numbers of elements are evaluated and then continues through the whole dataset with steps 1.
}
\arguments{
\item{x}{The data to be analyzed. \code{x} needs to be a vector, a list or a data frame.}
\item{nr}{If \code{divide = TRUE}: The total number of parts, the datasat has to be divided in, if \code{x} is a vector.\n If \code{divide = FALSE}: The number of elements which has to be evaluated at the same time while using the continous method.}
\item{divide}{If set to \code{FALSE}, the continous method is used to find standard deviations. Default set to \code{TRUE}.}
\item{divided}{Needs to be defined to tell the function whether the dataset \code{x} is already divided into parts (list or data frame) or not divided into parts (vector). If \code{divided} is set to \code{TRUE} the function divides the dataset into \code{nr} parts which are "as equal as possible". Default set to \code{TRUE}.}
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

# Use the continous method with a list (or data frame or vector):
a = sample(100,50)
sdplot(a, divide = FALSE, nr = 10, return = TRUE)
