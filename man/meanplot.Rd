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

}
\examples{
#Simulate a normal distributed data set:
x = dnorm(1000,3)
# Use meanplot() to divide x into 10 parts and to find the different means:
meanplot(x, divided = FALSE, nr = 10)
# Also return the estimated standard deviations:
meanplot(x, divided = FALSE, nr = 10, return = TRUE)


}
