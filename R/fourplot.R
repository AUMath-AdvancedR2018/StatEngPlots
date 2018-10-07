fourplot <- function(x){

   n <- length(x)

  par(mfrow=c(2,2))
  # first plot of y[i] vs. i.
  plot( x, main = "Sequence Plot", xlab = "i", ylab = "y[i]")
  # second plot of y[i] vs. y[i-1].
  plot(n, main = "Lag Plot", xlab = "y[i-1]", ylab = "[i]")
  # third plot a histogram of y.
  hist(x, main = "Histogram of y", xlab = "y", ylab = "Counts")
  # fourth plot a normal probability plot of y.
  qqnorm(x, main ="Normal probability plot", xlab = "Theoretical values from a
        standard normal distribution", ylab = "Ordered y[i]")

}
