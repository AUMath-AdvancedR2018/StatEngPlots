# PPCC Plot
ppcc <- function(x, distribution = "weibull", minshape = 1, maxshape = 4, steps1 = 10, steps2 = 10, plots = FALSE, brks = 100, meandist = FALSE, mean = NULL, bandw = 0.1, ...){
  if (distribution == "weibull"){
    est <- density(x, bw = bandw, n = length(x), from = 0)$y
    j = 1
    if (minshape >= 1){
      corvec <- integer(round((maxshape-1)/steps1))
      for (i in seq(1, maxshape, by = (maxshape-1)/steps1)){
        wei <- dweibull(seq(0, max(x), length = length(x)), shape = i)
        corvec[j] <- cor(est, wei)
        j = j+1
      }
      plot(seq(1, maxshape, by = (maxshape-1)/steps1),corvec)
      cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
      cat("The best shape parameter is", seq(minshape, maxshape, by=(maxshape-1)/steps1)[which.max(abs(corvec))])
    } else {
      corvec <- integer(round((1-minshape)/steps2))
      for (i in seq(minshape, 1, by = (1-minshape)/steps2)){
        wei <- dweibull(seq(0, max(x), length = length(x)+1), shape = i)
        wei <- wei[-1]
        corvec[j] <- cor(est, wei)
        j = j+1
      }
      plot(seq(minshape, 1, by = (1-minshape)/steps2),corvec)
      cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
      cat("The best shape parameter is", seq(minshape, maxshape, by=(1-minshape)/steps2)[which.max(abs(corvec))])
    }
  }
  if (plots == TRUE){
    hist(x, freq = FALSE, breaks = brks)
    curve(dweibull(x, seq(minshape, maxshape, by=steps)[which.max(corvec)]), col = "green", add = TRUE)
    lines(density(x, bw = bandw), col = "red")
    legend("topleft", c("True Weibull density", "Estimated density"), lty = 2, col = c("green", "red"))
  }
}
