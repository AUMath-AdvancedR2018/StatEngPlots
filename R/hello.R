# PPCC Plot
ppcc <- function(x, distribution = "weibull", minshape = 1, maxshape = 4, steps1 = 10, steps2 = 10, plots = FALSE, brks = 70, meandist = FALSE, mean = NULL, bandw = 1, ...){
  if (distribution == "weibull"){
    j = 1
    est <- density(x, bw = bandw, n = length(x), from = 0, to = max(x))$y
    if (minshape >= 1){
      corvec <- integer(steps1)
      for (i in seq(1, maxshape, by = (maxshape-1)/(steps1-1))){
        wei <- dweibull(seq(0, max(x), length = length(x)), shape = i)
        corvec[j] <- cor(est, wei)
        j = j+1
      }
      plot(seq(1, maxshape, by = (maxshape-1)/(steps1-1)),corvec)
      if (plots == TRUE){
        hist(x, freq = FALSE, breaks = brks)
        curve(dweibull(x, seq(minshape, maxshape, by=(maxshape-1)/(steps1-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
        lines(density(x, bw = bandw, from = 0, to = max(x)), col = "red")
        legend("topleft", c("True Weibull density", "Estimated density"), lty = 2, col = c("green", "red"))
      }
      cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
      cat("The best shape parameter is", seq(minshape, maxshape, by=(maxshape-1)/(steps1-1))[which.max(abs(corvec))])
    } else {
      corvec <- integer(steps2)
      for (i in seq(minshape, 1, by = (1-minshape)/(steps2-1))){
        wei <- dweibull(seq(0, max(x), length = length(x)+1), shape = i)
        wei <- wei[-1]
        corvec[j] <- cor(est, wei)
        j = j+1
      }
      plot(seq(minshape, 1, by = (1-minshape)/(steps2-1)), corvec)
      if (plots == TRUE){
        hist(x, freq = FALSE, breaks = brks)
        curve(dweibull(x, seq(minshape, maxshape, by=(1-minshape)/(steps2-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
        lines(density(x, bw = bandw, from = 0, to = max(x)), col = "red")
        legend("topleft", c("True Weibull density", "Estimated density"), lty = 2, col = c("green", "red"))
      }
      cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
      cat("The best shape parameter is", seq(minshape, maxshape, by = (1-minshape)/(steps2-1))[which.max(abs(corvec))])
    }
  } else if (distribution == "poisson"){
    j = 1
    est <- density(x, bw = bandw, n = length(x), from = 0, to = max(x))$y
    corvec <- integer(steps1)
    for (i in seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))){
      pois <- dpois(round(seq(0, max(x), length = length(x))), lambda = i) ###ask ute bcs of evl bias when using round()?
      corvec[j] <- cor(est, pois)
      j = j+1
    }
    plot(seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), corvec)
    if (plots == TRUE){
      hist(x, freq = FALSE, breaks = brks)
      curve(dpois(x, seq(minshape, maxshape, by=(maxshape-minshape)/(steps1-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
      lines(density(x, bw = bandw, from = 0, to = max(x)), col = "red")
      legend("topleft", c("True Poisson density", "Estimated density"), lty = 2, col = c("green", "red"))
    }
    cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
    cat("The best lambda is", seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))[which.max(abs(corvec))])
  } else if (distribution == "normal"){
    est <- density(x, bw = bandw, n = length(x),from = min(x), to = max(x))$y
    j = 1
    corvec <- integer(steps1)
    for (i in seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))){
      nor <- dnorm(seq(min(x), max(x), length = length(x)), mean = 0, sd = i)
      corvec[j] <- cor(est, nor)
      j = j+1
    }
    plot(seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), corvec)
    if (plots == TRUE){
      hist(x, freq = FALSE, breaks = brks)
      curve(dnorm(x, mean = 0, sd = seq(minshape, maxshape, by=(maxshape-minshape)/(steps1-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
      lines(density(x, bw = bandw, from = min(x), to = max(x)), col = "red")
      legend("topleft", c("True Normal density", "Estimated density"), lty = 2, col = c("green", "red"))
    }
    cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
    cat("The best standard deviation is", seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))[which.max(abs(corvec))])
  }
}
