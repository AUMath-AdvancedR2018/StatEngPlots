# PPCC Plot
ppcc <- function(x, distribution = "weibull", minshape = 1, maxshape = 10, steps1 = 30, steps2 = 30, plots = FALSE, brks = 70, bandw = "nrd0", returncor = FALSE){
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
      plot(seq(1, maxshape, by = (maxshape-1)/(steps1-1)), corvec)
      if (returncor == TRUE){
        df <- data.frame("Shape" = seq(1, maxshape, by = (maxshape-1)/(steps1-1)), "Correlation" = corvec)
        print(df)
      }
      if (plots == TRUE){
        a <- hist(x, freq = FALSE, breaks = brks)
        hist(x, freq = FALSE, breaks = brks, ylim = c(0, (max(a$density))*1.5))
        curve(dweibull(x, seq(minshape, maxshape, by=(maxshape-1)/(steps1-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
        lines(density(x, bw = bandw, from = 0, to = max(x)), col = "red")
        legend("topleft", c("True Weibull density", "Estimated density"), lty = 1, col = c("green", "red"))
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
      if (returncor == TRUE){
        df <- data.frame("Shape" = seq(minshape, 1, by = (1-minshape)/(steps2-1)), "Correlation" = corvec)
        print(df)
      }
      if (plots == TRUE){
        a <- hist(x, freq = FALSE, breaks = brks)
        hist(x, freq = FALSE, breaks = brks, ylim = c(0, (max(a$density))*1.5))
        curve(dweibull(x, seq(minshape, maxshape, by=(1-minshape)/(steps2-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
        lines(density(x, bw = bandw, from = 0, to = max(x)), col = "red")
        legend("topleft", c("True Weibull density", "Estimated density"), lty = 1, col = c("green", "red"))
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
    if (returncor == TRUE){
      df <- data.frame("Lambda" = seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), "Correlation" = corvec)
      print(df)
    }
    plot(seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), corvec)
    if (plots == TRUE){
      a <- hist(x, freq = FALSE, breaks = max(x))
      hist(x, freq = FALSE, breaks = max(x), ylim = c(0, (max(a$density))*1.5))
      lines(dpois(0:max(x), seq(minshape, maxshape, by=(maxshape-minshape)/(steps1-1))[which.max(abs(corvec))]), col = "green")
      lines(density(x, bw = bandw, from = 0, to = max(x)), col = "red")
      legend("topleft", c("True Poisson density", "Estimated density"), lty = 1, col = c("green", "red"))
    }
    cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
    cat("The best lambda is", seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))[which.max(abs(corvec))])
  } else if (distribution == "normal"){
    est <- density(x, bw = bandw, n = length(x), from = min(x), to = max(x))$y
    j = 1
    corvec <- integer(steps1)
    for (i in seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))){
      nor <- dnorm(seq(min(x), max(x), length = length(x)), mean = mean(x), sd = i)
      corvec[j] <- cor(est, nor)
      j = j+1
    }
    plot(seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), corvec)
    if (returncor == TRUE){
      df <- data.frame("Standard Deviation" = seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), "Correlation" = corvec)
      print(df)
    }
    if (plots == TRUE){
      a <- hist(x, freq = FALSE, breaks = brks)
      hist(x, freq = FALSE, breaks = brks, ylim = c(0, (max(a$density))*1.5))
      curve(dnorm(x, mean = mean(x), sd = seq(minshape, maxshape, by=(maxshape-minshape)/(steps1-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
      lines(density(x, bw = bandw, from = min(x), to = max(x)), col = "red")
      legend("topleft", c("True Normal density", "Estimated density"), lty = 1, col = c("green", "red"))
    }
    cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
    cat("The best standard deviation is", seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))[which.max(abs(corvec))])
  } else if (distribution == "exponential"){
    j = 1
    est <- density(x, bw = bandw, n = length(x), from = 0, to = max(x))$y
    corvec <- integer(steps1)
    for (i in seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))){
      expo <- dexp(seq(0, max(x), length = length(x)), rate = i)
      corvec[j] <- cor(est, expo)
      j = j+1
    }
    plot(seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), corvec)
    if (returncor == TRUE){
      df <- data.frame("Lambda" = seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), "Correlation" = corvec)
      print(df)
    }
    if (plots == TRUE){
      a <- hist(x, freq = FALSE, breaks = brks)
      hist(x, freq = FALSE, breaks = brks, ylim = c(0, (max(a$density))*1.5))
      curve(dexp(x, seq(minshape, maxshape, by=(maxshape-minshape)/(steps1-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
      lines(density(x, bw = bandw, from = 0, to = max(x)), col = "red")
      legend("topleft", c("True Exponential density", "Estimated density"), lty = 1, col = c("green", "red"))
    }
    cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
    cat("The best lambda is", seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))[which.max(abs(corvec))])
  } else if (distribution == "lognormal"){
    x <- log(x)
    est <- density(x, bw = bandw, n = length(x), from = min(x), to = max(x))$y
    j = 1
    corvec <- integer(steps1)
    for (i in seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))){
      lnor <- dnorm(seq(min(x), max(x), length = length(x)), mean = mean(x), sd = i)
      corvec[j] <- cor(est, lnor)
      j = j+1
    }
    plot(seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), corvec)
    if (returncor == TRUE){
      df <- data.frame("Standard Deviation" = seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1)), "Correlation" = corvec)
      print(df)
    }
    if (plots == TRUE){
      a <- hist(x, freq = FALSE, breaks = brks)
      hist(x, freq = FALSE, breaks = brks, ylim = c(0, (max(a$density))*1.5))
      curve(dnorm(x, mean = mean(x), sd = seq(minshape, maxshape, by=(maxshape-minshape)/(steps1-1))[which.max(abs(corvec))]), col = "green", add = TRUE)
      lines(density(x, bw = bandw, from = min(x), to = max(x)), col = "red")
      legend("topleft", c("True Log-Normal density", "Estimated density"), lty = 1, col = c("green", "red"))
    }
    cat("The highest absolute correlation coefficient is", max(abs(corvec)), "\n")
    cat("The best standard deviation is", seq(minshape, maxshape, by = (maxshape-minshape)/(steps1-1))[which.max(abs(corvec))])
  }
}
