# Standard Deviation plot
sdplot <- function(x, nr = 0, divided = TRUE, return = FALSE){
  if (divided == FALSE){
    if (nr == 0){
      cat("Error. No number of partitions (nr) defined. You need to define the number of partitions to be bigger than 0.")
    } else {
      xnew <- list()
      sdvec <- integer(nr)
      l <- length(x)
      for (i in 1:nr){
        xnew[[i]] <- x[1:round((l/nr))]
        x <- x[-(1:round((l/nr)))]
      }
      xnew[[length(xnew)]] <- as.numeric(na.omit(xnew[[length(xnew)]]))
      for(i in 1:nr){
        sdvec[i] <- sd(xnew[[i]])
      }
      if(return == TRUE){
        df <- data.frame("Part" = c(1:nr), "Standard Deviation" = sdvec)
        print(df)
      }
      m <- median(sdvec)
      plot(sdvec, type = "b", ylab = "Standard Deviation", xlab = "Part", col = "red")
      abline(h = m)
      legend("topleft", c("Standard Deviation", "Median"), lty = 4, col = c("red", "black"))
    }
  } else {
    sdvec <- integer(length(x))
    if (class(x) == "list"){
      for(i in 1:length(x)){
        sdvec[i] <- sd(x[[i]])
      }
      m <- median(sdvec)
      plot(sdvec, type = "b", ylab = "Standard Deviation", xlab = "Part", col = "red")
      abline(h = m)
      legend("topleft", c("Standard Deviation", "Median"), lty = 4, col = c("red", "black"))
    } else if (class(x) == "data.frame"){
      for(i in 1:length(x)){
        sdvec[i] <- sd(x[,i])
      }
      m <- median(sdvec)
      plot(sdvec, type = "b", ylab = "Standard Deviation", xlab = "Part", col = "red")
      abline(h = m)
      legend("topleft", c("Standard Deviation", "Median"), lty = 4, col = c("red", "black"))
    } else {
      cat("Error. Your data has the wrong format. Please transform your data into a list or data frame.")
    }
    if(return == TRUE){
      df <- data.frame("Part" = c(1:length(x)), "Standard Deviation" = sdvec)
      print(df)
    }
  }
}
