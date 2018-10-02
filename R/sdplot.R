# Standard Deviation Plot
sdplot <- function(x, nr = 0, divide = TRUE, divided = TRUE, return = FALSE){
  if (divide == TRUE){
    if (divided == FALSE){
      if (nr == 0){
        cat("Error. No number of partitions (nr) defined. You need to define the number of partitions to be bigger than 0 and smaller than length(x).")
      } else {
        chunk <- function(y, z) split(y, factor(sort(rank(y)%%z)))
        xnew <- chunk(x,nr)
        sdvec <- integer(nr)
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
  } else {
    if (nr == 0){
      cat("Error. The number of elements which has to be evaluated at the same time (nr) is 0. Please define nr to be bigger than 0 and smaller than length(x).")
    } else {
      if (class(x) == "integer" || class(x) == "numeric"){
        sdvec <- integer(length(x)-(nr-1))
        for (i in 1:(length(x)-(nr-1))){
          sdvec[i] <- sd(x[i:((nr-1)+i)])
        }
        if(return == TRUE){
          df <- data.frame("Part" = c(1:(length(x)-(nr-1))), "Standard Deviation" = sdvec)
          print(df)
        }
      } else if (class(x) == "list"){
        xunlist <- unlist(x)
        sdvec <- integer(length(xunlist)-(nr-1))
        for (i in 1:(length(xunlist)-(nr-1))){
          sdvec[i] <- sd(xunlist[i:((nr-1)+i)])
        }
        if(return == TRUE){
          df <- data.frame("Part" = c(1:(length(xunlist)-(nr-1))), "Standard Deviation" = sdvec)
          print(df)
        }
      } else if (class(x) == "data.frame"){
        xunlist <- as.integer(unlist(x))
        sdvec <- integer(length(xunlist)-(nr-1))
        for (i in 1:(length(xunlist)-(nr-1))){
          sdvec[i] <- sd(xunlist[i:((nr-1)+i)])
        }
        if(return == TRUE){
          df <- data.frame("Part" = c(1:(length(xunlist)-(nr-1))), "Standard Deviation" = sdvec)
          print(df)
        }
      } else {
        cat("Error. Your data has the wrong format. Please transform your data into a list, data frame or vector.")
      }
    }
    m <- median(sdvec)
    plot(sdvec, type = "b", ylab = "Standard Deviation", xlab = "Part", col = "red")
    abline(h = m)
    legend("topleft", c("Standard Deviation", "Median"), lty = 4, col = c("red", "black"))
  }
}
