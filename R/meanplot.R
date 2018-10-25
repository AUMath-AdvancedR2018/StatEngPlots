# Mean Plot
#' @export
meanplot <- function(x, nr = 0, divide = TRUE, divided = TRUE, return = FALSE){
  if (divide == FALSE){
    if (nr != 0){
      if (class(x) == "list"){
        xunlist <- unlist(x)
        meanvec <- integer(length(xunlist)-(nr-1))
        for (i in 1:(length(xunlist)-(nr-1))){
          meanvec[i] <- mean(xunlist[i:((nr-1)+i)])
        }
        if(return == TRUE){
          df <- data.frame("Part" = c(1:(length(xunlist)-(nr-1))), "Mean" = meanvec)
          print(df)
        }
      } else if (class(x) == "integer" || class(x) == "numeric"){
        meanvec <- integer(length(x)-(nr-1))
        for (i in 1:(length(x)-(nr-1))){
          meanvec[i] <- mean(x[i:((nr-1)+i)])
        }
        if(return == TRUE){
          df <- data.frame("Part" = c(1:(length(x)-(nr-1))), "Mean" = meanvec)
          print(df)
        }
      } else if (class(x) == "data.frame"){
        xunlist <- as.integer(unlist(x))
        meanvec <- integer(length(xunlist)-(nr-1))
        for (i in 1:(length(xunlist)-(nr-1))){
          meanvec[i] <- mean(xunlist[i:((nr-1)+i)])
        }
        if(return == TRUE){
          df <- data.frame("Part" = c(1:(length(xunlist)-(nr-1))), "Mean" = meanvec)
          print(df)
        }
      } else {
        cat("Error. Your data has the wrong format. Please transform your data into a list, data frame or vector.")
      }

    } else {
      cat("Error. The number of elements which has to be evaluated at the same time (nr) is 0. Please define nr to be bigger than 0 and smaller than length(x).")
     }
    med <- median(meanvec)
    plot(meanvec, type = "b", ylab = "Mean", xlab = "Part", col = "blue")
    abline(h = med)
    legend("topleft", c("Mean", "Median"), lty = 4, col = c("blue", "red"))
  } else {

    if (divided == TRUE){
      meanvec <- integer(length(x))
      if (class(x) == "list"){
        for(i in 1:length(x)){
          meanvec[i] <- mean(x[[i]])
        }
        med <- median(meanvec)
        plot(meanvec, type = "b", ylab = "Mean", xlab = "Part", col = "blue")
        abline(h = med)
        legend("topleft", c("Mean", "Median"), lty = 4, col = c("blue", "red"))
      } else if (class(x) == "data.frame"){
        for(i in 1:length(x)){
          meanvec[i] <- colMeans(x[,i,drop =FALSE], na.rm = TRUE)


        }
        med <- median(meanvec)
        plot(meanvec, type = "b", ylab = "Mean", xlab = "Part", col = "blue")
        abline(h = med)
        legend("topleft", c("Mean", "Median"), lty = 4, col = c("blue", "red"))
      } else {
        cat("Error. Your data has the wrong format. Please transform your data into a list or data frame.")
      }
      if(return == TRUE){
        df <- data.frame("Part" = c(1:length(x)), "Mean" = meanvec)
        print(df)
      }
    } else {
      if (nr != 0){
        x <- x[!is.na(x)]
        chunk <- function(y, z) split(y, factor(seq_along(y)%%z))
        xvec <- chunk(x,nr)
        meanvec <- integer(nr)
        for(i in 1:nr){
          meanvec[i] <- mean(xvec[[i]])
        }
        if(return == TRUE){
          df <- data.frame("Part" = c(1:nr), "Mean" = meanvec)
          print(df)
        }
        med <- median(meanvec)
        plot(meanvec, type = "b", ylab = "Mean", xlab = "Part", col = "blue")
        abline(h = med)
        legend("topleft", c("Mean", "Median"), lty = 4, col = c("blue", "red"))
      } else {
        cat("Error. No number of partitions (nr) defined. You need to define the number of partitions to be bigger than 0 and smaller than length(x).")
             }
    }

  }
}

