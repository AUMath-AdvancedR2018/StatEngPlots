# Hint function. IMPORTANT FOR THE GROUP THAT HAS TO LOOK AT OUR PACKAGE: Read the very beginning of the
# vignette before looking at this function.
# This function is not relevant for the rest of the package.
hint <- function(x = NULL){
  if (x == 1){
    cat("The wrong part(s) are in the ppcc part.")
  } else if (x == 2){
    cat("We are looking for shape parameters, right?")
  } else if (x == 3){
    cat("Try to use the following code:", "\n", "a <- sort(rexp(1000, 5))", "\n", "summary(replicate(1000, cor(a, sort(rexp(1000, 1)))))", "\n", "Evaluate the result.")
  } else if (x == 4){
    cat("Exponential and Normal distribution.")
  } else {
    cat("Error. Please select a hint (x) from 1 to 4.")
  }
}
