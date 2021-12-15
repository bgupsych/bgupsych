#' Fibonacci sequence generator
#' @description This function calculates the fibonacci sequence and returns an approximation of the golden ratio.
#' @export
#' @param x The number of numbers in the sequence.
#' @param decimals Decimals to print of the golden ratio, default is 9.
#' @note R can't print more than 9 decimals, so leave at default for max accuracy.
#' The golden ratio is 1.61803398874989484820...
#' @examples fibonacci(20)
#' # returns the sequence, up to the 20th number
#' fibonacci(x=20,decimals=4)
#' # returns up to the 20th number, golden ratio is rounded to 4 decimal digits

fibonacci <- function(x,decimals=9){
  a <- c(0,1)
  gold <- c(0,1)
  x <- x-2
  for (i in 1:x) {
    a[i+2] <- sum(a[i],a[i+1])
    gold[i+2] <- round(a[i+1]/a[i],decimals)
  }
  return(data.frame(Fibonacci=a,"Golden Ratio"=gold))
}


