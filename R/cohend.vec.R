#' Cohen's d effect size for two groups
#' @export
#' @description Calculates effect size (Cohen's d) with given vectors.
#' @seealso If you have parameters, use \code{\link{cohend.param}}.
#' @examples
#' x <- c(14,17,19,22)
#' y <- c(15,19,20,12)
#' \code{\link{cohend.vec(x,y)}}
#' # in this example, Null hypothesis is that reall diff is 0
#' \code{\link{cohend.vec(x,y,h0=2)}}
#' # Here, diff in pop (H0) is 2
#' @param x First vector.
#' @param y Second vector.
#' @param h0 Null hypothesis. default is 0

cohend.vec <- function(x,y,h0=0){
  mu1 <- mean(x)
  mu2 <- mean(y)
  s1 <- sd(x)*(length(x)-1)
  s2 <- sd(y)*(length(y)-1)
  N1 <- length(x)
  N2 <- length(y)
  sdd <- sqrt((s1*N1+s2*N2)/(N1+N2-2))
  h1 <- mu1-mu2
  return(data.frame("Cohen d"=(h1-h0)/sdd, h1=h1, h0=h0))
}

