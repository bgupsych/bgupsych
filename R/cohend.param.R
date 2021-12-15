#' Cohen's d effect size for two groups
#' @export
#' @description Calculates effect size (Cohen's d) with given parameters.
#' @param mu1 Avg of 1st group.
#' @param mu2 Avg of 2nd group.
#' @param s1 Variance of 1st group (NOT SD).
#' @param s2 Variance of 2nd group (NOT SD).
#' @param N1 Sample size of 1st group.
#' @param N2 Sample size of 2nd group. default is N2=N1.
#' @param h0 Null hypothesis. default is 0.
#' @seealso If you have vectors, use \code{\link{cohend.vec}}
#' @examples \code{\link{cohend.param(mu1=835,mu2=831,s1=8.5,s2=12,N1=11)}}

cohend.param <- function(mu1,mu2,s1,s2,N1, N2=N1, h0=0){
  sdd <- sqrt((s1*N1+s2*N2)/(N1+N2-2))
  h1 <- mu1-mu2
  return(c("Cohen's d"=(h1-h0)/sdd))
}

