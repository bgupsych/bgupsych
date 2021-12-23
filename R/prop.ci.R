#' Proportion Confidence Interval
#' @export
#' @param n is population size.
#' @param k is number of success.
#' @param conf is confidence level desired. default at 95.
#' @seealso \code{\link{prop.sig}} for a proportion significance test
#' @examples prop.ci(n=100,k=30)
#' prop.ci(100,30,conf=99)
#' @note  Calculates the binomal to normal assumption: \code{p&q * n > 5}.
#' @returns \code{p} Is the succses rate, given \code{n} and \code{k}.
#' @returns \code{q} Is the opposite probability of p (\code{1-p}).
#' @return \code{Lower} and \code{Upper} are the margins of the confidence interval.
#' @return \code{conf} is the confidence level.
#' @return \code{binom2norm} answers whether the assumption of normality is met.
prop.ci <- function(n,k,conf=95){
  p <- k/n
  q <- 1-p
  Zscore <- qnorm((1-conf/100)/2,0,1)
  prob <- sqrt((p*q)/n)
  norma <- p*n & q*n >5
  return(data.frame(p=round(p,2),q=round(q,2),Lower=round(p+Zscore*prob,3),
                Upper=round(p-Zscore*prob,3), "conf"=round(conf),
                "binom2norm"=ifelse(norma, "YES","NO")))
}
