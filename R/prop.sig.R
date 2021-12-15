#' Proportion Hypothesis Test
#' @export
#' @param p Percents of success in population.
#' @param N Sample size.
#' @param x Percents of success in sample.
#' @description This function returns the Z value of the sampled proportion. Input values refer to percents.
#' @examples \code{\link{prop.sig(p=46,N=36,x=38)}}
#' # 46% success in pop, while in a 36 people sample, 38% success.
#' @seealso \code{\link{prop.ci()}} for proportion confidence interval.
#' @note  Calculates the binomal to normal assumption: \code{p&q * n > 5}
#' @returns \code{p} is the success rate in population.
#' @return \code{q} is the opposite probability of p (\code{1-p}).
#' @return \code{sd} is the SD estimation.
#' @return \code{Z} is the Z value, genreated given the data.
#' @return \code{binom2norm} answers whether the assumption of normality is met.

prop.sig <- function(p,N,x){
  p <- p/100
  q <- 1-p
  x <- x/100
  sd_pop <- sqrt((p*q)/N)
  Z <- (x-p)/sd_pop
  norma <- p*N & q*N >5
  return(data.frame(p=round(p,3), q=round(q,3),
 sd=round(sd_pop,3),Z=round(Z,3),
"binomal2normal"=ifelse(norma, "YES","NO")))

}
