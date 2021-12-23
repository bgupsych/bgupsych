#' Confidence Interval for a known sd in population
#' @export
#' @param avg avg found in sample.
#' @param sd  SD in the population.
#' @param N sample size.
#' @param conf confidence level. default is 95.
#' @description Use this function when SD in population is known.
#' @seealso \code{\link{ci.t}} if SD in population is unknown.
#' @examples ci(620,15,100)
#' # is 620 the mean in population, 95% confidence
#' ci(avg=100,sd=20,N=50,conf=99)
#' # confidence is now at 99%
ci <- function(avg,sd,N,conf=95){
  sdmean <- sd/sqrt(N)
  Zscore <- qnorm((1-conf/100)/2,0,1)
  return(data.frame(sdmid=round(sdmean,2),"a/2"=round(-Zscore,2),
                Lower=round(avg+Zscore*sdmean,2),
                Upper=round(avg-Zscore*sdmean,2), "conf"=conf))
}
