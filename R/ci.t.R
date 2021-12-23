#' Confidence Interval using a t distribution
#' @export
#' @param  avg Avg found in sample
#' @param sd SD in the sample
#' @param N Sample size
#' @param conf Confidence level for CI. default 95
#' @examples ci.t(avg=25,sd=12,N=30)
#' @examples ci.t(avg=25,sd=12,N=30,conf=99)
#' @description Calculates ci when SD in population is unkonwn
#' @seealso \code{\link{ci}} if SD in population is known
#' @examples ci.t(620,15,100)
#' # is 620 the mean in population, 95% confidence
#' ci.t(avg=100,sd=20,N=50,conf=99)
#' # confidence is now at 99%
#' @returns \code{data.frame} containing all values, so each  can be called with $.
#' @return \code{sd pop} An estimate for the SD in the population, based on the sample.
#' @return \code{SE} Standard Error.
#' @return \code{t value} The statistic.
#' @return \code{Upper Lower} Range of the ci.
#' @return \code{df} Degrees of freedom.
#' @return \code{conf} Confidence level.



ci.t <- function(avg,sd,N,conf=95){
  sdpop <- sqrt(N/(N-1))*sd
  SE <- sdpop/sqrt(N)
  t <- qt((1+conf/100)/2,N-1)
  return(data.frame("sd pop"=round(sdpop,4),"SE"=round(SE,4),
          "t value"=round(t,4),
 "df"=N-1,Lower=round(avg-t*SE,4),
         Upper=round(avg+t*SE,4),
        "conf"=conf))
}
