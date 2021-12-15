#' Confidence Interval using a t distribution
#' @export
#' @param  avg Avg found in sample
#' @param sd SD in the sample
#' @param N Sample size
#' @param conf Confidence level for CI. default 95
#' @examples \code{\link{ci.t(avg=25,sd=12,N=30)}}
#' @examples \code{\link{ci.t(avg=25,sd=12,N=30,confidence=99)}}
#' @description Use this function when SD in population is unknown.
#' @seealso \code{\link{ci()}} if SD in population is known
#' @examples \code{\link{ci.t(620,15,100)}}
#' # is 620 the mean in population, 95% confidence
#' ci.t(avg=100,sd=20,N=50,conf=99)
#' # confidence is now at 99%


ci.t <- function(avg,sd,N,conf=95){
  sdpop <- sqrt(N/(N-1))*sd
  sdmid <- sdpop/sqrt(N)
  t <- qt((1+conf/100)/2,N-1)
  return(data.frame("sd pop"=round(sdpop,4),"sd midg"=round(sdmid,4),
          "t value"=round(t,4),
 "df"=round(N-1,4),Lower=round(avg-t*sdmid,4),
         Upper=round(avg+t*sdmid,4),
        "conf"=round(conf,4)))
}

