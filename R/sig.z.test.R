#' Two tailed Hypothesis test when SD is known
#' @export
#' @description The test is two tailed for when the SD in population is known.
#' # p value for two tails, default \code{alpha=5%}, \code{z= +-1.96}
#' @param  miu Avg/mean in population.
#' @param sd SD in the population.
#' @param N Sample size.
#' @param alpha Alpha decided. default 5.
#' @param x Mean found in sample.
#' @examples \code{\link{sig.z.test(620,80,25,588)}}
#' @examples \code{\link{sig.z.test(miu=620,sd=80,N=25,x=588,alpha=1)}}
#' @returns \code{H0} \code{rejected} or \code{accepted}.
#' @return \code{Z} the Z value.
#' @return \code{Z crit} Criteria according to the \code{alpha} provided.
#' @return \code{alpha} in both sides (takes the \code{alpha} input and divides by 2).
sig.z.test <- function(miu,sd,N,x,alpha=5){
  sdmid <- sd/sqrt(N)
  Z <- (x-miu)/sdmid
  return(data.frame("H0"=ifelse(Z< -qnorm((100-(alpha/2))/100)
                                | Z>qnorm((100-(alpha/2))/100),
                                "rejected", "accepted"),
                    "Z"=round(Z,3),
                    "Z crit"=round(qnorm((100-(alpha/2))/100),3),
                    "alpha"=alpha/100/2))
}

