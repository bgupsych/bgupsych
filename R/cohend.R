#' Cohen's d effect size for one or two groups.
#' @export
#' @description Calculates effect size (cohen's d) for one or two samples.
#' @seealso This function makes use of \code{\link{cohen.d}} from \code{\link{effsize}} package.
#' @importFrom effsize cohen.d
#' @details Full details via the \code{\linkeffsize}} documantation. You can use either two vectors \code{x} and \code{y},
#' Or set \code{x} as a vector and \code{y} as a factor variable
#' @examples
#' x <- big5$Agreeableness[big5$gender=="Females"]
#' y <- big5$Agreeableness[big5$gender=="Males"]
#' cohend(x,y)
#' # We comapre Agreeableness level between both gender, using two seperate vectors.
#' cohend(x=big5[,1],y=big5$gender)
#' # We comapre the same variables, now y is a factoring varaible. Also used '[]' for shorter script.
#' cohend(big5$Agreeableness,y=NA,mu=1,conf=99)
#' # Only one vector compparison against Null hypothesis that mu equals 1, in 99% confidence level.
#' ## Note that you need to set y=NA if you dont want to use it, otherwise you'll get an Error.
#' @param x First vector.
#' @param y Second vector.
#' @param h0 Null hypothesis. default is 0
#' @param conf Desired confidence level. default is set to 95.
#' @param mu Use when comparing one sample. defaults is seto to 0.
#' @param g calls for the 'Hedge's g' correction. default is set to FALSE.
#' @source \url{https://www.rdocumentation.org/packages/effsize/versions/0.8.1/topics/cohen.d}

cohend <- function(x,y,conf=95,mu=0,g=F){
  effsize::cohen.d(x,y,hedges.correction=g,
  conf.level=conf/100,mu=mu,na.rm=T)
}
