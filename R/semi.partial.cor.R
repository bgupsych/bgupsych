#' Partial and Semi-partial correlations
#' @export
#' @import ppcor
#' @description In the Partial correlation, the controlled variable is removed from both DV and UDV,
#' While in Semi-partial, controlled is removed only from the UDV, and so it is very much alike the Beta (direct effect).
#' @details Be sure to understand the theoretical meaning of semi and partial correlation.
#' In a nutshell, both correlate residuals created by a third variable.
#'  Partial correlation is the correlation of two
#'  variables while controlling a third.
#'   SP holds constant the controlled variable only for the UDV (predictor).
#'   Read more here \url{https://www.youtube.com/watch?v=OpAf4N582bA}
#' @seealso This function runs \code{\link{ppcor}}, check it out for more options.
#' @examples \dontshow{attach(depression)}
#' semi.partial.cor(DV=Bsum,x.predictor=m_years, x.controlled=age)
#' # We correlate 'Bsum' and 'm_years', holding 'age' constant. In SP it's been removed from only the predicting variable,
#' # whilst in partial from both.
#' \dontshow{detach(depression)}
#' @note Partial will always be Greater than/Equal to Semi-partial.
#' @returns \code{data.frame} with the \code{estimate} for both semi-partial & partial correlations, and its \code{p-value}.

semi.partial.cor <- function(DV,x.predictor,x.controlled){
  require(ppcor)
  x=DV
  y=x.predictor
  z=x.controlled
  p=ppcor::pcor.test(x,y,z)
  sp=ppcor::spcor.test(x,y,z)
  psp=data.frame(type=c("partial","semipartial"),
                 estimate=c(as.numeric(p[1]),as.numeric(sp[1])),
                 p.value=c(as.numeric(p[2]),as.numeric(sp[2])))
  return(psp)
}
