#' Partial and Semi-partial correlations
#' @export
#' @description In the Partial correlation, the controlled variable is removed from both DV and UDV,
#' While in Semi-partial, controlled is removed only from the UDV, and so it is very much alike the Beta (direct effect).
#' @details Be sure to understand the theoretical meaning of semi and partial correlation.
#' In a nutshell, both correlate residuals created by a third variable.
#'  Partial correlation is the correlation of two
#'  variables while controlling a third.
#'   SP holds constant the controlled variable only for the UDV (predictor).
#' @seealso \code{\link{ppcor}} for more options.
#' @examples semi.partial.cor(DV=depression$Bsum,
#'  x.predictor=depression$m_years, x.controlled=depression$age)
#' @examples # We correlate 'Bsum' and 'm_years', holding 'age' constant. In SP it's been removed from only the predicting variable,
#' whilst in partial from both.
#' @note Partial will always be Greater than/Equal to Semi-partial.
#' @returns \code{data.frame} with the \code{Rsq} of partial and semi-partial, sample size and DF.

semi.partial.cor <- function(DV,x.predictor,x.controlled){
  partial <- cor(resid(lm(DV~x.controlled)),
                 resid(lm(x.predictor~x.controlled)))
  semi_partial <- cor(DV,resid(lm(x.predictor~x.controlled)))
  return(data.frame(semi_partial=semi_partial,partial=partial,
                    df=length(DV)-3,n=length(DV)))

}
