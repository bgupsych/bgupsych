#' Homoscedasticity visualization
#' @description A nice visualization to asses whether a homoscedasticity is present
#' @export
#' @param lm_object A linear model \code{\link{lm}}.
#' @param res Logical to detremine whether to use raw DV, or set the y scale to the residuals. default is set to TRUE (show residualized y axis).
#' @param transparence Controls the transparency of the points. Default as 0.8.
#' @returns \code{\link{summary}} of the lm object.
#' @returns  \code{graph} (print only).
#' @note Though you can use a multi-regression model, the X axis will show the first predictor only.
#' @seealso \url{https://www.youtube.com/watch?v=zRklTsY9w9c}
#' @examples model <- lm(Bsum~m_years,depression)
#' HOMO(model)
#' # Checking whether the residuals are equally distributed along the line, residualized y axis.
#' @examples HOMO(model,res=FALSE,transparence=.1)
#' ## Raw y axis with the regression line.
HOMO <- function(lm_object,res=T,transparence=0.8){

  transparence <- abs(transparence)
  Predictor <- lm_object$model[,2]
  if(res){DV=resid(lm_object)}
  else{DV=lm_object$model[,1]}
  m=coef(lm_object)
  if(res){s=0}else{s=m[2]}
  if(res){a=0}else{a=m[1]}
  g=ggplot2::ggplot(lm_object$model,aes(x=Predictor,y=DV))+
    geom_point(aes(x=Predictor,y=DV,color=as.factor(Predictor)),alpha=transparence)+
    geom_abline(slope = s,intercept = a,size=transparence*1.8+.05,color="navy")+
    theme(legend.position = "none")
  print(g)
  return(summary(lm_object))
}
