#' Generates the SST/Res/Reg, Rsq and Adj Rsq of lm model.
#' @export
#' @description Make sure to create your \code{lm} object first. Put your object in the function and see how magic happens.
#' @examples x <- lm(Happiness.Score~.,Happiness.Score)
#' ssmodel(x)
#' # This model predicts Happiness score by all other variables in the data frame.
#'
#' y <- lm(Bsum~age+sat,data=depression)
#' ssmodel(y)
#' # This model only uses age and sat to predict Bsum
#' @param model lm() object
#' @seealso What happens if you use the mean in your model as prediction?
#'
#' \code{Mean <- rep(mean(depression$Bsum),nrow(depression))}
#' # a repetition of the Bsum's mean, times the number of observations in 'depression'
#' \code{ssmodel(lm(Bsum~Mean,depression))}
#
#' # The mean value is used to predict Bsum. See for yourself that SSRes=SST and SSReg=0.

ssmodel <- function(model){
  SST = round(var(model$model[,1])*
                (length(model$model[,1])-1),3)
  SSRes = round(sum(resid(model)^2),3)
  SSReg = round(SST-SSRes,3)
  Rsq=round(SSReg/SST,4)
  lm_m <- summary(model)
  adrsq=round(lm_m$adj.r.squared,4)
  pred=names(model$coefficients[-1])
  return(data.frame(SST=SST,SSRes=SSRes,SSReg=SSReg,
           Rsq=Rsq,"Adj.Rsq"=adrsq,Predictor=pred))
}
