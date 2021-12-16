#' Generates the SST/Res/Reg, Rsq and Adj Rsq of lm model.
#' @export
#' @description Make sure to create your \code{lm} object first. Put your object in the function and see how magic happens.
#' @examples x <- lm(Volume~.,trees)
#' ssmodel(x)
#' @seealso What happens if you use the mean in your model as prediction?
#' \code{ssmodel(lm(trees$Volume~rep(mean(trees$Volume),nrow(trees))))}
#' # This is a hard-to-read input that contains a repetition vector of the mean value of Volume for as many times as the  number of rows in the data,
#' # then these values (actually, constant) are used to predict Volume. See for yourself that SSRes=SST and SSReg=0.

ssmodel <- function(model){
  SST = round(var(model$model[,1])*
                (length(model$model[,1])-1),3)
  SSRes = round(sum(resid(model)^2),3)
  SSReg = round(SST-SSRes,3)
  Rsq=round(SSReg/SST,4)
  lm_m <- summary(model)
  adrsq=round(lm_m$adj.r.squared,4)
  pred=names(model$coefficients[-1])
  return(c(SST=SST,SSRes=SSRes,SSReg=SSReg,
           Rsq=Rsq,"Adj.Rsq"=adrsq,Predictor=pred))
}
