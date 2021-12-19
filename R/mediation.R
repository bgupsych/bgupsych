#' Mediation between two variables.
#' @export
#' @param DV The dependent variable.
#' @param x.pred The predictor variable.
#' @param x.med The mediation variable.
#' @param alpha Used in significant tests. default is 5 and means 0.05 .
#' @description This function calculates the direct and indirect effect of two variables,
#'  mediated by a 3rd variable.
#' @details Output is a table of your 4 paths, their value and p value.
#' 3 outputs are printed to ensure the results.
#' @examples mediation(DV=trees$Girth,x.pred=trees$Height,x.med=trees$Volume)
#' # In this example, we check whether volume is the mediator between Height and Girth.
#' @examples mediation(..., alpha =1)
#' # In this example, the alpha is set to 1, or 0.01.
#' @seealso \code{\link{medmod}} package for more options.
#' @note  If some paths are significant and some aren't, check table for more details.
#' @returns [1] \code{\link{a*b+c'=c}} formula is tested.
#' @returns [2] Paths's significance.
#' @returns [3] Is there a direct effect of \code{\link{c'}}.
#' @returns [4] \code{\link{data.frame}} with the paths (values and p value).


mediation <- function(DV,x.pred,x.med,alpha=5){
  al <- alpha/100
  x.model <- summary(lm(DV~x.pred))
  c <- coef(x.model)[2,c(1,4)]
  m.model <- summary(lm(x.med~x.pred))
  a <- coef(m.model)[2,c(1,4)]
  full.model <- summary(lm(DV~x.med+x.pred))
  b <- coef(full.model)[2,c(1,4)]
  c.tag <- coef(full.model)[3,c(1,4)]
  test <- round(c.tag[1]+a[1]*b[1],5)==round(c[1],5)
  sig <- unique(c(c[2]<al,a[2]<al,
                  b[2]<al,c.tag[2]<al))
  paths <- data.frame(
    a=round(a,4),b=round(b,4),
    "c.tag"=round(c.tag,4),c=round(c,4))
  rownames(paths) <- c("Mekadem","P VALUE")
  ifelse(test,print("a*b+c'=c is correct"),
         "a*b+c'=c went wrong")
  print(ifelse(sig,"all paths are significant",
               "NOT all paths are significant, see table"))

  print(ifelse(as.numeric(round(c.tag[1],3))==0 |
                 as.numeric(c.tag[2])>al,
               "c tag is close to zero, or isn't significant",
               "There's a direct and significant effect"))

  return(paths)
}
