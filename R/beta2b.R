#' Convert Beta to b
#' @export
#' @description Converts the Beta value to a raw b, given the x and y
#' @param beta The beta value you want to convert.
#' @param x The x variable (Independent).
#' @param y The y variable (Dependent).
#' @details You can either give as an input the full variable (a vector of length > 2) or the standard deviation.
#' @returns raw Mekadem b
#' @examples
#' model = lm(Bsum~sat+age,data=depression)
#' by using \code{\link{lm.beta}} we get sat~0.504, and age~0.467.
#' We want to convert them back to raw b:
#' sat: beta2b(beta=0.5038211, x=depression$sat, y=depression$Bsum)
#' ## 0.7899105
#' age: beta2b(0.4673743,sd(depression$age), y=sd(depression$Bsum))
#' ## 0.4072746

beta2b <- function (beta, x, y){
  res <- beta * sd(y)/sd(x)
  if(length(x)==1){
    res = beta * y/x
  }
  return(res)
}
