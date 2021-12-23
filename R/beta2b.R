#' Convert Beta to b
#' @export
#' @description Converts the Beta value to a raw b, given the x and y
#' @param beta The beta value you want to convert.
#' @param x The x variable (Undependent).
#' @param y The y variable (Dependent).
#' @returns raw Mekadem b
#' @examples
#' model = lm(Bsum~sat+age,data=depression)
#' by using \code{\link{lm.beta}} we get sat~0.504, and age~0.467.
#' We want to convert them back to raw b:
#' sat: beta2b(beta=0.5038211, x=depression$sat, y=depression$Bsum)
#' ## 0.7899105
#' age: beta2b(0.4673743,depression$age, y=depression$Bsum)
#' ## 0.4072746
beta2b <- function(beta,x,y)(beta*sd(y)/sd(x))

