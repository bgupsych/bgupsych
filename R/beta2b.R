#' Convert Beta to b
#' @export
#' @description Converts the Beta value to a raw b, given the x and y
#' @param beta The beta value you want to convert.
#' @param x The x variable (Undependent).
#' @param y The y variable (Dependent).
#' @returns raw Mekadem b
#' @examples beta2b(beta=1.45, x=trees$Height, y=trees$Volume)
#' ## 3.740674
#' ## For every increase of 1 unit in Height, Volume increases by 3.740674 units.
beta2b <- function(beta,x,y)(beta*sd(y)/sd(x))


