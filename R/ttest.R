#' Standard Error for difference in means when SD in population is unknown
#' @description Calculates the standard error given two numeric vectors (may differ in length)
#' @export
#' @param x A numeric vector.
#' @param y A second numeric vector (Independent).
#' @seealso \url{https://www.youtube.com/watch?v=Y-bI-3kBGTY} for further explenation.
#' @examples x <- c(17,4,58,96,0,12,14)
#' y <- c(11,47,62,0,96,12,47,5)
#' se(x,y)
#' @examples
#' (t=(mean(x)-mean(y))/se(x,y))
#' # This is the calculation to manually find t value.
#' \dontrun{t.test(x,y,var.equal=TRUE)}
se <- function(x,y){
  # The function also makes sure to ignore NA values and return a correct answer.
  mone=BGUPsych::sum((x-BGUPsych::mean(x))^2)+BGUPsych::sum((y-BGUPsych::mean(y))^2)
  mechane=length(x)+length(y)-2
  sealmost=sqrt(mone/mechane)
  return(se=sealmost*sqrt(1/length(x)+1/length(y)))
}

#' T value for difference in independent means when SD in population is unknown
#' @description Calculates the t value and SE, given two numeric vectors
#' @export
#' @seealso \url{https://www.youtube.com/watch?v=3azuAaOJack} for further explenation.
#' @param x A numeric vector.
#' @param y A second numeric vector (Independent).
#' @param print A logical,whether to print the usual t.test print. default is set to FALSE.
#' @returns \code{t value} The t value.
#' @returns \code{se} Standard Error.
#' @returns \code{df} Degrees of freedom.
#' @examples x <- c(17,4,58,96,0,12,14)
#' y <- c(11,47,62,0,96,12,47,5)
#' tvalue(x,y)
tvalue <- function(x,y,print=FALSE){
  if(print){print(t.test(x,y,var.equal=T))}
  return(data.frame(t=(BGUPsych::mean(x)-BGUPsych::mean(y))/BGUPsych::se(x,y),
se=BGUPsych::se(x,y),df=length(x)-2+length(y)))
}
