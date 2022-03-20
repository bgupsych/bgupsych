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
#' @returns \code{meandiff} The mean difference between the groups.
#' @returns \code{t value} The t value.
#' @returns \code{SE} Standard Error.
#' @returns \code{df} Degrees of freedom.
#' @returns \code{p.value} The P Value of the result.
#' @examples x <- c(17,4,58,96,0,12,14)
#' y <- c(11,47,62,0,96,12,47,5)
#' tvalue(x,y,print=TRUE)
tvalue <- function (x, y, print = FALSE) {
  printT <- t.test(x, y, var.equal = T)

  if (print) {
    print(printT)
  }
  return(data.frame(meandiff=as.numeric(printT$estimate[1]-printT$estimate[2]),
                    t = (BGUPsych::mean(x) - BGUPsych::mean(y))/BGUPsych::se(x, y),
                    SE = BGUPsych::se(x, y),
                    df = length(x) - 2 + length(y),
                    p.value=printT$p.value))
}
