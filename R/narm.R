#' Range with na.rm=TRUE
#' @description Simply changes the default argument to remove NA.
#' @export
#' @examples x <- c(1,2,3,4,5,NA)
#' range(x)
range <- function(..., na.rm = TRUE) {
  base::range(..., na.rm = na.rm)
}
#' SD with na.rm=TRUE
#' @description Simply changes the default argument to remove NA.
#' @export
#' @examples x <- c(1,2,3,4,5,NA)
#' sd(x)
sd <- function(..., na.rm = TRUE) {
    stats::sd(..., na.rm = na.rm)
}
#' Var with na.rm=TRUE
#' @description Simply changes the default argument to remove NA.
#' @export
#' @examples x <- c(1,2,3,4,5,NA)
#' var(x)
var <- function(..., na.rm = TRUE) {
  stats::var(..., na.rm = na.rm)
}
#' Sum with na.rm=TRUE
#' @description Simply changes the default argument to remove NA.
#' @export
#' @examples x <- c(1,2,3,4,5,NA)
#' sum(x)
sum <- function(x, ..., na.rm = TRUE) {
base::sum(x, ..., na.rm = na.rm)
}
#' Median with na.rm=TRUE
#' @description Simply changes the default argument to remove NA.
#' @export
#' @examples x <- c(1,2,3,4,5,NA)
#' median(x)
median <- function(x, ..., na.rm = TRUE) {
  stats::median(x, ..., na.rm = na.rm)
}
#' Mean with na.rm=TRUE
#' @description Simply changes the default argument to remove NA.
#' @export
#' @examples x <- c(1,2,3,4,5,NA)
#' mean(x)
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
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
#' t.value(x,y)
t.value <- function(x,y,print=FALSE){
  if(print){print(t.test(x,y,var.equal=T))}
  return(data.frame(t=(BGUPsych::mean(x)-BGUPsych::mean(y))/BGUPsych::se(x,y),
                    se=BGUPsych::se(x,y),df=length(x)-2+length(y)))
}
