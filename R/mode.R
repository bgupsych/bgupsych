#' Mode finder
#' @export
#' @description What value is the most frequent one?
#' @details In case of two Mo values, both will return.
#' @param x a vector containing numbers.
#' @examples x <- c(11,14,13,15,13,16,14,13)
#' mode(x) ## 13
#' y <- c(x,14)
#' mode(y) ## 13, 14
mode <- function(x) {
  x <- as.numeric(names(table(x))[table(x)==max(table(x))])
  return(x)}
