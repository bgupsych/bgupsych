#' Mode finder
#' @export
#' @description What value is the most frequent one?
#' @details In case of two Mo values, both will return.
#' @param x a vector containing numbers.
#' @examples x <- c(11,14,13,15,13,16,14,13)
#' mode(x) ## 13
#' y <- c(x,14)
#' mode(y) ## 13, 14
#' mode(y,freq=TRUE)
#' @param x A numeric vector
#' @param freq A logical argument. Adds the frequency of your mode, default is FALSE
#' @returns Most frequent value, and if freq is set to TRUE, a how many times it appears
mode <- function(x,freq=F) {
  ans <- as.numeric(names(table(x))[table(x)==max(table(x))])
  frq=head(sort(table(x),decreasing = T),1)
  ifelse(freq,return(c(ans,freq=as.numeric(frq))),return(ans))
}
