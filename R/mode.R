#' Mode finder
#' @export
#' @description What value is the most frequent one?
#' @details In case of two Mo values, both will return.
#' @param x a vector containing numbers.
#' @examples x <- c(4,3,2,3)
#' mode(x)
#' @examples
#' y <- c(x,2)
#' mode(y)
#' @examples
#' # Set frequancy to TRUE:
#' mode(y,freq=TRUE)
#' @examples
#' # Set show.na to TRUE
#' z <- c(4,3,2,3,2,NA,NA)
#' mode(z,show.na=TRUE)
#' @examples
#' mode(z,T,T)
#' @param x A numeric vector
#' @param freq A logical argument. Adds the frequency of your mode, default is FALSE
#' @param  show.na A logical argument. Counts NA as a value.
#' @returns Most frequent value, and if freq is set to TRUE, how many times it appears (including NAs).
mode <- function(x,freq=F,show.na=F) {
  na=ifelse(show.na,"always","no")
  ans <- as.numeric(names(table(x,useNA = na))[table(x,useNA = na)==max(table(x,useNA = na))])
  frq=head(sort(table(x,useNA = na),decreasing = T),1)
  ifelse(freq,return(c(ans,freq=as.numeric(frq))),return(ans))
}
