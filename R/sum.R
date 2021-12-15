#' Defaults sum() to na.rm=TRUE
#' @export
#' @description Simply changes the default argument to remove NA.
#' @examples x <- c(4,5,6,NA,6)
#' sum(x)
#' ## 21

sum <- function(x, ..., na.rm = TRUE) {
  base::sum(x, ..., na.rm = na.rm)
}


