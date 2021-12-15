#' Defaults mean() to na.rm=TRUE
#' @export
#' @description Simply changes the default argument to remove NA.
#' @examples x <- c(4,5,6,NA,6)
#' mean(x)
#' ## 5.25
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}
