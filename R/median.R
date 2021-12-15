#' Defaults median() to na.rm=TRUE
#' @export
#' @description Simply changes the default argument to remove NA.
#' @examples x <- c(4,5,6,NA,6)
#' median(x)
#' ## 5.5
median <- function(x, ..., na.rm = TRUE) {
  stats::median(x, ..., na.rm = na.rm)
}
