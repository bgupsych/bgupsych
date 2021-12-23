#' Defaults various functions to na.rm=T
#' @description Simply changes the default argument to remove NA.
#' @export
range <- function(..., na.rm = TRUE) {
  base::range(..., na.rm = na.rm)
}
#' Defaults various functions to na.rm=T
#' @description Simply changes the default argument to remove NA.
#' @export
sd <- function(..., na.rm = TRUE) {
    stats::sd(..., na.rm = na.rm)
}
#' Defaults various functions to na.rm=T
#' @description Simply changes the default argument to remove NA.
#' @export
var <- function(..., na.rm = TRUE) {
  stats::var(..., na.rm = na.rm)
}
#' Defaults various functions to na.rm=T
#' @description Simply changes the default argument to remove NA.
#' @export
sum <- function(x, ..., na.rm = TRUE) {
base::sum(x, ..., na.rm = na.rm)
}
#' Defaults various functions to na.rm=T
#' @description Simply changes the default argument to remove NA.
#' @export
median <- function(x, ..., na.rm = TRUE) {
  stats::median(x, ..., na.rm = na.rm)
}
#' Defaults various functions to na.rm=T
#' @description Simply changes the default argument to remove NA.
#' @export
mean <- function(x, ..., na.rm = TRUE) {
  base::mean(x, ..., na.rm = na.rm)
}

