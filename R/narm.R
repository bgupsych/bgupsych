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
