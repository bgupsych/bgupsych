#' Defaults range() to na.rm=TRUE
#' @export
#' @description Simply changes the default argument to remove NA.
#' @examples x <- c(4,5,6,NA,6)
#' range(x)
#' ## 4 6

range <- function(..., na.rm = TRUE) {
  base::range(..., na.rm = na.rm)
}

