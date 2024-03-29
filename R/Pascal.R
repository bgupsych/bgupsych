#' Pascal Triangle generator
#' @description This function generates the levels of the pascal triangle.
#' It is highly linked with the \code{\link{choose}} function.
#' @export
#' @importFrom purrr accumulate
#' @seealso \url{https://www.mathsisfun.com/pascals-triangle.html}
#' @source This function makes use of \code{\link{accumulate}} from the \code{\link{purrr}} package.
#'
#'
#' \url{https://purrr.tidyverse.org/reference/index.html}
#' @examples Pascal(4)
#' # if you count k indexes (after 1) in row n (after 1st), you get the result of choost(n,k).
#' ## in [[5]], which is row n=4, k=2 index is 6. this is the binomal result of choose(4,2).
#' read more in the URL provided.
#' @returns all levels up to level 30 of the triangle (or pyramid).

Pascal <- function(nrow){
  require(purrr)
  purrr::accumulate(1:nrow,~c(0,.)+c(.,0),.init = 1)
}
