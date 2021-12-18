#' Degrees to Radians
#' @export
#' @description transforms degrees to radians
#' @examples deg.to.rad(360)
#' # returns 6.283185, which indeed equals to '2*pi'
deg.to.rad <- function(deg){
  rad=(deg/360)*2*pi
  return(rad)
}
# example deg.to.rad(180)
