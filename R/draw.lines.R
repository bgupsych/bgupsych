#' Draw cool looking lines
#' @export
#' @param n The total number of lines to be drawn.
#' @param slope_range A vector of length two, or a single value, setting the range of slopes desired.
#' @param intercepts A vector of slopes (unlimited length).
#' @param alpha_range A vector of length two,0-1, or a single value, setting the transparency (alpha) range desired.
#' @param line_color A string of colors desired. Leave untouched to use all system \code{\link{colors}}.
#' @param line_sizes A vector of line size's desired, or a single value for a fixed size.
#' @param line_types Integers 0-6, single or vectorized input.
#' @description Just try different values of numbers and see the different outcome. Use the example to be inspired.
#' @examples draw.lines()
#' @examples draw.lines(line_sizes = c(0.2,1,1.5,3,4,5), # different lines sizes
#' slope_range = c(-2,1), # slopes ranges from -2 to 1
#' intercepts = c(0,1,2,3,.5), # various intercepts
#' alpha_range = c(0.2,.8)) # alpha ranges from 0.2 to 0.8 (by fixed jumps of 0.15)
#' @examples for(. in 1:4){
#'           x11()
#'           print(draw.lines())}

draw.lines <- function(n=100,slope_range=c(0,1),
                       intercepts=0,alpha_range=c(0,0.4),
                       line_color=colors(),line_sizes=c(0.5,1),
                       line_types=1){
  require(ggplot2)
  line_sizes <- abs(line_sizes)
  if(length(slope_range)){slope_range <- c(slope_range,slope_range)}
  if(length(alpha_range)){alpha_range <- c(alpha_range,alpha_range)}

  ggplot()+
    geom_abline(slope=seq(slope_range[1],slope_range[2],length=n),
                intercept = sample(intercepts,n,T),
                alpha=sample(seq(alpha_range[1],alpha_range[2],by=0.15),n,T),
                color=sample(line_color,n,T),
                size=sample(line_sizes,n,T),
                linetype=sample(line_types,n,T)


    )
}
