#' Numerical moderation graph and summary
#' @export
#' @param DV The dependent variable.
#' @param x.pred The predictor variable.
#' @param x.mod The moderator variable.
#' @param sdH Leave untouched, unless you wish to generate a different SD moderation. \code{Positive sd mod}.
#' @param sdL Leave untouched, unless you wish to generate a different SD moderation.\code{Negative sd mod}.
#' @param sdM Leave untouched, unless you wish to generate a different SD moderation. \code{Mean of moderator}.
#' @description This function really does it all. It will generate both a ggplot graph,
#' slopes&intercepts and a summary!
#' @examples \code{\link{mitun(DV=trees$Girth,x.pred=trees$Height,x.mod=trees$Volume)}}
#' # This commands suggests that Girth is predicted by Height, and Volume has moderate affect on Height
#' @examples mitun(..., sdH=2,sdL=2)
#' # In this example, the graph will generate moderations of SD=+2, mean(sdM=0), and SD=-2.
#' # Note that you only mention the absolute value even for the smaller SD (without - sign)
#' @seealso \code{\link{jtools}} or \code{\link{interactions}} for more options.
#' @note  The moderator MUST be numeric.
#' @returns \code{\link{ggplot graph}}.
#' @returns \code{\link{lm() model summary}}.
#' @returns \code{\link{intercepts and slopes}} for each level of moderator.
#' @import ggplot2

mitun <- function(DV,x.pred,x.mod,sdH=1,sdM=0,sdL=1){
  x.pred.c <- scale(x.pred,T,F)
  x.mod.c <- scale(x.mod,T,F)
  moderate <- x.pred.c*x.mod.c
  m <- lm(DV~x.pred.c+x.mod.c+moderate)
  a <- coef(m)[1]
  b1 <- coef(m)[2]
  b2 <- coef(m)[3]
  b3 <- coef(m)[4]
  x.modH <- sd(x.mod.c)*sdH
  x.modM <- sd(x.mod.c)*sdM
  x.modL <- sd(x.mod.c)*-sdL
  mH <- b1+b3*x.modH
  mM <- b1
  mL <- b1+b3*x.modL
  interH <- a+b2*x.modH
  interM <- a
  interL <- a+b2*x.modL
  funH <- function(x.pred.c) interH+x.pred.c*mH
  funM <- function(x.pred.c) interM+x.pred.c*mM
  funL <- function(x.pred.c) interL+x.pred.c*mL
  library(ggplot2)
  data <- data.frame(x.pred.c,DV)
  SD = paste0("+",sdH, " SD")
  colL = paste0("-",sdL, " SD")
  g <- ggplot(data,aes(x.pred.c,DV))+geom_point()+labs(fill="SD")
  gg <- g+stat_function(fun = funH,aes(color=SD))+
    stat_function(fun=funM)+
    stat_function(fun=funL,aes(color=colL))
  print(summary(m))
  print(data.frame(SD=c(sdH,sdM,sdL),intercepts=c(interH,interM,interL),
                   slopes=c(mH,mM,mL)))
  return(gg)
}
