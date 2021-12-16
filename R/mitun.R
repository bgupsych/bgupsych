#' Numerical moderation graph and summary
#' @export
#' @param DV The dependent variable.
#' @param x.pred The predictor variable.
#' @param x.mod The moderator variable.
#' @param sdH Leave untouched, unless you wish to generate a different SD moderation. \code{Positive sd mod}.
#' @param sdL Leave untouched, unless you wish to generate a different SD moderation.\code{Negative sd mod}.
#' @param sdM Leave untouched, unless you wish to generate a different SD moderation. \code{Mean of moderator}.
#' @param sdH/M/L refer to SD level of the moderator. feel free to explore, but default is best for analsys.
#' @description This function really does it all. It will generate both a ggplot graph,
#' slopes&intercepts and a summary!
#' @examples mitun(DV=trees$Girth,
#'           x.pred=trees$Height,
#'           x.mod=trees$Volume)
#' # This commands suggests that Girth is predicted by Height, and Volume has moderate affect on Height
#' @examples mitun(..., sdH=2,sdL=-2)
#' # In this example, the graph will generate moderation of SD=+2, mean(sdM=0), and SD=-2.
#' @seealso \code{\link{jtools}} or \code{\link{interactions}} for more options.
#' @note  The moderator MUST be numeric.
#' @returns \code{\link{ggplot}} graph.
#' @returns \code{\link{lm}} model summary.
#' @returns \code{intercepts and slopes} for each level of moderator.
#' @import ggplot2

mitun <- function(DV,x.pred,x.mod,sdH=1,sdM=0,sdL=-1){
  x.pred.c <- scale(x.pred,T,F) # scaling variables
  x.mod.c <- scale(x.mod,T,F) # scaling variables
  moderate <- x.pred.c*x.mod.c # creating moderate variable
  m <- lm(DV~x.pred.c+x.mod.c+moderate) # new model
  # Extracting coeffs
  a <- coef(m)[1]
  b1 <- coef(m)[2]
  b2 <- coef(m)[3]
  b3 <- coef(m)[4]
  # Values for lvls of moderation
  x.modH <- sd(x.mod.c)*sdH
  x.modM <- sd(x.mod.c)*sdM
  x.modL <- sd(x.mod.c)*sdL
  # Slopes
  mH <- b1+b3*x.modH
  mM <- b1+b3*x.modM
  mL <- b1+b3*x.modL
  # Intercepts
  interH <- a+b2*x.modH
  interM <- a
  interL <- a+b2*x.modL
  # Preparing functions for gg graph
  funH <- function(x.pred.c) interH+x.pred.c*mH
  funM <- function(x.pred.c) interM+x.pred.c*mM
  funL <- function(x.pred.c) interL+x.pred.c*mL
  # gg prep (df, legend, lables...)
  data <- data.frame(x.pred.c,DV)
  SD_Moderator = paste0("+",sdH, " SD")
  colL = paste0(sdL, " SD")
  colM <- ifelse(sdM==0,"Mean",
                 ifelse(sdM>0,paste0("+",sdM, " SD"),paste0(sdM," SD")))
  # basic gg graph
  library(ggplot2)
  g <- ggplot(data,aes(x.pred.c,DV))+geom_point(alpha=0.4,
                                                shape=sample(22,1))+labs(fill="Moderator SD")
  # advanced gg graph
  gg <- g+stat_function(fun = funH,aes(color=SD_Moderator),
                        size=0.8)+
    stat_function(fun=funM,aes(color=colM),size=0.6)+
    stat_function(fun=funL,aes(color=colL),size=0.4)+
    xlab("Predictor values")
  print(summary(m))
  print(data.frame(SD=c(sdH,sdM,sdL),intercepts=c(interH,interM,interL),
                   slopes=c(mH,mM,mL)))
  # NOTE when sdH < 0 & coding shit
  print(ifelse(sdH<0,"In the legend, ignore + sign",
               sample(c("WTF?! Its great!","this is the best thing i've seen",
                        "holy shit this function is awesome!!"),1,
                      prob = c(.1,.10,.8))))

  return(gg)
}
