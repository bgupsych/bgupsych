#' Draws any distribution
#' @description By providing X and their probability, you can draw either CDF or PMF
#' @export
#' @param X Mishtane Mikri. Can be a vector of values.
#' @param P Probability. Should be the same length as X (also vectorized).
#' @param cumulative a logical. If TRUE: draws a CDF. default is set to FALSE.
#' @param sort.prob a logical. Concerns the data frame returned, if TRUE: sort by probability (in decreasing order), Otherwise same order as input of X. default is set to FALSE.
#' @param round How many decimals to show. Default is set to 3.
#' @param show.label A logical. Default (TRUE) prints probability labels on the plot.
#' @param nudge When \code{show.label} is TRUE, you can adjust the location of the labels by inputing a 2 length vector to adjust the label position (a single values works too). Usualy no adjustment is needed.
#' @returns \code{graph} (Print only).
#' @return \code{Tohelet and Shonut} (Print only).
#' @return \code{data.frame} of X and their probs (use \code{sort.prob} to change order method).
#' @note Probs can add up to more than 1, It will work, but not advised and a warning message will appear.
#' Negative probs will be automatically corrected to absolute value.
#' @examples x <- c(1:5)
#' p <- c(0.1,0.2,0.25,0.25,0.2)
#' draw(x,p)
#' @examples draw(x,p,cumulative=TRUE, sort.prob=TRUE)
#' @examples dice <- 1:6
#' draw(dice,-1/6)
#' ## Even when using negative probability, it will work.
#' @seealso  For a fully detailed explanation about the differences between distributions please visit \url{https://www.researchgate.net/post/What-is-the-difference-between-probability-distribution-function-and-probability-density-function}
#'
draw <- function(X,p,cumulative=F,sort.prob=F,round=3,show.label=TRUE,nudge=0){
  if (!require("ggplot2"))library(ggplot2)
  P <- abs(p)
  len=c("Please make sure there's either 1 probability (like in a dice),\n  or same length as X (Mishtane Mikri)")
  if(length(P)!=1&length(P)!=length(X)){stop(len)}
  t=sum(X*P)
  ogp=P
  if(cumulative){for(i in 1:length(P)){
    P[i]=sum(P[i],P[i-1])}}
  if(length(nudge==1)){nudge=c(nudge,nudge)}
  a=data.frame(X=X,prob=round(P,round))
  g=ggplot(a,aes(X,prob,fill=prob))+
          geom_col()+
          geom_label(aes(label=round(prob,round)),
                     color="white",alpha=0.6,nudge_y = 0.02)+
          scale_x_continuous(breaks = X)+
          theme(legend.position = "none")+
          scale_y_continuous(breaks = NULL)

  cat("Tohelet:", t,"\n")
  cat("Shonut:",sum((X-t)^2*ogp),"\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}

if(sum(ogp)>1){cat("\nWARNING:\nProbabilities add up tp more than 1.\n")}
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}
  ifelse(show.label,print(g+geom_label(aes(label=round(prob,round)),
                                       alpha=0.6,nudge_x = nudge[1],nudge_y = nudge[2],color="white")),print(g))

  return(a)
}

#' Draws a binomal distribution
#' @description By providing X, n and a probability, you can draw either CDF or PMF
#' @export
#' @param X Mishtane Mikri. Can be a vector of values. each X is k successes.
#' @param n Number of trials. If not mentioned, same as the maximum X.
#' @param P Probability for success in each trial.
#' @param cumulative a logical. If TRUE: draws a CDF. default is set to FALSE.
#' @param sort.prob a logical. Concerns the data frame returned, if TRUE: sort by probability (in decreasing order), Otherwise same order as input of X. default is set to FALSE.
#' @param round How many decimals to show. Default is set to 3.
#' @param show.label A logical. Default (TRUE) prints probability labels on the plot.
#' @param nudge When \code{show.label} is TRUE, you can adjust the location of the labels by inputing a 2 length vector to adjust the label position (a single values works too). Usualy no adjustment is needed.
#' @returns \code{graph} (Print only).
#' @return \code{Tohelet and Shonut} (Print only).
#' @return \code{data.frame} of X and their probs (use \code{sort.prob} to change order method).
#' @note Negative probs will be automatically corrected to absolute value, and ranged \code{0 < p < 1}.
#' @examples x <- c(0:5)
#' draw.binom(x,n=6,p=0.3)
#' @examples draw.binom(x,n=6,p=0.3,cumulative=TRUE, sort.prob=TRUE)
#' @examples dice <- 1:6
#' draw.binom(dice,p=-1/6)
#' ## Even when using negative probability, it will work.
#' @seealso  For a fully detailed explanation about the differences between distributions please visit \url{https://www.researchgate.net/post/What-is-the-difference-between-probability-distribution-function-and-probability-density-function}
#'
draw.binom <- function (X, n = max(X), p, cumulative = F, sort.prob = F, round = 3,
                        show.label = TRUE, nudge = 0)
{
  if (!require("ggplot2"))library(ggplot2)
  p <- abs(p)
  errp = c("\nProbability MUST be less than 1.")
  if (p > 1) {stop(errp)}

  if (max(X) > n) {n = max(X)}

  toh = n * p
  sho = round(n * p * (1 - p), round)

  if (cumulative) {prob = pbinom(X, n, p)}
  else {prob = dbinom(X, n, p)}

  if(length(nudge==1)){nudge=c(nudge,nudge)}

  a = data.frame(X = X, prob = round(prob, round))
  g = ggplot(a, aes(X, y = prob, fill = prob)) + geom_col() +
    theme(legend.position = "none") + scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = X)
  cat("Tohelet =", toh, "\n")
  cat("Shonut =", sho, "\n\n")
  if (sort.prob) {
    a = a[order(a[, 2], decreasing = T), , ]
  }
  ifelse(show.label, print(g +
                             geom_label(aes(label = round(prob, round)),
                                        alpha = 0.6, nudge_x  = nudge[1],
                                        nudge_y = nudge[2], color = "white")),
         print(g))
  return(a)
}

#' Draws a Geometric distribution
#' @description By providing X and a probability, you can draw either CDF or PMF
#' @export
#' @param X Mishtane Mikri. Can be a vector of values. each X is k successes.
#' @param P Probability for success in each trial.
#' @param cumulative a logical. If TRUE: draws a CDF. default is set to FALSE.
#' @param decreasing a logical. Concerns the data frame returned, if TRUE: decreasing order of probabilities, Otherwise same order as input of X. default is set to FALSE.
#' @param round How many decimals to show. Default is set to 3.
#' @param show.label A logical. Default (TRUE) prints probability labels on the plot.
#' @param nudge When \code{show.label} is TRUE, you can adjust the location of the labels by inputing a 2 length vector to adjust the label position (a single values works too). Usualy no adjustment is needed.
#' @returns \code{graph} (Print only).
#' @return \code{Tohelet and Shonut} (Print only).
#' @return \code{data.frame} of X and their probs (use \code{decreasing} to change order method, in case random numbers are assigned).
#' @note Very important: Unlike usual input in \code{\link{dgeom}}, please refer to 1 as success in 1st attempt.
#' @examples x <- c(1:5)
#' draw.geom(x,p=0.3)
#' @examples draw.geom(x,p=0.3,cumulative=TRUE)
#' @seealso  For a fully detailed explanation about the differences between distributions please visit \url{https://www.researchgate.net/post/What-is-the-difference-between-probability-distribution-function-and-probability-density-function}
#'
draw.geom <- function(X,p,cumulative=F,decreasing=F,round=3,show.label=TRUE,nudge=0){
  if (!require("ggplot2"))library(ggplot2)
  p <- abs(p)
  X <- abs(X)
  # Validation argument
  errp=c("\nProbability MUST be smaller than 1")
  if(p>1){stop(errp)}
  ifelse(X==0 & length(X)==1,X <-X+1 ,"")
  # if prob is greater than 1 or used x=0, stop.
  toh=1/p
  sho=(1-p)/(p^2)
  if(cumulative){prob=pgeom(X-1,p)}
  else {prob=dgeom(X-1,p)}

  a=data.frame(x=X,"prob"=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+
    geom_point()+geom_col(alpha=0.7)+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = X)+
    xlab("MISHTANE MIKRI")+
    labs(caption = "This function uses x=1 as succsses in 1st try")

    cat("Tohelet =",toh,"\n")
  cat("Shonut =",sho,"\n")
  if(decreasing){a=a[order(a[,2],decreasing = T), ,]}
  if(length(nudge==1)){nudge=c(nudge,nudge)}
  ifelse(show.label,print(g+geom_label(aes(label=round(prob,round)),
                                       alpha=0.6,nudge_x = nudge[1],nudge_y = nudge[2],color="white")),print(g))

  return(a)
}

#' Draws a Poisson distribution
#' @description By providing X and a rate of events, you can draw either CDF or PMF
#' @export
#' @param X Mishtane Mikri. Can be a vector of values. each X is k successes.
#' @param lam Rate of events.
#' @param cumulative a logical. If TRUE: draws a CDF. default is set to FALSE.
#' @param sort.prob a logical. Concerns the data frame returned, if TRUE: sort by probability (in decreasing order), Otherwise same order as input of X. default is set to FALSE.
#' @param round How many decimals to show. Default is set to 3.
#' @param show.label A logical. Default (TRUE) prints probability labels on the plot.
#' @param nudge When \code{show.label} is TRUE, you can adjust the location of the labels by inputing a 2 length vector to adjust the label position (a single values works too). Usualy no adjustment is needed.
#' @returns \code{graph} (Print only).
#' @return \code{Tohelet and Shonut} (Print only).
#' @return \code{data.frame} of X and their probs (use \code{sort.prob} to change order method, if random numbers are assigned).
#' @note Very important: Unlike usual input in \code{\link{dgeom}}, please refer to 1 as success in 1st attempt.
#' @examples x <- c(1:5)
#' draw.pois(x,lam=0.3)
#' @examples draw.pois(x,lam=1.2,cumulative=TRUE)
#' @seealso  For a fully detailed explanation about the differences between distributions please visit \url{https://www.researchgate.net/post/What-is-the-difference-between-probability-distribution-function-and-probability-density-function}
#'
draw.pois <- function(X,lam,cumulative=F,sort.prob=F,round=3,show.label=TRUE,nudge=0){
  if (!require("ggplot2"))library(ggplot2)
  lam <- abs(lam)
  X <- abs(X)
  # Making the function robust
  lamog=lam
  if(cumulative){prob=ppois(X,lam)}
  else {prob=dpois(X,lam)}

  a=data.frame(x=X,prob=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+geom_col()+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = X)+
    xlab("MISHTANE MIKRI")

  cat("Tohelet =",lamog,"\n")
  cat("Shonut =",lamog,"\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}

  if(length(nudge==1)){nudge=c(nudge,nudge)}
  ifelse(show.label,print(g+geom_label(aes(label=round(prob,round)),
   alpha=0.6,nudge_x = nudge[1],nudge_y = nudge[2],color="white")),print(g))

  return(a)
}

#' Draws an Exponential distribution
#' @description By providing X and a rate of events, you can draw either CDF or PMF
#' @export
#' @param X Mishtane Mikri. Can be a vector of values.
#' @param rate Rate of events (same as lambda).
#' @param cumulative a logical. If FALSE: draws a PMF default is set to TRUE
#' @param sort.prob a logical. Concerns the data frame returned, if TRUE: sort by probability (in decreasing order), Otherwise same order as input of X. default is set to FALSE.
#' @param round How many decimals to show. Default is set to 3.
#' @param x.range a vector of two numbers The probability of occurrence between these two values. Use \code{c(min,max)}.
#' @param show.label A logical. Default (TRUE) prints probability labels on the plot.
#' @param nudge When \code{show.label} is TRUE, you can adjust the location of the labels by inputing a 2 length vector to adjust the label position (a single values works too). Usualy no adjustment is needed.
#' @returns \code{graph} (Print only).
#' @return \code{Tohelet and Shonut} (Print only).
#' @return \code{data.frame} of X and their probs (use \code{sort.prob} to change order method, in case random numbers are assigned).
#' @return \code{range} The probability between the range in \code{x.range}. "Prob for first success between 1st and 2nd argument of the vector"
#' @seealso  For a fully detailed explanation about the differences between distributions please visit \url{https://www.researchgate.net/post/What-is-the-difference-between-probability-distribution-function-and-probability-density-function}
#' @examples x <- c(1:5)
#' draw.exp(x,rate=0.3)
#' @examples draw.exp(0:5,rate=1/3,x.range=c(3,5))
#' ## Prob for successes between this range (3rd and 5th), given P~rate of 1/3.
#'
draw.exp <- function(X,rate,cumulative=T,sort.prob=F,round=3,x.range=NULL,show.label=TRUE,nudge=0){
  if (!require("ggplot2"))library(ggplot2)
  rate <- abs(rate)
  X <- abs(X)
  # Making the function robust
  toh=round(1/rate,round)
  sho=round(1/(rate^2),round)
  if(cumulative){prob=pexp(X,rate)}
  else {prob=dexp(X,rate)}

  a=data.frame(x=X,prob=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+geom_col(alpha=.4)+
    geom_col(alpha=.4)+
    geom_smooth(se=F,formula = 'y ~ x',method = 'loess')+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = X)+
    xlab("MISHTANE MIKRI")+ylab("density")+
    labs(caption = "for P(x) use cumulative=TRUE ")

  cat("Tohelet (and SD) =",toh,"\n")
  cat("Shonut =",sho,"\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}
  if(is.vector(x.range)) {cat("Probability for event between",
                              x.range[1],"and",x.range[2],"is",round(abs(pexp(x.range[1],rate)-pexp(x.range[2],rate)),round),"\n")}
  if(length(nudge==1)){nudge=c(nudge,nudge)}
  ifelse(show.label,print(g+geom_label(aes(label=round(prob,round)),
                                       alpha=0.8,nudge_x = nudge[1],nudge_y = nudge[2],color="white")),print(g))
  return(a)
}

#' Draws an Normal distribution
#' @description By providing X, mean and sd, you can draw either CDF or PMF
#' @export
#' @param X Mishtane Mikri. Can be a vector of values.
#' @param mean The mean of the distribution
#' @param sd The standard deviation of the distribution
#' @param cumulative A logical. If FALSE: draws a PMF default is set to TRUE
#' @param sort.prob A logical. Concerns the data frame returned, if TRUE: sort by probability (in decreasing order), Otherwise same order as input of X. default is set to FALSE.
#' @param round How many decimals to show. Default is set to 3.
#' @param x.range a vector of two numbers The probability of occurrence between these two values. Use \code{c(min,max)}.
#' @param show.label A logical. Default (TRUE) prints probability labels on the plot.
#' @param nudge When \code{show.label} is TRUE, you can adjust the location of the labels by inputing a 2 length vector to adjust the label position (a single values works too). Usualy no adjustment is needed.
#' @returns \code{graph} (Print only).
#' @return \code{Tohelet and Shonut} (Print only).
#' @return \code{data.frame} of X and their probs (use \code{sort.prob} to change order method.
#' @return \code{range} The probability between the range in \code{x.range}. "Prob for success between these two values"
#' @examples x <- seq(60,90,by=5)
#' draw.norm(x,mean=75,sd=5)
#' @examples draw.norm(x,mean=75,sd=5,cumulative=FALSE)
#' @examples draw.norm(0:10,5,1.5,x.range=c(3.5,7))
#' # In a distribution of mean=5 and sd=1.5, print values 0-10. Also what is the probability between 3.5 and 7?
#' @seealso  For a fully detailed explanation about the differences between distributions please visit \url{https://www.researchgate.net/post/What-is-the-difference-between-probability-distribution-function-and-probability-density-function}
#'
draw.norm <- function(X,mean=0,sd=1,cumulative=T,sort.prob=F,round=3,x.range=NULL,show.label=TRUE,nudge=0){
  if (!require("ggplot2"))library(ggplot2)
  toh=mean
  sho=sd^2

  if(cumulative){prob=pnorm(X,mean,sd)}
  else {prob=dnorm(X,mean,sd)}
  a=data.frame(x=X,prob=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+geom_col(alpha=.4)+
    geom_smooth(se=F,formula = 'y ~ x',method = 'loess')+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = c(round(min(X),round),
                                  round(mean(X),round),round(max(X),round),toh))+
    xlab("MISHTANE MIKRI")+ylab("density")+
    labs(caption = "for P(x) use cumulative=TRUE ")

  cat("Tohelet = ",toh,"\n")
  cat("Shonut =",sho,"\n")
  if(sort.prob){a=a[order(a[,2],decreasing = T), ,]}
  if(is.vector(x.range)) {cat("Probability for event between",
                              x.range[1],"and",x.range[2],"is",round(abs(pnorm(
                                x.range[1],mean,sd)-pnorm(x.range[2],mean,sd)),round),"\n")}
  if(length(nudge==1)){nudge=c(nudge,nudge)}
  ifelse(show.label,print(g+geom_label(aes(label=round(prob,round)),
                                       alpha=0.8,nudge_x = nudge[1],nudge_y = nudge[2],color="white")),print(g))
  return(a)
}
