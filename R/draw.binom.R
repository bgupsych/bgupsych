#' Draws a Binomal distribution, Tohelet and Shonut
#' @export
#' @description By giving the desired X (Mishtane Mikri), number of trials and succsses rate,
#' You will get a clean graph, plus a \code{\link{data.frame}} and two prints of Tohelet and Shonut.
#' @details There are 2 red balls and 3 white ones in a bucket. Each time you pick a ball,
#' you return it to the bucket (replace=TRUE).
#' Succsess is to pull a red ball. You have n=3 trials. What are your X and their probability?
#' \code{draw.binom(X=c(0,1,2,3),n=3,2/5)}.
#'
#' @param X Possible overall succsess in all trials (Mishtane Mikri).
#' @param n Number of trials.
#' @param p The probability for success in each trial.
#' @param round Number of decimals desired. default is set to 3.
#' @examples draw.binom(10:12,12,0.85) # 10,11 or 12 succsses in 12 trials with 85% chance for succsess.
#'
#' @examples You can change the decimals by adjusting the 'round' argument.
#'  draw.binom(c(0,1,2),2,3/5,round=5)
#' @return \code{\link{data.frame}} with the specific X and its probability.
#'   \code{\link{print}}: two verbal prints: Tohelet and Shonut of your experiment.
#' @note when using large numbers, \code{\link{ggplot2}} graph might go off. Sorry about that. See data instead.
#' @seealso Use \code{\link{draw}} to draw a generic distribution.

draw.binom <- function(X,n,p,round=3){
  prob=dbinom(X,n,p)
  a=data.frame(x=X,prob=round(prob,round))
  g=ggplot(a,aes(x=X,y=prob,fill=prob))+geom_col()+
    theme(legend.position = "none")+
    scale_y_continuous(breaks = NULL)+
    scale_x_continuous(breaks = X)+
    geom_label(aes(label=round(prob,round)),
               color="white",alpha=0.6,nudge_y = 0.02)+
    xlab("MISHTANE MIKRI")
  cat("Tohelet =",n*p,"\n")
  cat("Shonut =",round(n*p*(1-p),round),"\n")
  print(a)
  return(g)
}
