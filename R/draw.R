#' Draws a  distribution, Tohelet and Shonut
#' @export
#' @description Generates a graph, Tohelet and Shonut, plus a \code{\link{data.frame}} with your data.
#' @details By supplying the X and P(x), This function will generate the Tohelet and Shonut, and a clean graph.
#'
#' @param X Mishtane Mikri.
#' @param p The probability for each x 'P(x)'
#' @param round Number of decimals desired. default is set to 3.
#'
#' @examples X=10:12
#' probs=c(0.4,0.25,0.35)
#' draw(X,probs)
#' # Distribution of 10, 11 & 12, with their probs as created in 'X' and 'Probs'.
#'
#' @examples # You can change the decimals by adjusting the 'round' argument.
#'  draw(c(0,1,2),c(0.2,0.5,0.3),round=5)
#' @return \code{\link{data.frame}} with the specific X and its probability.
#'   \code{\link{print}}: two verbal prints: Tohelet and Shonut of your experiment.
#' @seealso Use \code{\link{draw.binom}} to draw a binomal distribution.

draw <- function(X,P,round=3){
  a=data.frame(X=X,prob=round(P,round))
  t=sum(X*P)
  print(ggplot(a,aes(X,prob,fill=prob))+
          geom_col()+
          geom_label(aes(label=round(prob,round)),
                     color="white",alpha=0.6,nudge_y = 0.02)+
          scale_x_continuous(breaks = X)+
          theme(legend.position = "none"))+
    scale_y_continuous(breaks = NULL)
  cat("Tohelet:", t,"\n")
  cat("Shonut:",sum((X-t)^2*P),"\n")
  print(a)

}
