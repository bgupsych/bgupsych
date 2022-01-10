#' Calculates Maximum Likelihood
#' @export
#' @description This function returns the likelyhood of the data,
#' given a binomal probability.
#' @details Say you have a dice. success is defined as getting 4.
#' In \code{n=10} trials, you got \code{k=3} successes. Check the prob to get 4 in a single trial (it's \code{L=1/6}, obviously).
#' Given the data, only 0.16 chance that 0.167 (1/6) was the success probability in each trial.
#' you can check various assumptions by using a vector for the L parameter:
#' \code{ml(3,10,c(1/6,2/6))}
#' Here we can see that it is more likely that the prob to get 4 was 2/6, so the dice was fixed.
#' @param n Number of trials.
#' @param k Number of successes observed in all trials.
#' @param L The probability for success in each trial.
#' @param round Number of decimals desired. default is set to 4.
#' @param geom You can choose out of 3 \code{geoms} to visualise your results: \code{\link{geom_jitter}} (default), \code{\link{geom_col}}, or \code{\link{geom_line}}.
#' @examples ml(k=7,n=10,L=0.7) # see full print for this one.
#'
#' @examples You can change the decimals by adjusting the round argument.
#'  ml(7,10,0.7,round=3)
#' @examples ml(7,10,L=probs,geom="col")
#' @examples You can even test the likelihood of numerous probabilities.
#'  probs <- seq(0,1,length=10) #10 values between 0 and 1
#'  ml(7,10,L=probs)
#' @return \code{\link{data.frame}} with the probability tested and its likelihood.
#'   \code{print}: A verbal print, modified to percentage.
#'   \code{graph} of the Likelihood for each probability.
ml <- function(k,n,L,round=4,geom="jitter"){
  require("ggplot2")
  ml <- dbinom(k,n,L)
  df <- data.frame(prob=round(L,round),Likelihood=round(ml,round))
  dff <- df[order(df[,2],decreasing = T),,]
  if(length(L)<=1){
    cat("Given ",k," succsess in ",n," trials,\nthere's a ",
        dff[1,2]*100,"% chance that ",dff[1,1],"\nis the probability for succsess in each trial.\n\n",sep="")
  }else{
    cat("It is most likley that ",dff[1,1],
        " is the probability for succsess in each trial.\nIt has a ",
        dff[1,2]*100,"% chance.\n\n",sep="")
  }
  gdff=dff[dff[,2]>0,]
  if(geom=="jitter"){print(ggplot(gdff,aes(x=prob,y=Likelihood))+
                              geom_jitter())} else if(geom=="col"){
                                print(ggplot(gdff,aes(x=prob,y=Likelihood))+
                                        geom_col())} else {print(ggplot(gdff,aes(x=prob,y=Likelihood))+
                                                                   geom_line())}
  return(dff)
}
