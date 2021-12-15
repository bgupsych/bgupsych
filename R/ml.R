#' Calculates Maximum Likelihood
#' @export
#' @description This function returns the likelyhood of the data,
#' given a binomal probability.
#' @examples \code{\link{ml(3,10,1/6)}}
#' @details Say you have a dice. success is defined as getting 4.
#' In \code{n=10} trials, you got \code{k=3} successes. Check the prob to get 4 in a single trial (it's \code{L=1/6}, obviously).
#' Given the data, only 0.16 chance that 0.167 (1/6) was the success probability in each trial.
#' you can check various assumptions by using a vector for the L parameter:
#' \code{\link{ml(3,10,c(1/6,2/6))}}
#' Here we can see that it is more likely that the prob to get 4 was 2/6, so the dice was fixed.
#' @param n Number of trials.
#' @param k Number of successes observed in all trials.
#' @param L the probability for success in each trial.
#' @return \code{prob}: Is the probability of the observed data,
#'  given the \code{L}: the probability for success in each trial.
ml <- function(k,n,L){
ml <- dbinom(k,n,L)
return(c(round(L,3),prob=round(ml,2)))
}

