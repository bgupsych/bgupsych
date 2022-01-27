#' Sampling Distribution Generator
#' @export
#' @param vector A numeric vector.
#' @param central The type of central value you want to apply (mean, median, sd etc..).
#' @param n Number of times to sample the data from a given sample (dont confuse with sample size for each sample).
#' @param sam_size Sample size to sample each time
#' @param hist A logical. Whether to plot a histogram. Default is set to TRUE.
#' @param replace A logical for random sampling. Default is set to TRUE.
#' @examples
#' times_50 <- sampdist(big5$age,mean,100,100)
#' # How the histogram looks when sampling 50 means?
#' @examples
#' times_5000 <- sampdist(big5$age,central=mean,n=1000,samp_size=100)
#' # And with 5,000?
#' @examples
#' iqrs <- sampdist(big5$age,central=IQR,n=1000,samp_size=100)
#' # What about the IQR? and sd? median? Try any central value with different n/samp_size.
#' @description Use this to print and plot different sample distributions
#' @details This function allows you the flexibility to change values (sample size, central value or number of samples) and see for yourself how the histograms and output change.
#' @seealso Take a look at the well known webApp:
#' \url{https://onlinestatbook.com/stat_sim/sampling_dist/}.
sampdist <- function(vector,central,n,samp_size,hist=TRUE,replace=TRUE){
  loops <- c()
  for( i in 1:n){
    data_loops <- sample(vector,samp_size,replace)
    loops[i] <- central(data_loops)
  }
  if(hist){hist(loops)}
  return(loops)
}
