#' Find the norm of a vector
#' @export
#' @description The default is finding the euclidean norm of a given vector, though you can modift the function to find any norm.
#' @param vec A given vector.
#' @param p The specific dimension, default is \code{p=2}.
#' @returns The norm of the vector.
#' @examples
#' find_norm(vec = c(1,2,3))
#' find_norm(c(0,1,0,0,1),p=0)
#' find_norm(c(2,-6,1.5,0,2),Inf)
find_norm <- function(vec,p=2){

  # For the general exponential p (default: p=2)
  res <- sqrt(sum(vec^p))

  # For infinite exponant, returns the maximum absolute value
  if(p==Inf){
    res=max(abs(vec))
  }

  # For exponant of p=0, returns the length of non-zero values
  if(p==0){
    res=length(vec[vec!=0])
  }
  return(res)
}
