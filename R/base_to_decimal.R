#' Find the decimal representation of any numerical system in any base.
#' @export
#' @import stringr
#' @import purrr
#' @description Solve any numerical system in a given base easily.
#' @examples base_to_decimal10(digits=10010,base=2)
#' @param digits A digit vector in a specific base
#' @param base The base of your digits
#' @returns A numeric object, The decimal (base 10) answer.
base_to_decimal10 <- function(digits,base){
  require(stringr)

  # Informative error messege:
  err <- paste("\nAt least one digit is equal
              or bigger than Base = ",base)

  # Turning the numbers to seperate digits:
  vec <- as.numeric(unlist(str_split(digits,"")))

  # Avoiding forbidden digits for a given base
  ifelse(vec>=base,
         stop(err),
         "")

  # Core of function:
  to_sum <- c()
  exponent <- sort(0:(length(vec)-1),decreasing = TRUE)

  for(i in 1:length(vec)){

    to_sum[i] <- vec[i]*base^exponent[i]

  }

  # Informative solution:
  cat(
    paste("  ",as.numeric(str_flatten(vec)),
          "in base", base,
          "=",sum(to_sum),"\n")
  )

  # Numeric solution
  return(sum(to_sum))
}


