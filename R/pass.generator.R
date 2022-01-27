#' Strong passwords generator
#' @export
#' @param pass_length The length of each password.
#' @param pass_num The total passwords you want to generate.
#' @param use.signs A logical, whether to use signs or not. Default is set to FALSE.
#' @param pin A numeric pin code (seed) to generate the same list of passwords each time. For random passwords each time, leave untouched.
#' @examples password.generator()
#' # Since default values are set, running an empty function generates 10 passwords, 8 length each.
#' @examples password.generator(pass_length=4,pass_num=10,use.signs=TRUE,pin=1927)
#' # This line of code will produce the same passwords each time.
#' @details Don't compromise on weak passwords or ones that are hard to remember. With this simple function you can generate strong passwords that contain letters (upper/lower case) and digits, and if you wish, signs as well, such as \link{!#$^&*.~`;}.
#'
#' If you don't set a pin code, you will get different passwords each time, but if you set a pincode (that ranges between 1 and 10 digits),
#'  you will be able to produce the same passwords even months later, making sure not one of your strong passwords is lost!
#' @note Some websites don't support signs as their password.
#' @description Generate as many strong passwords as you wish, in whichever length, and more!
password.generator <- function(pass_length=8,pass_num=10,use.signs=F,pin=NULL){
  if(use.signs){require("stringr")}
  set.seed(pin)
  pass.ops <- c(LETTERS,letters,0:9)
  signs=unlist(stringr::str_split("!@#$%^&*.~`;",pattern = ""))
  if(use.signs){pass.ops=c(pass.ops,signs)}
  password <- function(pass_length){
    paste(sample(pass.ops,
                 pass_length,T),collapse = "")}
  pass <- c()
  for(i in 1:pass_num){
    pass[i] <- password(pass_length)
  }
  return(data.frame(pass))
}
