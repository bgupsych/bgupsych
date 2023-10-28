#' ID Bikoret (control) number generator
#' @export
#' @param id Only input is your id without bikoret.
#' #' @examples BikoretFinder(12345678)
#' # returns 2
#' @description Not a very useful command :)

BikoretFinder <- function(id){
  # id is a string
  id <- str_split(id,"")
  id <- unlist(id)
  id <- as.vector(as.numeric(id))
  # seperating each digit

  ida <- id[c(1,3,5,7)]
  idb <- id[c(2,4,6,8)]
  idb <- idb*2
  idc <- c(ida,idb)
  # manipulating according to the formula

  # any larger than 10? :
  idd <- idc[idc>=10]
  idd <- as.character(idd)
  idd <- strsplit(idd, "")
  idd <- unlist(idd)
  idd <- as.vector(as.numeric(idd))
  idd <- sum(idd)
  ide <- sum(idd,idc[idc<10],na.rm=T)
  # manipulating data to extract digits
  ide <- strsplit(as.character(ide), "")
  ide <- unlist(ide)
  ide <- as.vector(as.numeric(ide))
  # extracting the final (and important) digit
  FINAL <- 10-ide[2]
  return(FINAL)
}
