#' Install and/or call multiple packages
#' @export
#' @param packages A string of 1 or more packages names to be installed. Be aware of typos.
#' @param library.only  A logical. When TRUE, function only calls required packages, otherwise, packages will be installed. Default is set to TRUE.
#' @description A vectorized version of \code{\link{library}} or \code{\link{install.packages}} that allows you to install and call from memory multiple packages at once.
#' @note If you ask to re-install an existing package, a restart might be necessary. just press YES when asked by R. Otherwise abort and set \code{library.only} to TRUE.
#' @examples use.package("tidyverse")
#' @examples my_pack <- c("ggplot2","magrittr")
#' use.package(my_pack,library.only=FALSE) # This will re-install these packages, Abort and set library only to TRUE.
use.package <- function(packages,library.only=TRUE){
  ifelse(library.only,"",lapply(packages,install.packages,character.only=T))
  lapply(packages, require, character.only = T)
}
