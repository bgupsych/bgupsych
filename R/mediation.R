#' Mediation between two variables.
#' @export
#' @param DV The dependent variable.
#' @param x.pred The predictor variable.
#' @param x.med The mediation variable.
#' @param alpha Used in significant tests. default is 5 and means 0.05 .
#' @description This function calculates the direct and indirect effect of two variables,
#'  mediated by a 3rd variable.
#' @details Output is a table of your 4 paths, their value and p value.
#' 3 outputs are printed to ensure the results.
#' @examples \dontshow{attach(depression)}
#' mediation(DV=Bsum,x.pred = age,x.med = sat)
#' # In this example, we check whether satisfaction is the mediator depression score (Bsum) Height and age.
#' @examples \dontrun{mediation(..., alpha =1)}
#' # In this example, the alpha is set to 1 (=0.01).
#' @seealso \code{\link{medmod}} package for more options.
#'
#' \url{https://cran.r-project.org/web/packages/medmod/medmod.pdf}
#' @note  If some paths are significant and some aren't, check table for more details.
#' @returns Please pay attention to the prints. They will conclude the results and warn you if there is anything to worry about.
#' In case something went wrong with the formula for some reason, a \code{\link{stop}} command will stop exacution.
#' @returns \code{data.frame} with the results.

mediation <- function(DV,x.pred,x.med,alpha=5){
  al <- alpha/100
  x.model <- summary(lm(DV~x.pred))
  # Creating the basic lm() modelv
  c <- coef(x.model)[2,c(1,4)]
  m.model <- summary(lm(x.med~x.pred))
  # Path model
  a <- coef(m.model)[2,c(1,4)]
  full.model <- summary(lm(DV~x.med+x.pred))
  # Full model including the mediator
  b <- coef(full.model)[2,c(1,4)]
  c.tag <- coef(full.model)[3,c(1,4)]
  # Pulling dirrect effect from full model
  test <- round(c.tag[1]+a[1]*b[1],5)==round(c[1],5)
  error <- c("Something is wrong with the formula c'+a*b=c")
  # Incase the formula doesn't exist, adding an Error msg
  sig <- unique(c(c[2]<al,a[2]<al,
                  b[2]<al,c.tag[2]<al))
  paths <- data.frame(
    a=round(a,4),b=round(b,4),
    "c.tag"=round(c.tag,4),c=round(c,4))
  # Creatig the data frame to return
  rownames(paths) <- c("Mekadem","P VALUE")
  ifelse(test,"",stop(error))
  if(alpha<1){cat("did you mean alpha=",
                  alpha*100,"%? change value to ",alpha*100,"\n",sep="")
  } else {
    ifelse(sig&length(sig)==1,print("all paths are significant"),
           print("NOT all paths are significant, see table"))}
  print(ifelse(as.numeric(round(c.tag[1],3))==0 |
                 as.numeric(c.tag[2])>al,
               "c tag is close to zero, or isn't significant",
               "There's a direct and significant effect"))
  # Testing hypothesis
  cat("\n")
  return(paths)
}
