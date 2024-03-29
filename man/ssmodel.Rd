% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ssmodel.R
\name{ssmodel}
\alias{ssmodel}
\title{Generates the SST/Res/Reg, Rsq and Adj Rsq of lm model.}
\usage{
ssmodel(model)
}
\arguments{
\item{model}{\code{\link{lm}} object}
}
\description{
Make sure to create your \code{\link{lm}} object first. Put your object in the function and see how magic happens.
}
\examples{
x <- lm(Happiness.Score~.,Happiness.Score)
ssmodel(x)
# This model predicts Happiness score by all other variables in the data frame.

y <- lm(Bsum~age+sat,data=depression)
ssmodel(y)
# This model only uses age and sat to predict Bsum
}
\seealso{
What happens if you use the mean in your model as prediction?

\code{Mean <- rep(mean(depression$Bsum),nrow(depression))}
# a repetition of the Bsum's mean, times the number of observations in 'depression'
\code{ssmodel(lm(Bsum~Mean,depression))}
# The mean value is used to predict Bsum. See for yourself that SSRes=SST and SSReg=0.
}
