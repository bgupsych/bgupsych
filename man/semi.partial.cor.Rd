% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semi.partial.cor.R
\name{semi.partial.cor}
\alias{semi.partial.cor}
\title{Partial and Semi-partial correlations}
\usage{
semi.partial.cor(x1, x2, x.controlled)
}
\arguments{
\item{x1}{A numeric vector you want to correlate with x2.}

\item{x2}{A numeric vector you want to correlate with x1.}

\item{x.controlled}{A numeric vector you want to hold constant.}
}
\value{
\code{data.frame} with the \code{estimate} for both semi-partial & partial correlations, and its \code{p-value}.
}
\description{
In the Partial correlation, the controlled variable is removed from both DV and UDV,
While in Semi-partial, controlled is removed only from the UDV, and so it is very much alike the Beta (direct effect).
}
\details{
Be sure to understand the theoretical meaning of semi and partial correlation.
In a nutshell, both correlate residuals created by a third variable.
 Partial correlation is the correlation of two
 variables while controlling a third.
  SP holds constant the controlled variable only for the UDV (predictor).
  Read more here \url{https://www.youtube.com/watch?v=OpAf4N582bA}
}
\note{
Partial will always be Greater than/Equal to Semi-partial.
}
\examples{
\dontshow{attach(depression)}
semi.partial.cor(x1=Bsum,x2=m_years, x.controlled=age)
# We correlate 'Bsum' and 'm_years', holding 'age' constant. In SP it's been removed from only the predicting variable,
# whilst in partial from both.
\dontshow{detach(depression)}
}
\seealso{
This function runs \code{\link{ppcor}}, check it out for more options.
}
