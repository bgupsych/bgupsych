% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fibonacci.R
\name{fibonacci}
\alias{fibonacci}
\title{Fibonacci sequence generator}
\source{
\url{https://www.mathsisfun.com/numbers/fibonacci-sequence.html}
}
\usage{
fibonacci(x, round = 9)
}
\arguments{
\item{x}{The number of numbers in the sequence.}

\item{round}{Decimals to print of the golden ratio, default is 9.}
}
\description{
This function calculates the fibonacci sequence and returns an approximation of the golden ratio.
}
\note{
R can't print more than 9 decimals, so leave at default for max accuracy.
The golden ratio is 1.61803398874989484820...
}
\examples{
fibonacci(20)
# returns the sequence, up to the 20th number
fibonacci(x=20,decimals=4)
# returns up to the 20th number, golden ratio is rounded to 4 decimal digits
}
