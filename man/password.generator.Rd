% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pass.generator.R
\name{password.generator}
\alias{password.generator}
\title{Strong passwords generator}
\usage{
password.generator(pass_length = 8, pass_num = 10, use.signs = F, pin = NULL)
}
\arguments{
\item{pass_length}{The length of each password.}

\item{pass_num}{The total passwords you want to generate.}

\item{use.signs}{A logical, whether to use signs or not. Default is set to FALSE.}

\item{pin}{A numeric pin code (seed) to generate the same list of passwords each time. For random passwords each time, leave untouched.}
}
\description{
Generate as many strong passwords as you wish, in whichever length, and more!
}
\details{
Don't compromise on weak passwords or ones that are hard to remember. With this simple function you can generate strong passwords that contain letters (upper/lower case) and digits, and if you wish, signs as well, such as \link{!#$^&*.~`;}.

If you don't set a pin code, you will get different passwords each time, but if you set a pincode (that ranges between 1 and 10 digits),
 you will be able to produce the same passwords even months later, making sure not one of your strong passwords is lost!
}
\note{
Some websites don't support signs as their password.
}
\examples{
password.generator()
# Since default values are set, running an empty function generates 10 passwords, 8 length each.
password.generator(pass_length=4,pass_num=10,use.signs=TRUE,pin=1927)
# This line of code will produce the same passwords each time.
}
