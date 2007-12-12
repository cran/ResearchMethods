\name{tau}
\alias{tau}
\title{ Finding the non-centrality parameter }
\description{
  An iterative, brute-force function to calculate the non-centrality parameter of the chi-sqaured distribution required to achieve the given power and significance level.
}
\usage{
  tau(k,alpha,beta)
}
\arguments{
  \item{k}{ degrees of freedom }
  \item{alpha}{ The significance level }
  \item{beta}{ 1-power }
}
\details{
  This function is designed for the calculation of sample sizes in survival analysis.  It calulates the non-centrality paramater of a chi-sqaured distribution, given the significance, power and degrees of freedom.
}
\value{
  A scalar value representing the non-centrality paramter is returned.
}
\references{
  Makuch, Robert W. and Simon, Richard M.  \dQuote{Sample Size Requirements for Comparing Time-To-Failure Among k Treatment Groups}.  Chron Dis. 35. pg 861-867/  1982. 
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\keyword{univar}
