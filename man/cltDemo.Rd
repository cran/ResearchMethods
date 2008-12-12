\name{cltDemo}
\alias{cltDemo}
\title{A demonstration of how the central limit theorem works on samples of size n.}
\description{
  This function demonstrates how the central theorem works on a variety of distributions, by allowing the user to manipulate the size of a sample and see that, if the sample size is large enough, the distribution of the sample mean is approximately normal.
}
\usage{
  cltDemo()
}
\details{
This function produces two windows. The Tk window, or the control window, A set of buttons to select the distribution to sample from, as well as a button to increment the sample size and an entry box to set the value of the desired parameter for the exponential and chi-squared distributions.

The plotting window has two figures: the one on the left is a histogram of the means from 10,000 samples of size n from the chosen distribution.  The figure on the left is a Q-Q plot comparing the quantiles of the sample means with the quantiles of the normal distribution.
}
\value{
  No value is returned.
}
\author{ Kevin Thorpe, Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\examples{ 
  cltDemo()
}
\keyword{survey}
