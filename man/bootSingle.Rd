\name{bootSingle}
\alias{bootSingle}
\title{A demonstration of how bootstrapping works step by step for one function.}
\description{
  This function uses a graphical user interface (GUI) to demonstrate how bootstrapping works, by bootstrapping a sample of the alphabet, either step-by-step or as an entire sample.  
}
\usage{
  bootSingle()
}
\details{
This function produces two windows. The Tk window, or the control window, contains two sampling button: the top button takes an entire sample, the bottom button takes a stepwise sample: note that the first 26 clicks take the samples and then the button changes so that it can sort the sample.

The second window is a visual demonstration of bootstrapping: the top is a color-coded bootstrapped sample, while the bottom is the population sampled from: when a circle in the bottom turns red it means that it has been sampled from.
}
\value{
  No value is returned.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\references{ This function was designed for a course by Mohamed Abdolell }
\seealso{ \code{\link{bootSequence}} }
\examples{ 
  bootSingle()
}
\keyword{survey}
