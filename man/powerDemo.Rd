\name{powerDemo}
\alias{powerDemo}
\title{A demonstration of the ways in which the power of a study can be adjusted}
\description{
  This function attaches a graphical user interface (GUI) to the power.examp function in the TeachingDemos library, as well as normalizing the two curves (where the power.examp function doesn't).  
}
\usage{
  powerDemo(n=1, sd=1, diff=1, alpha=0.05, xmin=-4, xmax=4, colored=NULL)
}
\arguments{
  \item{n}{The starting value for n}
  \item{sd}{The starting value for stdev}
  \item{diff}{The starting value for diff}
  \item{alpha}{The starting value for alpha}
  \item{xmin,xmax}{The limits for the plotting axes}
  \item{colored}{The name of the variable to color red}
}
\details{
This function produces two windows. The Tk window, or the control window, contains four sliders that control the four manipulable values.

The second window is the two plots: the top plot is the distribution if the null hypothesis is true (with the type I error highlighted), the bottom is the distribution is the alternative hypothesis is true (with 1-type II error highlighted).  Note that both distributions are normalized. 
}
\value{
  No value is returned.
}
\note{
  The sliders have a step size of 1: the value of a slider may manipulated by either clicking and draging the slider or clicking and holding the space to the left/right of the slider, which will decrease/increase the slider by the step size.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\references{ This function was designed for a course by Mohamed Abdolell }
\seealso{ \code{power.examp} from the TeachingDemos library }
\examples{ 
  powerDemo()
  powerDemo(n=100,sd=10,diff=3,alpha=0.025,colored='diff')
}
\keyword{distribution}
