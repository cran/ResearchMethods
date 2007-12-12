\name{histSample}
\alias{histSample}
\title{Sample size effects on sample distribution}
\description{
  Using a graphical user interface (GUI) this function demonstrates that, for a variety of distributions, as the sample size increases, the the sample-histogram converges to the true distribution.
}
\usage{
  histSample(dist=NA,gui=TRUE,yMax=FALSE,n=10000,sub=NaN,par=c(0,1))
}
\arguments{
  \item{dist}{the distribution being used, see below for a list.}
  \item{gui}{An indicator of whether the interface should be activated.}
  \item{yMax}{Boolean dictating whether the bounds should be (0,1), FALSE, or should be set to fit the data, TRUE }
  \item{n}{The size of the population being sampled}
  \item{sub}{ (Optional) number of elements in the sample }
  \item{par}{A scalar or vector containing the required parameters for each of the distributions}
}
\details{
  This function produces one or two windows.  The Tk window, or the control window, is only produced when \code{gui=T}.  It contains a slider that controls how big a sample is being taken from the population.  The range of the slider is [1,n]

  The second window is a plotting window, feacturing a histogram overlain with a line plot.  The line plot is the true value of the distribution, and the histogram for the sample defined by the control window.

  There are several distributions currently supported by the function.  They are as follows (par indicates how there paramters should be entered).

  "norm":		par = c(mu,sig)

  "exp":		par=lambda

  "chisq":		par=c(df,ncp)

  "gamma":		par=c(shape,scale)

  "pois":		par=lambda
 
}
\value{
  No meaningful value is returned, this function is run only for its plotting functions.  The variable \code{HSenv} is left behind so the variables used in the plotting function may be maniuplated.
}
\note{
  The slider has a step size of 1: the value of the slider may manipulated by either clicking and draging the slider or clicking and holding the space to the left/right of the slider, which will decrease/increase the slider by the step size.

  The function was designed to work with the GUI.  The ability to plot without it was added to allow the function to be embedded into other programs such as Sweave.  Whenever possible, it is better to use the GUI controls.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\references{ This plot was designed for a course by Mohamed Abdolell }
\seealso{ \code{\link{rhoRange}} }
\examples{ 
  histSample()				# a normal mu = 0, sig = 1 sample
  histSample('norm',n=500,par=c(0,1))		# a normal mu = 0, sig = 1 sample again
  histSample('gamma',n=5000,par=c(3,2))	# a gamma distn, a = 3, b = 2

  #Producing just plots
  histSample(dist='norm',n=1000,par=c(10,5),gui=FALSE,sub=45)
}
\keyword{distribution}
