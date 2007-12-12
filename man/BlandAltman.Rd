\name{BlandAltman}
\alias{BlandAltman}
\title{BlandAltman Plot}
\description{
  Using a graphical user interface (GUI) this function performs a Bland Altman plot, and allows for manipulation of the variables within the plot.
}
\usage{
BlandAltman(x, y, gui=TRUE, bandsOn=FALSE, biasOn=FALSE, regionOn=FALSE, sig=2)
}
\arguments{
  \item{x}{ The observations from the old measurement technique. }
  \item{y}{ The observations from the new measurement technique. }
  \item{gui}{ An indicator of whether the interface should be activated. }
  \item{bandsOn}{ An indicator of whether the confidence bands should be plotted. }
  \item{biasOn}{ An indicator of whether the bias region should be plotted. }
  \item{regionOn}{ An indicator of whether the region of agreement should be plotted. }
  \item{sig}{Initial value of the scalar on sigma}
}
\details{
  The Bland-Altman plot is a visual tool for comparing two different methods of measuring the same value, when the true value being measured is not known.  The purpose of a Bland Altman plot is to try and determine whether a new method of measurement is better than an established one, using a hypothesis testing approach.  See the referenced papers for more information.

  This function produces one or two windows.  The Tk window, or the control window, is only produced when \code{gui=T}, and provides controls to manipulate the plot in real time.  The control window contains three check buttons and a slider bar, which provide the manipulation of the Bland Altman plot.  The top button adds the confidence bounds, the second adds the bias line, and the third adds the region of agreement.  Note that the bias line and region of agreement will not work unless the confidence bounds are on.  The slider bar allows for manipulation of the confidence bounds: the standard confidence region is 2 x sigma, but this is strictly a statistical standard, and there may be a desire to widen of shrink the confidence region given the specific data.  The slider bar has a range of [0.1, 5] within which the multiplier on sigma may be set.

  The second window is the plotting window, featuring a scatter plot of the Bland Altman data.  See the referenced Bland Altman paper for a more detailed explanation of the Bland Altman process: a brief explanation is that the points are plotted with the difference between two observations on the y-axis, and the mean of the two observations on the x-axis.  The confidence bounds, when added, are plotted as dotted red lines, and all points within the confidence bounds are coloured green, all points outside the confidence bounds in red.  The bias region, plotted as an oranged field, is the range between the difference = 0 line and the average difference, and the region of agreement is merely a green shading of the area within the confidence bounds.
}
\value{
  No meaningful value is returned, this function is run only for its plotting functions.  The variable \code{BAenv} is left behind so the variables used in the plotting function may be manipulated.
}
\note{
  The slider has a step size of .1: the value of the slider may manipulated by either clicking and draging the slider or clicking and holding the space to the left/right of the slider, which will decrease/increase the slider by the step size.

  The function was designed to work with the GUI.  The ability to plot without it was added to allow the function to be embedded into other programs such as Sweave.  Whenever possible, it is better to use the GUI controls.
}
\references{ Altman, Douglas G. and Bland, J. Martin.  "Statistical Methods For Assessing Agreement Between Two Methods of Clinical Measurement".  Lancet, 1986, i:307-310. }
\author{Mohamed Abdolell <mohamed.abodolell@dal.ca> and Sam Stewart <samstewart11@gmail.com>}
\seealso{\code{\link{MFSV}}}
\examples{
  #a simple example
  data("MFSV")
  BlandAltman(MFSV$MF,MFSV$SV)

  # This will only produce a BlandAltman Plot with the region of agreement added
  BlandAltman(MFSV$MF,MFSV$SV,gui=FALSE,bandsOn=TRUE,regionOn=TRUE)

  #this example does not make sense in the context of the function
  #but does provide a dataset with a large bias-value
  data("agpop")
  BlandAltman(agpop$farms87,agpop$farms92)
}
\keyword{design}
