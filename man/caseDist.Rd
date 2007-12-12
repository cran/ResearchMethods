\name{caseDist}
\alias{caseDist}
\title{Effect of proportion changes on kappa}
\description{
  Using a graphical user interface (GUI) this function demonstrates the effects of changing proportion of points in a 2x2 classification table of kappa.
}
\usage{
  caseDist(pObs=0.9,gui=TRUE,row1=.5,col1=.5)
}
\arguments{
  \item{pObs}{The proportion of observations that are correct, ie, the proportion of points in entries [1,1] and [2,2]}
  \item{gui}{An indicator of whether the interface should be activated.}
  \item{row1}{The initial proportion of observations in the top row of the modified table.}
  \item{col1}{The initial proportion of observations in the first column of the modified table.}
}
\details{
  This function produces one or two windows.  The Tk window, or the control window, is produced on if \code{gui=T}.  It contains two slider bars, used to change the proportion of points in the table.  Both sliders have a range of [0.01,0.99], and represent the proportion of points in row x1 and y1 respectively.

  The second window is a plotting window, feacturing two tables.  The first table is the balanced data, and the second is the data shifted to the values specified by the sliders.  Below each plot is the pObs: the overall proportion of agreement, pExp, the expected proportion of agreement, calculated using the row and column totals, and k, or the chance corrected agreement.
}
\value{
  No meaningful value is returned, this function is run only for its plotting functions.  The variable \code{RRenv} is left behind so the variables used in the plotting function may be maniuplated.
}
\note{
  The slider has a step size of .02: the value of the slider may manipulated by either clicking and draging the slider or clicking and holding the space to the left/right of the slider, which will decrease/increase the slider by the step size.

  The function was designed to work with the GUI.  The ability to plot without it was added to allow the function to be embedded into other programs such as Sweave.  Whenever possible, it is better to use the GUI controls.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\references{ This plot was designed for a course by Mohamed Abdolell }
\seealso{ \code{\link{rhoRange}} }
\examples{ 
  # simple examples
  caseDist()
  caseDist(pObs=0.76)

  # producing the tables without the GUI
  caseDist(gui=FALSE,row1=.9,col1=.9)	#Like in the slides
  caseDist(.60,0,.7,.7)			# different plot
}
\keyword{univar}
