\name{rhoScale}
\alias{rhoScale}
\title{Scaling and Shifting rho calculations}
\description{
  Using a graphical user interface (GUI), this function demonstrates the effect of scaling and shifting a dataset has on its correlation, rho, and its inter-class correlation, icc, on another dataset.
}
\usage{
  rhoScale(x,y,gui=TRUE,sc=1,sh=0,xlab='x',ylab='y')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{The two variables from which the correlation is calculated, used as the x and y axes respectively in the plot.}
  \item{gui}{ An indicator of whether the interface should be activated. }
  \item{sc}{ The initial scaling value. }
  \item{sh}{ The initial shiting value. }
  \item{xlab,ylab}{What to label the plots with.}

}
\details{
  This function produces one or two windows.  The Tk window, which only appears if \code{gui=T} produces two sliders to manipulate the plot window.  \code{Scale} has a range of [0.1, 5] and is the multiplier on the x-variable, labeled \code{a} (see below).  \code{Shift} has a range of 25\% of the data range, about the mean, and is the shift on the x-variable, labeled \code{b} (see below).

  The second window is a plotting window, featuring two scatterplots.  The first, the black plot, is of the original data, of the form \code{y = x}, with a line of best fit through it.  The second, the red plot, is the data shifted and scaled, of the form \code{Y = aX + b}.
}
\value{
  No meaningful value is returned, this function is run only for its plotting functions.  The variable \code{RSenv} is left behind so the variables used in the plotting function may be maniuplated.
}
\references{
  This plot was designed for a course by Mohamed Abdolell 
}
\author{
  Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> 
}
\note{ 
  This program was written on Ubuntu Linux 7.0.4.  Though it should work on all operating systems, because of its use of Tcl/Tk it is only guarenteed on Linux.  Any problems with the GUI should be checked against the Tcl/Tk documentation.

  The function was designed to work with the GUI.  The ability to plot without it was added to allow the function to be embedded into other programs such as Sweave.  Whenever possible, it is better to use the GUI controls.
}
\seealso{ 
  \code{\link{rhoRange}} 
}
\examples{
  # two simple examples
  data("MFSV")
  rhoScale(MFSV$MF,MFSV$SV)
  data("agpop")
  rhoScale(agpop[,6],agpop[,7])

  # working without the GUI
  rhoScale(MFSV$MF,MFSV$SV,gui=FALSE,sc=2,sh=25)
}
\keyword{univar}
