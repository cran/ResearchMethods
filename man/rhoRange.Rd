\name{rhoRange}
\alias{rhoRange}
\title{Size effects on rho calculations}
\description{
  Using a graphical user interface (GUI) this function demonstrates the effects of changing the range of a dataset on the calculation of correlation, rho.
}
\usage{
  rhoRange(x,y,gui=TRUE,xMin=NaN,xMax=NaN,perc=NaN)
}
\arguments{
  \item{x,y}{The two variables to calculate the correlations from, used for the x and y axes respectively}
  \item{gui}{ An indicator of whether the interface should be activated. }
  \item{xMin}{ (Optional) Specify the lowest x-value in the subset.}
  \item{xMax}{ (Optional) Specify the highest x-value in the subset.}
  \item{perc}{ (Optional) Specifiy the proportion about the mean to be included in the subset.}
}
\details{
  The purpose of this function is to demonstrate the effect of sample size on the calculation of rho.  With any of the options set, the program sets the subset to be roughly half the dataset, about the mean.  The effect of setting either of the end points is obvious, but note some ofthe problems in the Warnings.

  This function produces two windows.  The Tk window, or the control window, is only produced when \code{gui=T}, and provides controls for manipulating the plot in real time.  It contains three slider bars, which provide two different ways to change the range of the data.  The \code{Lowest} and \code{Highest} sliders let the user set the lowest and highest values on the x-axis within which the new rho value will be calculated.  The \code{proportion of points} slider allows the user to select what proportion of the points (centered at the mean) should be included in the new calculations.  Note that changing the proportion slider moves the \code{Lowest} and \code{Highest} sliders, but the opposite is, for now, not true.  Note also that, if the \code{Lowest} and \code{Highest} sliders are moved, and then the \code{proportion of points} slider is selected, the other two sliders will reset to the points corresponding to the current proportion.

  The second window is a plotting window, feacturing a scatterplot and two lines of best fit.  The red points, and the corresponding best fit line, are the points that fall within the selection range, manipulated in the Tk window.  With this line is a changing rho value, which corresponds to the correlation of the given subset.  The green points are the points in the dataset outside the selection window, which means that the green points combined with the red points form the entire dataset, and therefore the green line and the corresponding correlation are on the entire dataset.
}
\value{
  No meaningful value is returned, this function is run only for its plotting functions.  The variable \code{RRenv} is left behind so the variables used in the plotting function may be maniuplated.
}
\section{Warning}{
  Because it works in realtime, the function does not deal with large datasets well, ex, with the \code{agpop} dataset.  It may be more useful when working with datasets like these to take a random subset of the data.  For an example, see below

  The effect of setting any of the optional variables is different if the GUI is on or off.  Specifically, setting xMin and xMax with GUI on does not have much effect, because the GUI is defined by the percent slider.  Also note that setting all three variables (which is contradictory to begin with) has two different effects: with the GUI on, the \code{perc} variable dominates, but with the GUI off, the \code{xMin} and \code{xMax} variables dominate.  Because of these differences, it is best to only set one of the two options.
}
\note{
  This program was written on Ubuntu Linux 7.0.4.  Though it should work on all operating systems, because of its use of Tcl/Tk it is only guarenteed on Linux.  Any problems with the GUI should be checked against the Tcl/Tk documentation.

  The function was designed to work with the GUI.  The ability to plot without the GUI on and to change the initial values was added to allow the program to be embeded into programs such as Sweave.  Whenever possible, it is better to use the GUI controls.
}
\author{Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\references{ This plot was designed for a course by Mohamed Abdolell }
\seealso{ \code{\link{rhoScale}} }
\examples{ 
  # a simple example
  data("MFSV")
  rhoRange(MFSV$MF,MFSV$SV)

  #These two will produce only the plot
  rhoRange(MFSV$MF,MFSV$SV,gui=FALSE,xMin=20,xMax=75)
  rhoRange(MFSV$MF,MFSV$SV,gui=FALSE,perc=0.75)

  # working with a big dataset
  data("agpop")
  rhoRange(agpop[,6],agpop[,7])			#this will work, but will respond in real time too well
}
\keyword{univar}
