\name{caseRatio}
\alias{caseRatio}
\title{ The effect of case:control ratios on power }
\description{
  Using a Graphical User Interface (GUI), this function demonstrates the effect of increasing the proportion of patients allocated to the test treatment in a case:control study.
}
\usage{
  caseRatio(GUI=TRUE,alpha=0.05,beta=0.1)
}
\arguments{
  \item{GUI}{ A boolean determining allowing the GUI to be removed from the function, allowing the plot to be embedded into other code. }
  \item{alpha}{ The significance level for the experiment. }
  \item{beta}{ The initial beta value for the experiment. }
}
\details{
  This function produces one or two windows.  The optional window is the control window, which allows the user to set the alpha and beta values, and manipulate in real time what proportion of the patients are allocated to the treatment option, rather than the control option.  The second window is the plotting window, in which the curve is the possible change in power as the proportion allocated to the treatment increases.  The point at which the proportion is currently set is indicated by the two red lines.
}
\value{
  No value is returned by this function, although the function environment, CRenv, is left so that the values may be retrieved.
}
\references{ Pocock, S. J.  Allocation of patients to treatment in clinical trials.  Biometrica, 1979.  35, 183-197. }
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\examples{
caseRatio()
#without the GUI
caseRatio(GUI=FALSE)
#with new initial values
caseRatio(alpha=0.01,beta=0.2)
}
\keyword{survival}
