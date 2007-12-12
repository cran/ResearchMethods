\name{multHypothesis}
\alias{multHypothesis}
\title{ The effect of multiple hypothesis testing }
\description{
  Using a tcltk Graphical User Interface (GUI), this function demonstrates the pontential dangers involved in testing multiple hypotheses with dependent data.
}
\usage{
  multHypothesis(GUI=TRUE,alpha=0.05,hyp=1)
}
\arguments{
  \item{GUI}{ A boolean value that lets the user turn the control window off, to allow the function to be embeded in other code. }
  \item{alpha}{ The initial significance level. }
  \item{hyp}{ The initial number of hypotheses being tested. }
}
\details{
  The function creates one or two windows.  The optional GUI window allows the user to set the significance level, alpha, and to change in real time the number of hypotheses being tested, k.  The other window is a plot of the potential chance of a false rejection of the null hypothesis, with indicators marked in red of the exact probability at the current number of tests.
}
\value{
  No tangible value is returned, though the enviroment \code{MHenv} is left so the user can access the variables.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\examples{
# The simplest way to run the function, with the GUI active
multHypothesis()

#running it without a GUI
multHypothesis(GUI=FALSE)

#Starting with initial values
multHypothesis(alpha=0.1,hyp=10)
}
\keyword{distribution}
