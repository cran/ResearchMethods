\name{ratioCompare}
\alias{ratioCompare}
\title{ A demonstration of the approximation of RR by OR }
\description{
  Using a Graphical User Interface (GUI), this function demonstrates how an odds ratio can be an accurate approximation of relative risk when the probability is low.
}
\usage{
ratioCompare(gui=TRUE, pointLabel=TRUE)
}
\arguments{
  \item{gui}{Whether to active the interface}
  \item{pointLabel}{Label the specific point}
}
\details{
  This function is rather straight forward, as it takes no arguements, and the corresponding control window only has two bars on it.  The idea is to demonstrate how, as both p1 and p2 approach 0, the odds ratio becomes a better approximation of the relative risk.

  The only thing of note on the plot is that the logs of the OR and the RR are plotted, rather than the values themselves.  This is because the range of both values is (0,Inf), so there is a problem of comparing them when the both approach 0.  To see this, set p1=0.05 and p2=0.95.  In this situation, the OR and the RR are 0.0028 and 0.0526 respectively, which is a large difference in terms of scale, but is not evident if their differences are taken at face value. 
}
\value{
  No value is returned, though the environment, RCenv, is left for the user to examine.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\keyword{design}
