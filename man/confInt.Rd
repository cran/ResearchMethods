\name{confInt}
\alias{confInt}
\title{ Demonstrating How Confidence Intervals Work }
\description{
  Using a Graphical User Interface (GUI), this function demonstrates what exactly a confidence means, and how random samples will fall inside and outside of it.
}
\usage{
confInt(...)
}
\arguments{
  \item{...}{Additional arguments...}
}
\details{
  This function produces two windows: a Tk window to manipulate the variables, and a plotting window that plots the confidence intervals.  The TK window has controls to manipulate the mean and standard deviation of the normal distribution to be sampled from, as well as a slider to set the value of alpha.  It also has two entry boxes in which n, the size of the sample, and reps, the number of samples, can be set.  Note that changing any of the variables except alpha requires a re-sampling of the data, and will therefore completely change the plot.

  The second window is the plotting window.  In it the y-axis represents the sample, and the x-axis represents the repetitions.  Each point represents a mean of a sample, with the lines about it being it's own confidence interval.  The green lines represent the confidence interval given the parameters of the plot.  If a mean falls outside the green confidence intervals, it is marked in red.
}
\value{
  No meaningful value is returned, though the computational environment is retain in CIenv to allow the user to check some of the values plotted.
}
\author{Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\note{ 
  This function relies on the \code{plotCI} function in the gplots library.  To see a non-interactive version of the function, look at \code{ci.examp} in the TeachingDemos library.
}
\keyword{univar}
