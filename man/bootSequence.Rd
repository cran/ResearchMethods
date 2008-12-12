\name{bootSequence}
\alias{bootSequence}
\title{A demonstration of how bootstrapping works, taking multiple bootstrap samples and watching how the means of those samples begin to normalize.}
\description{
  This function uses a graphical user interface (GUI) to demonstrate how bootstrapping works, by bootstrapping from a submitted dataset sequentially and watching the sampled means move towards a normal distribution.  The confidence intervals also narrow.
}
\usage{
  bootSequence(dat)
}
\arguments{
  \item{dat}{the dataset to sample from.}
}
\details{
This function produces two windows. The Tk window, or the control window, contains three sampling buttons: the top button takes a single bootstrap sample, and the bottom two buttons take 10 and 100 bootstrap samples.

The second window is a visual demonstration of bootstrapping.  There are three windows: the top-left window is the histogram of the submitted dataset.  The top right window is a histogram of the most recent bootstrap sample (for the 10 and 100 size samples, this plot is blank), and the bottom plot is a histogram of the means of the sample.

The histogram at the bottom has the 95\% confidence intervals, calculated as the 2.5\% and 97.5\% qunatiles of the bootstrapped means.  The blocks outside the confidence interval are coloured red.
}
\value{
  No value is returned.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\references{ This function was designed for a course by Mohamed Abdolell }
\seealso{ \code{\link{bootSingle}} }
\examples{ 
  data(agpop)
  bootSequence(agpop$farms92)
  data(MFSV)
  bootSequence(MFSV$MF)

}
\keyword{survey}
