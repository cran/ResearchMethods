\name{rocCurve}
\alias{rocCurve}
\title{ A GUI based classification function using a ROC curve }
\description{
  Using a Graphical User Interface (GUI), this function is used to calculate the best classification of data using a ROC curve, and displays the results in ways that demonstrate how a ROC curve works.
}
\usage{
   rocCurve(x = rnorm(25,10,1), y = rnorm(25,11,2))
}
\arguments{
  \item{x}{ The data who's true value is to be classified as 0. }
  \item{y}{ The data who's true value is to be classified as 1. }
}
\details{
  The ROC curve is used to evaluate the classification of numerical values as belonging to one of two distinct factors.  In this function, the factors are represented as 1 and 0.  Note that the function only works if the 0 values are in the x-vector and the 1 values are in the y vector.

  The function creates two windows.  The first window is the control window, which allows the user to change the value of the cutoff, ie, allows the user to set the point above which all values are classified as 1 (y) and below which all values are classified as 0 (x).

  The second window contains three plots.  The plot in the top left corner is the ROC curve itself.  The points indicate indicies at which the sensitivity and specificity change.  For more information on how a ROC curve works, consult the references.  The red point on the plot is the current cutoff, and the purple line points to the best cutoff point found.

  The plot in the top right corner is a simple classification table.  Reading from top left to bottom right, the numbers represent: classified correctly as x, classified incorrectly as y, classified incorrectly as x, classified correctly as y.

  The final plot at the bottom is a plot of the data points themselves.  The points on the bottom are the x values and the points on the top are the y's.  The green line represents the current cutoff point, and the purple line indicates the best cutoff point.
}
\value{
  This function does not return a value, but leaves the operating environment, RCenv, to allow the user access to the data.
}
\references{
  Rosner, Bernard.  Fundamentals of Biostatistics, 6th Ed.  Thompson Nelson: Toronto, 2006.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\examples{
  # If the function is run without any parameters, then it creates dummy vectors
  rocCurve()
  # Assuming x and y are the appropiate vectors to be classified
  \dontrun{rocCurve(x,y)}
  # Note that rocCurve(y,x) will work very poorly.
}
\keyword{design}
