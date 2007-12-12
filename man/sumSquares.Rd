\name{sumSquares}
\alias{sumSquares}

\title{A Graphical Demonstration of Least Squares Regression}
\description{This function demonstrates the idea of simple logistic
  regression graphically, and allows the user to manipulate the line of
  best fit (the regression line) and see its effect on the error sum of
  squares.
}
\usage{
  sumSquares(x,y)
}
\arguments{
  \item{x}{A vector containing the covariates for the regression.}
  \item{y}{A vector containing the responses for the regression.}
}
\details{
  The function performs a simple linear regression in the form y = b0 +
  b1x.  The result is three plots: the top left is a simple plot of the
  data points with the regression line fitted.  The top right plot is
  the same plot, but with squares drawn to connect each point to the
  fit.  These squares can be thought of as visualizations of the
  residuals squared, which is how the Residual Sum of Squares is
  calculated.  The bottom box prints individual sums of squares for some
  of the datapoints, and at the bottom prints the total sum of squares.

  The top left box is the "Active" box, in that it has the regression
  line that can be manipulated.  Both the intercept and the slope can be
  changed by clicking on the line.  If the line is clicked near the
  y-axis then the regression line will turn blue, indicating that the
  \emph{intercept} is being changed.  Clicking anywhere on the plot
  after the line turns blue will cause the intercept to change so that
  the new regression line will hit that point.  Clickinh on the line
  away from the y-axis will cause the line to turn red, indicating that
  the \emph{slope} is being changed.  Again, click on the plot and the
  slope will change so that the new regression line will hit that point.

  The point of this function is to notice that, regardless of how the
  line is changed, a smaller sum of squares cannot be attained, and to
  demonstrate the idea of residual sum of squares graphically.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart
  <samstewart11@gmail.com> }
\keyword{data}
