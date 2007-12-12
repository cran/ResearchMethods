\name{transRegression}
\alias{transRegression}
\title{ Transforming regressions to satisfy assumptions }
\description{
  Using a Graphical User Interface (GUI), this function allows the user to either transform y or weight x in an attempt to make the regression equation fit the underlying assumptions.
}
\usage{
  transRegression(y,x,weights=rep(1,length(y)))
}
\arguments{
  \item{y}{ The response variable. }
  \item{x}{ The predictor variable. }
  \item{weights}{The vector of weights to be applied to x.}
}
\details{
  The purpose of this function is to demonstrate the effect of transforming y or weighting x to fit a regression model.  Note that this only works for a single covariate.  The first window is a control window, from which transformations on y or x can be performed.

  The second window contains two plots: the first is a standard regression plot, and the second is the plot of the residuals against the fitted values.
}
\value{
  No value is returned, though the environment, TRenv, is left to access.
}
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\keyword{regression}
