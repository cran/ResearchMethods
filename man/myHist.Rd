\name{myHist}
\alias{myHist}
\title{Drawing simple histograms within a plotting area}
\description{
  This function is an internal function used to draw histograms within a plotting area.
}
\usage{
  myHist(xl, yb, xr, yt, hDat, breaks = NULL, sample = TRUE,CIs = FALSE, xlab = FALSE, label.bars = FALSE, right = TRUE)
}
\arguments{
  \item{xl,yb,xr,yt}{The left, bottom, right and top parts of the plot respectively.}
  \item{hDat}{The data to be plotted.}
  \item{breaks}{Where the break points should be.}
  \item{sample}{The subset of the dataset to be plotted.}
  \item{CIs}{Should CIs be plotted around the bars.}
  \item{xlab}{The x-axis label.}
  \item{label.bars}{The bar labels.}
  \item{right}{Should the plot be right-justified?}
}

\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\references{ This plot was designed for a course by Mohamed Abdolell }
\keyword{distribution}
