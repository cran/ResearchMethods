\name{outlierTest}
\alias{outlierTest}
\title{Effects of removing outliers on regression fits}
\description{
  Using the \code{identify} function in R, this function demonstrates the effects that outliers can have on regression by allowing them to be removed in real time.
}
\usage{
  outlierTest(y,x,output=FALSE)
}
\arguments{
  \item{y}{The response variable for the linear regression}
  \item{x}{The predictor variable for the linear regression}
  \item{output}{a boolean deciding whether to print a summary of each new model to the R terminal}
}
\details{
  This function takes the input vectors \code{y} and \code{x} and performs a simple linear regression using the code \code{lm(y~x)}.  Then, using the \code{locator()} function it allows the user to point and click on the plot to add or remove modifiers from the dataset.  Data points in the model will be colored blue, points excluded will be colored green, and new points will be colored red.
}
\note{
  The function breaks down once the dataset is reduced to one point.
}
\author{Mohamed Abdolell <mohamed.abdolell@dal.ca and Sam Stewart <samstewart11@gmail.com> }
\references{ This plot was designed for a course by Mohamed Abdolell }
\seealso{ \code{\link{identify}} }
\examples{
\dontrun{ 
  data(MFSV)
  outlierTest(MFSV[,1],MFSV[,2])		# simplest test
  outlierTest(MFSV[,2],MFSV[,2],output=TRUE)	# see the details of each successive model
}
}
\keyword{regression}
