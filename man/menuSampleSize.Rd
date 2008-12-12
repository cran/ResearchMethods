\name{menuSampleSize}
\alias{menuSampleSize}
\title{Sample Size Caclulations}
\description{
  This function calculates required sample sizes for a variety of data types, and allows for the interactive manipulation of these calculations.
}
\usage{
menuSampleSize(...)
}
\arguments{
  \item{...}{Additional arguements...}
}
\details{
  Using a GUI, this function calculates the sample size required for a variety of sampling designs.  See the file \code{functionList.pdf} for a detailed description of the equations.
}
\note{
  Because of the way R libraries are compiled, greek characters and subscripts could not be built into the function.  To convert the menu to greek characters with subscripts, open the file \code{menuSampleSize.R} and uncomment the 11 commented lines at the beginning of the file.  Note that THIS IS NOT NECESSARY for the program to work correctly, but it does make the menu clearer and easier to relate to the supporting document, \code{functionList.pdf}.
}
\references{
\url{../doc/functionList.pdf}
}
\author{  Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\keyword{univar}
