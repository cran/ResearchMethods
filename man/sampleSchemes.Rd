\name{sampleSchemes}
\alias{sampleSchemes}
\title{ Demonstrate different sampling techniques }
\description{
  Using a Graphical User Interface (GUI) this function demonstrates the differences between several different sampling methods
}
\usage{
sampleSchemes(gui=TRUE, sampleType = 0, sampleSize = 30, clusterSize = 5, ...)
}
\arguments{
  \item{gui}{ An indicator of whether the interface should be activated}
  \item{sampleType}{Type of sampling to perform: 0=Simple random sampling, 1=Stratified by dorm, 2=Stratified by student type, 3=Cluster sampling by floor, 4=Synthetic sample}
  \item{sampleSize}{Number of students to sample}
  \item{clusterSize}{Number of clusters to sample or step size for synthetic sample}
  \item{...}{Additional arguements...}
}
\details{
  This function produces two windows.  The control window is a Tk window in which the type and size of sample can be specified.  For the simple and stratified samples, the sample size n is taken as input.  For the cluster sample, the number of clusters is taken, and for the synthetic sample, the number taken is how many rooms to skip before taking the next room.

  The second window is a plot of the rooms, with the red rooms being the ones sampled.
}
\references{ Heckard, Robert F., and Utts, Jessica M.  Mind on Statistics.  Thompson Brooks/Cole. Canada: 2007. }
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\note{
  For the stratified random samples, if the n value doesn't divide evenly between the strata, then the extra n-values are assigned randomly, so some strata may have 1 more/less than others.

  Also, for the synthetic sample, the starting point is randomly chosen on the first floor of the first building.  the buildings are processed top to bottom, left to right, but within the buildings they are sample starting at the left side of the first floor.
}
\keyword{univar}
