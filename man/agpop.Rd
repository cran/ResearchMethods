\name{agpop}
\alias{agpop}
\title{Farming Data}
\description{
  The results from the US 1992 Census of Agriculture
}
\arguments{
  \item{agpop}{A dataset with 3078 entries and 15 variables:}
  \item{$county}{County name}
  \item{$state}{state abbreviation}
  \item{$acres92}{Number of acres devoted to farms in 1992 (-99=missing)}
\item{acres87}{Number of acres devoted to farms in 1987 (-99=missing)}
\item{acres82}{Number of acres devoted to farms in 1982 (-99=missing)}
\item{farms92}{Number of farms in 1992 (-99=missing)}
\item{farms87}{Number of farms in 1987 (-99=missing)}
\item{farms82}{Number of farms in 1982 (-99=missing)}
\item{largef92}{Number of farms, with 100 acres or more, in 1992 (-99=missing)}
\item{largef87}{Number of farms, with 100 acres or more, in 1987 (-99=missing)}
\item{largef82}{Number of farms, with 100 acres or more, in 1982 (-99=missing)}
\item{smallf92}{Number of farms, with 9 acres or less, in 1992 (-99=missing)}
\item{smallf87}{Number of farms, with 9 acres or less, in 1987 (-99=missing)}
\item{smallf82}{Number of farms, with 9 acres or less, in 1982 (-99=missing)}
\item{region}{S=South, W=west, NC=north central, and NE=northeast}
}
\details{
  As stated above, a -99 entry indicates a missing value.  To replace the missing values with NA, use the example code below.
}
\seealso{ \code{\link{MFSV}} }
\examples{
  #to call the dataset
  data("agpop")					#note the quotation marks
}
\keyword{datasets}
