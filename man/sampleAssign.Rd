\name{sampleAssign}
\alias{sampleAssign}
\title{ A demonstration of different patient allocation methods }
\description{
  Using a Graphical User Interface (GUI), this function demonstrates to the user different ways to allocate patients to different treatments.  The methods are taken from Pocock's book referenced below.
}
\usage{
sampleAssign(gui = T, n = 30, treat = 3, coinProb, type = 0, ...)
}
\arguments{
  \item{gui}{An indicator of whether the interface should be activated}
  \item{n}{Number of patient to allocate}
  \item{treat}{Number of treatments to allocate patients to}
  \item{coinProb}{The probability for the biased coin}
  \item{type}{The type of allocation to use: 0=simple allocation, 1=random permuted blocks, 2=Biased Coin}
  \item{...}{Additional arguements...}
}
\details{
  This function produces two windows.  The first window is the control window, from which the user can select one of the following three allocation methods: Simple Randomization, Random Permuted Blocks, Biased Coin.  For each of the methods, the number of patients being allocated, \code{n} can be set, and for the first two, the number of treatments can also be set.  Note that Biased Coin can only take two treatments.

  The second window is the plotting window, which displays what treatment each patient is allocated to.  The first patient is in the top-right corner, and the patients read across as normal.  The color coding for the first two methods is self explanatory.  For the Biased Coin method, treatment A is colored as red and B as blue.  The light red and light blue indicate that the patient is allocated to the same treatment, but they are allocated there because of the biased coin, ie, the light red patients would be blue if a simple randomization scheme were used.
}
\value{
  No value is returned, though the environment, SAenv, is left for the user to examine.
}
\references{  }
\author{ Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\keyword{design}
