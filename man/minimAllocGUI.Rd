\name{minimAllocGUI}
\alias{minimAllocGUI}
\title{ The Minimization Alocation Technique }
\description{
  Using a Graphical User Interface, this function is used to allocate patients to treatment groups.  It can also write the results of the current allocation to a file, to be re-used at a later point.  The purpose of this function is to provide researchers with a simple, effective patient allocation program.
}
\usage{
  minimAllocGUI(factors=NULL,file=NULL,prob=1)
}
\arguments{
  \item{factors}{ an optional list object describing the factors by which the data should be stratified.  The list should have as its first element a vector of the names of the different factors, and for each factor, a subsequent list of levels.  In the examples, the object \code{f} is an example of the factors object. }
  \item{file}{ an optional file that contains the data from a previous allocation.  If no file is input then the output is written to \code{tempFile.dat}; with an input file, the data is written back to that file, with a backup of it being saved in tempFile.dat. }
  \item{prob}{ this is the probability that the new subject will be allocated to the group that has fewer patients.  Pocock's design had the probability set to 1, but the problem has evolved to include a probability so that the allocation of the next patient cannot be predicted. }
}
\details{
  This function produces two or three windows, depending on what the input is.  The first, and optional, window is a tcltk control window designed to allow the user to set the factors by which the patients will be stratified.  It contains two input lines: the first allows the user to set the name of the first factor, the second allows the user to set the levels of the named factor.  Pressing \code{continue} adds the current factor to the list of factors, pressing \code{Return Factors} returns the list of factors WITHOUT the one in the input lines.  This step may be skipped by including a factor list, such as f in the examples.

  Once the factors have been set, two more windows pop up.  The first is a tcltk control window that allows the user to set all the strata variables, and then allocate the patient.  Note that, if two buttons are checked in the same strata then both become unchecked, in order to avoid costly errors.  There are three buttons in this window: \code{Allocate} assigns a patient with the given attributes to a group.  The other two buttons allow the user to quit with or without saving the new results.  Note that quiting the program without using the buttons is equivalent to quitting without saving.

  The final window is the display of the allocation.  The left column is a list of the factors, with their levels in the second column, and then how many patients are allocated to each group.
}
\value{
  No formal values are returned, though a file is saved with the relevant data, either to \code{tempFile.dat} or the input file.
}
\references{ 
  Pocock, Stuart, J.  "Clinical Trials: A practical Approach".  John Wiley and Sons, Toronto: 1988.
}
\author{ 
  Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com>
}
\section{Warning}{
  Because of the way the function is programed, YOU CANNOT USE SEMICOLONS (;) in the names of the factors or the factor levels.
}
\examples{
# the most basic way to call the function is without params
minimAllocGUI()

# a list of factors can be used in the following way
f = list()
f[1][[1]] = c("Performance Status","Age","Disease-free interval","Dominant metastatic lesion")
f[2][[1]] = c("Ambulatory","Non-ambulatory")
f[3][[1]] = c("<50",">=50")
f[4][[1]] = c("<2 years",">=2 years")
f[5][[1]] = c("Visceral","Osseous","Soft Tissue")

minimAllocGUI(f)

# once a file has been saved, it may be recalled
\dontrun{minimAllocGUI(file="tempFile.dat")}
}
\keyword{design}
