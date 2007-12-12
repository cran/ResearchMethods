\name{GUIdemo}
\alias{GUIdemo}
\docType{package}
\title{
  Using GUIs for R-ploting functions: GUIdemo
}
\description{
  Using Tcl/Tk to create Graphical User Interfaces (GUIs), this package is designed to provide real time controls to basic statistical plotting functions in order to help with the teaching of these methods.  The package was originally designed to accompany a research methods course for the Radiology Department at Dalhousie University.
}

\details{
  \tabular{ll}{
    Package: \tab GUIdemo\cr
    Type: \tab Package\cr
    Version: \tab 1.0\cr
    Date: \tab 2007-07-10\cr
    License: \tab What license is it under?\cr
  }
  
  The functions in the package take very few variables, and are most often implemented using the form \code{function(x,y)} or \code{function(dat)} where x and y are vectors, and dat is a matrix, most often n x 2, or a vector.  Calling the function initiates two windows: a plotting window and a Tk control window, which will manipulate the corresponding plot.

  The function depends heavily on the Tcl/Tk library in R, which is poorly documented and depends heavily on the Tcl/Tk language itself.  Because of this, how the boxes appear and how they function is not guarenteed to work exactly the same on every machine.  Care has been taken to test the products on three different operating systems, Windows, Mac OS and Ubuntu Linux, but the results are not guarenteed.  Any problems should first be checked against the Tcl/Tk documentation, and then directed to the contact person below.
}
\author{
Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart 

Maintainer: Sam Stewart <samstewart11@gmail.com>
}
\references{
  "Tk Commands Manual".  http://www.tcl.tk/man/tcl8.4/TkCmd/contents.htm.  Last accessed July 17, 2007.
}
\keyword{ package }
