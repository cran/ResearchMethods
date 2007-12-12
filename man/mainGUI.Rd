\name{mainGUI}
\alias{mainGUI}
\title{ GUI based command window }
\description{
  Using this function, all the other interactive functions in the GUIdemo library can be run.
}
\usage{
mainGUI(...)
}
\arguments{
  \item{...}{Additional arguements...}
}
\details{
  The function produces a tk window that has a list of the functions, along with a "Run" button, and two entry windows for x and y.  The purpose of the entry windows is to allow the user to input the x and y vectors required for certain ploting functions: these inputs may be complex, as is the default, but x and can also be defined in the main R environment and then recalled with their variable names.
}
\author{Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com>}
\keyword{environment} 
