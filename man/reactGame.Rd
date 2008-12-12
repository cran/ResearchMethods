\name{reactGame}
\alias{reactGame}
\title{A game to create data for two-way Anovas'}
\description{
  Using a graphical user interface (GUI) this function demonstrates the effects of changing the range of a dataset on the calculation of correlation, rho.
}
\usage{
  reactGame()
}
\details{
This game allows the user to create a dataset of reaction times that can be analyzed using 2-way Anova.

The function produces two windows, a tcltk control window and then the game window in the plotting area.

The plotting window contains a number of options: first there is a space to enter and ID.  This can be a number or a character, and is used to track participants in the final analysis.  There are two sets of buttons to select whether you are going to click on red or green targets and whether the background should be white or should be colored a similar color to the target.  Finally there is an option ot either practice, or to save your results to a file.  If the named file already exists then the results will be appended to it.

Once the "Play Game!" button is pressed then the plotting window pops up, prompting the user to get ready.  Clicking on this screen once starts the game.  Once into the game the objective is to click on the screen as soon as you see a target (DO NOT CLICK BEFOREHAND, the resulting data will not make any sense).  This will iterate 10 times, after which, if the results were saved, the ANOVA results will be reported.  Note that these results can be unpredictable until there is at least one set observations for each of the four combinations.
}
\section{Warning}{
This function does not seem to work with the Quartz window manager on the Mac, and has not been tested on Windows, so the function call should be preceeded with the command \code{X11()} to use the X11 window manager instead.
}
\author{Mohamed Abdolell <mohamed.abdolell@dal.ca> and Sam Stewart <samstewart11@gmail.com> }
\references{ This plot was designed for a course by Mohamed Abdolell }
\keyword{univar}
