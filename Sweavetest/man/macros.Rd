\name{macros}
\alias{newCommands}
\alias{marklist}
\alias{multiplechoice}
\title{
Write out commonly used macros.
}
\description{
This writes out the Latex definitions of some commonly used macros.
}
\usage{
newCommands()
marklist(marks, names=1:length(marks), testversion)
multiplechoice(names) 
}
\arguments{
\item{marks}{
Numerical values of the marks to be assigned to the questions.
}
\item{names}{
The \dQuote{names} of the questions associated with the marks or answers.
}
\item{testversion}{
Which version of the test to use?  Normally this is taken from the global
variable of the same name.
}
}
\details{
Run the example to see what macros are included in the list printed by \code{newCommands}.

The \code{marklist} function prints a box suitable for recording marks on a list of questions.

The \code{multiplechoice} function prints a box suitable for recording multiple choice answers.
}
\value{
Called for the side effect of writing the macros.
}
\examples{
newCommands()
marklist(rep(5,4), 1:4)
multiplechoice(1:4)
}
\keyword{ utils }
