\name{horiz}
\alias{horiz}
\alias{enumerate}
\alias{items}
\alias{Rhoriz}
\alias{Renumerate}
\alias{Ritems}
\alias{resultenumerate}
\alias{resultitems}
\alias{things}
\alias{msg}
\alias{print.msg}
\title{
Items in multiple choice.
}
\description{
Output Latex for items in multiple choice.
}
\usage{
horiz(..., Correct = 1, KeepLast = 0, CheckDups, testversion, randomize, itemlabels, Answers)
enumerate(..., Correct = 1, KeepLast = 0)
items(..., Correct = 1, KeepLast = 0, CheckDups, Answers)

Rhoriz(..., Correct = 1, KeepLast = 0)
Renumerate(..., Correct = 1, KeepLast = 0)
Ritems(..., Correct = 1, KeepLast = 0)

resultenumerate(..., Correct = 1, KeepLast = 0, strip = TRUE)
resultitems(..., Correct = 1, KeepLast = 0, strip = TRUE)

things(..., Correct = NA,  KeepLast = 0, testversion, randomize, Answers)

msg(string)
\S3method{print}{msg}(x, ...)
}
\arguments{
  \item{\dots}{
Strings to be used as the individual items.
}
  \item{Correct}{
Which item is correct?
}
  \item{KeepLast}{
How many items (e.g. \dQuote{None of the above}) to keep in the final position?
}
  \item{CheckDups}{
Check for duplicate items.  Defaults to \code{TRUE},
or the value of the global variable \code{CheckDups}.
}
  \item{testversion}{
Which of the four versions of the test should we print?  Defaults to \code{1},
or the value of the global variable \code{testversion}.
}

\item{randomize}{
Should the items be printed in a random order?  Defaults to \code{FALSE},
or the value of the global variable \code{randomize}.
}

\item{itemlabels}{
What to use to label items.  Default is \code{paste("(", letters, ")", sep="")}.
}
\item{strip}{
Strip extra white space from printed results?
}
\item{Answers}{
The vector of previous correct answers; defaults to the global variable
of the same name.
}
\item{string}{The message to print.}
\item{x}{A \code{msg} object.}
}
\value{
Run for the side effect of producing Latex output.  As a second side effect,
the global variable LastIndices will be created, holding the answers
in the order as displayed (i.e. the items will be collected from \code{...}
into a vector \code{items}, and they will be displayed in the order
\code{items[LastIndices]}.

\code{horiz} puts the items
horizontally. 

\code{enumerate} puts them within an \verb{enumerate} environment;
\code{items} just adds the \verb{\items} prefix to each line.

\code{Rhoriz}, \code{Renumerate}, and \code{Ritems} are like the
above, but format the items as if they are \R code.

\code{resultitems} displays the items as R results, while
\code{resultenumerate} wraps them in an \verb{enumerate} environment.

\code{things} just permutes the strings passed to it.  It assumes any transformations
to obtain Latex have already been done.

Use \code{msg} to display text among the items in \code{resultitems}.
}
\examples{
 horiz(1,2,3,4)
 Rhoriz("x <- 1", 1+2, "plot(x)")
 items(1,2,3,4)
 Ritems("x <- 1", 1+2, "plot(x)")
 resultitems(1,2,3,msg("The fourth item"))
 things(1,2,3,4)
}
\keyword{ utils }
