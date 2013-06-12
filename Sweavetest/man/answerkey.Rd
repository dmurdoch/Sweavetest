\name{answerkey}
\alias{answerkey}
\title{
Generate an answer key for a test.
}
\description{
Each of the multiple choice functions appends the correct answer
to the global \code{\link{Index}} dataframe; this function displays the
correct answers in a table.
}
\usage{
answerkey(symbols = letters, group=5, perline=6)
}
\arguments{
  \item{symbols}{
What symbols to use to display the answers.
}
  \item{group}{
Group the answers into groups of this size.
}
\item{perline}{
Put at most this many groups per line.
}
}
\value{
This function is called for the side effect of writing Latex code
to display the table of correct answers.
}
\seealso{
\code{\link{things}} and \code{\link{horiz}} update the \code{Index} global.
}
\examples{
\dontrun{
answerkey(symbols=LETTERS)
}
}
\keyword{ utils }
