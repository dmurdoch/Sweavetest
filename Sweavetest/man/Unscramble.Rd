\name{Unscramble}
\alias{Unscramble}
\title{
Undo Question Randomization for Use in Low Level Report
}
\description{
Using correspondences provided by the instructor, this function will order all
questions from 1 to the last question, and will export a new updated scanex variable
for use in further analysis.
}
\usage{
Unscramble(scanex, Orders)
}

\arguments{
  \item{scanex}{
The original Scantron file in an unmodified format
}
  \item{Orders}{
A matrix or dataframe recording the order of the questions for each exam code.  See Details.
}
}
\details{
The \code{Orders} matrix contains one column per exam code, giving the question numbers used
in this exam from the \dQuote{master} exam.  (Often the first exam code is taken to be the
master, in which case the first column will contain \code{1:n} for \code{n} questions; the
\code{i}th row of another column will contain the corresponding question on the master.)
}
\value{
This function returns the Scantron file back to the user, but with answers in the order specified.
}
\author{
Adam Rahman and Duncan Murdoch
}

\examples{

scanex <- readScanex(system.file("sample/sample.dat", package="Sweavetest"))
scanex <- scanex[1:8,]
scanex

Orders <- data.frame("170"=c(1,2,3,4), "738"=c(2,3,4,1), "840"=c(3,4,1,2), "967"=c(4,1,2,3),
                     check.names = FALSE)

Unscramble(scanex, Orders)

}
