\name{grades}
\alias{grades}
\title{
Compute grades for all students based on Scantron responses.
}
\description{
This marks all Scantrons with a list of answer keys based on an exam code. 
Scores can be adjusted to give different marks for correct, blank and wrong
answers.
}
\usage{
grades(scanex, key = scanex[scanex$"Student ID" == "999999999", ], correct = 1, blank = 0, wrong = 0)
}
\arguments{
  \item{scanex}{
  A file of Scantron lines.
}
  \item{key}{
  Scantron lines with the known correct answer key.
}
  \item{correct}{
  The score to give for correct answers.  
}
  \item{blank}{
  The score to give for blank answers.
}
  \item{wrong}{
  The score to give for wrong answers.
}
}
\details{
  By default, \code{correct}, \code{blank} and \code{wrong} are scalars and are
  used for all questions.  However, vectors may be used; they will be recycled as
  necessary to the length of the first answer key.
  
  If multiple answer keys are given for a given exam code, an answer will be marked
  correct if it matches any of the keys.
}
\value{
A data frame with the same rows as the \code{scanex} input.  Columns will 
include the original columns as well as
 \item{Correct}{The answer key for the matching exam code.}
 \item{Grade}{The computed grade.}
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{wrongKey}} for detecting errors and cheating.
}
\keyword{ utils }
