\name{wrongKey}
\alias{wrongKey}
\title{
Function to detect Scantrons with the wrong exam key
}
\description{
This remarks all Scantrons with all possible keys.  If one has had the wrong exam code entered,
it will score better using the answer key for that exam.  Similarly, if a student has
copied another student with a different exam version, they may score much better
on another key.
}
\usage{
wrongKey(scanex, key = scanex[scanex$"Student ID" == "999999999", ])
}
\arguments{
  \item{scanex}{
  A file of Scantron lines.
}
  \item{key}{
  Scantron lines with the known correct answer key.
}
}
\value{
A data frame with the same rows as the \code{scanex} input.  Columns will 
include the original columns as well as
 \item{Grade}{The count of correct answers under the stated exam code.}
 \item{Badcount}{The count of incorrect answers which would be correct using the best
 of the other answer keys.}
 \item{Badkey}{A copy of the best of the other answer keys.}
 \item{Badcode}{The exam code of the best of the other answer keys.}
}
\author{
Duncan Murdoch
}
\note{
An unusually high value of \code{Badcount} for a given \code{Grade} level is a sign
of either cheating or a mis-coded exam code.  If seating arrangements are known,
it might be worthwhile to include neighbouring students' answer keys as other
possible keys.
}
\seealso{
\code{\link{readScanex}} to read the \code{scanex} file, \code{\link{grades}}
for standard grading.
}
\keyword{ utils }

