\name{readScanex}
\alias{readScanex}
\title{
Read a Scanex file of scanned Scantron pages.
}
\description{
UWO uses a standard Scantron form for recording multiple choice tests.  The ITS department
will scan these and produce a fixed format file recording all answers.  This function
reads that file into a dataframe.
}
\usage{
readScanex(filename)
}
\arguments{
  \item{filename}{
  The file from ITS.
}
}

\value{
A dataframe with the following columns, all read as character vectors:
\item{Student ID}{The student number.}
\item{Section}{The class section number.}
\item{ExamCode}{The exam code indicating the version of the test.}
\item{Marker}{A marker which must mean something...}
\item{Scantron}{The sequence number of the Scantron sheet.  This number is stamped onto
the Scantron sheet as it is read, so it can be used to retrieve the original in case
it needs to be checked for errors.}
\item{Answers}{The coded answers read from the Scantron.}
}
\author{
Duncan Murdoch
}

\seealso{
\code{\link{grades}}, \code{\link{wrongKey}}
}
\keyword{ utils }
