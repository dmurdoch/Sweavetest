\name{readScanex}
\alias{readScanex}
\alias{writeScanex}
\title{
Read or write a Scanex file of scanned Scantron pages.
}
\description{
UWO uses a standard Scantron form for recording multiple choice tests.  The ITS department
will scan these and produce a fixed format file recording all answers.  \code{readScanex}
reads that file into a dataframe, and \code{writeScanex} writes the correct
records into a file of the same format.
}
\usage{
readScanex(filename)
writeScanex(name)
}
\arguments{
  \item{filename}{
  The file from ITS.
}
  \item{name}{
  The basename of the test.
}
}
\details{
The \code{writeScanex} function reads an index file from \code{paste0(
name, "Index.csv")} and writes out records with Student ID of
999999999, the standard marker for an answer key, to a filename
constructed as \code{paste0(name, "Correct.dat")}.
}
\value{
For \code{readScanex},
a dataframe with the following columns, all read as character vectors:
\item{Student ID}{The student number.}
\item{Section}{The class section number.}
\item{ExamCode}{The exam code indicating the version of the test.}
\item{Marker}{A marker which must mean something...}
\item{Scantron}{The sequence number of the Scantron sheet.  This number is stamped onto
the Scantron sheet as it is read, so it can be used to retrieve the original in case
it needs to be checked for errors.}
\item{Answers}{The coded answers read from the Scantron.}

The \code{writeScanex} function is called for the side effect of writing 
the correct records to a file.
}
\author{
Duncan Murdoch
}

\seealso{
\code{\link{grades}}, \code{\link{wrongKey}}
}
\keyword{ utils }
