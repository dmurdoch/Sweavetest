\name{KR20}
\alias{KR20}
\title{
Compute the Kuder-Richardson 20 Reliability Statistic for the Multiple Choice Test
}
\description{

}
\usage{
KR20(GradedTests)
}
\arguments{
  \item{GradedTests}{
    The result of running the function grades(), which is also available in the Sweavetest package.
}
}
\value{
Returns the calculated value of the Kuder-Richardson 20 Statistic.
}
\author{
Duncan Murdoch
Adam Rahman
}

\seealso{
grades()
}
\examples{
setwd("C:/Users/Adam Rahman/Desktop/Summer Project")
scanex <- readScanex("sample.dat")

GradedTests <- grades(scanex, correct=weights) 

KR20(GradedTests)
}