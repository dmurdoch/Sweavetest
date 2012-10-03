\name{FergusonsDelta}
\alias{FergusonsDelta}
\title{
Compute the Ferguson's Delta reliability statistic for the test.
}
\description{
The function FergusonsDelta() calculates the Ferguson's Delta reliability statistic, to help the instructor determine if the administered test adequately discriminates between the strong and weak students who wrote the test.
}
\usage{
FergusonsDelta(GradedTests)
}
\arguments{
  \item{GradedTests}{
    The result of running the function grades() which can be found in the Sweavetest package
}
}
\value{
The function returns the Ferguson's Delta statistic.
}
\author{
Adam Rahman
}

\seealso{
grades()
}
\examples{

scanex <- readScanex(system.file("sample/sample.dat", package="Sweavetest"))

GradedTests(grades(scanex))

FergusonsDelta(GradedTests())
}