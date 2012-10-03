\name{KR20}
\alias{KR20}
\title{Kuder-Richardson 20 Reliabilibility Statistic
}
\description{
Computes the Kuder-Richardson 20 reliability statistic for the multiple choice test.

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

scanex <- readScanex(system.file("sample/sample.dat", package="Sweavetest"))

GradedTests(grades(scanex)) 

KR20(GradedTests())
}