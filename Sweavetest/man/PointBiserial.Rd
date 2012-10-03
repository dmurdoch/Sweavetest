\name{PointBiserial}
\alias{PointBiserial}
\title{
Calculate the Point Biserial Correlation for Each Question
}
\description{
This function calculates the point biserial values for each question in the multiple choice test.
}
\usage{
PointBiserial(GradedTests, Questions=qs)
}
\arguments{
  \item{GradedTests}{
    The result of running the function grades(), which can be found in the Sweavetest package
}
  \item{Questions}{
    Which questions to process.
}
}
\value{
The function PointBiserial returns a data frame containing the point biserial values for each question.
}
\author{
Adam Rahman
}
\seealso{
grades()
}
\examples{

scanex <- readScanex(system.file("sample/sample.dat", package="Sweavetest"))

GradedTests <- grades(scanex, correct=weights) 

PointBiserial(GradedTests)
}

