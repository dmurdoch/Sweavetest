\name{PointBiserial}
\alias{PointBiserial}
\title{
Calculate the Point Biserial Correlation for Each Question
}
\description{
This function calculates the point biserial values for each question in the multiple choice test.
}
\usage{
PointBiserial(GradedTests)
}
\arguments{
  \item{GradedTests}{
    The result of running the function grades(), which can be found in the Sweavetest package
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
setwd("C:/Users/SS1024")
scanex <- readScanex("sample.dat")

GradedTests <- grades(scanex, correct=weights) 

PointBiserial(GradedTests)
}

