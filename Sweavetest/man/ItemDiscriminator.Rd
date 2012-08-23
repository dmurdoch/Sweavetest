\name{ItemDiscriminator}
\alias{ItemDiscriminator}
\title{
Computes Item Discrimination values for each question in the test
}
\description{
This function computes item discrimination values for the questions on the test.
}
\usage{
ItemDiscriminator(GradedTests)
}
\arguments{
  \item{GradedTests}{
    The result of running the function grades(), which can be found in the Sweavetest package.
}
}
\value{
This function returns a data frame consisting of the calculated item discrimination values for each question.
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

ItemDiscriminator(GradedTests)
}
