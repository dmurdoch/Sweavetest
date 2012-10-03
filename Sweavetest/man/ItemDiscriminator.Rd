\name{ItemDiscriminator}
\alias{ItemDiscriminator}
\title{
Computes Item Discrimination values for each question in the test
}
\description{
This function computes item discrimination values for the questions on the test.
}
\usage{
ItemDiscriminator(GradedTests, Questions = qs)
}
\arguments{
  \item{GradedTests}{
    The result of running the function grades(), which can be found in the Sweavetest package.
}
  \item{Questions}{
    Which questions to process?  The default is all questions.
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

scanex <- readScanex(system.file("sample/sample.dat", package="Sweavetest"))

GradedTests(grades(scanex)) 

ItemDiscriminator(GradedTests())
}
