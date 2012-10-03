\name{EmpiricalProbabilityPlot}
\alias{EmpiricalProbabilityPlot}

\title{
Create an Empirical Probability Plot of Answering Each Question Correctly.
}
\description{
This function creates a plot of the empirical probability of answering a question correctly. One plot is created for each question, with test score along the x-axis, and the probability of answering the question correctly along the y-axis.
}
\usage{
EmpiricalProbabilityPlot(GradedTests, QuestionCount)
}
\arguments{
  \item{GradedTests}{
	The result of running the function grades(), which is also available in the Sweavetest package.
}
  \item{QuestionCount}{ ?? }
}

\value{
One plot per question is returned, with the test scores along the x-axis, and the probability of answering the question correctly along the y-axis
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

EmpiricalProbabilityPlot(GradedTests(), 2)

}