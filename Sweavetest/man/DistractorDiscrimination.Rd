\name{DistractorDiscrimination}
\alias{DistractorDiscrimination}
\title{
Analyse the Question Distractors using Item Discrimination Analysis
}
\description{
Using the created test index, this function executes item discrimination analysis on each of the individual answer choices, to help determine which distractors are effective, and which distractors are not.
}
\usage{
DistractorDiscrimination(
   GradedTests = getglobal(GradedTests, NULL),
   QuestionCounter = getglobal(QuestionCounter, 0),
   Index = getglobal(Index, NULL))
}
\arguments{
  \item{GradedTests}{
    The result of running the function \code{\link{grades}}.
}
  \item{QuestionCounter}{
    A global variable used throughout the test creation process to keep track of the current question
}
  \item{Index}{ ?? }
}

\value{
A vector of values between -1 and 1 is returned. Each value corresponds to an answer choice provided by the instructor on the test.
}
\author{
Adam Rahman
}

\seealso{
grades(), ItemDiscrimination()
}
\examples{


scanex <- readScanex("sample.dat")
GradedTests <- grades(scanex, correct=weights)

#Analyse the first question
QuestionCounter <- 1

DistractorDiscrimination(GradedTests,QuestionCounter)


}