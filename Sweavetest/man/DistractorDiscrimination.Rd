\name{DistractorDiscrimination}
\alias{DistractorDiscrimination}
\title{
Analyse the Question Distractors using Item Discrimination Analysis
}
\description{
Using the created test index, this function executes item discrimination analysis on each of the individual answer choices, to help determine which distractors are effective, and which distractors are not.
}
\usage{
DistractorDiscrimination(GradedTests, Question = QuestionCounter())
}
\arguments{
  \item{GradedTests}{
    The result of running the function \code{\link{grades}}.
}
  \item{Question}{
    Which question to process?
}
}
\value{
A vector of values between -1 and 1 is returned. Each value corresponds to an answer choice provided by the instructor on the test.
}
\author{
Adam Rahman
}

\seealso{
\code{\link{grades}}, \code{\link{ItemDiscriminator}}
}
\examples{

scanex <- readScanex(system.file("sample/sample.dat", package="Sweavetest"))

GradedTests(grades(scanex))
Index(read.csv(system.file("sample/TestIndex.csv", package="Sweavetest")))

#Analyse the first question

DistractorDiscrimination(GradedTests(),1)


}