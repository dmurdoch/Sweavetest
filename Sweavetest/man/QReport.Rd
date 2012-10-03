\name{QReport}
\alias{QReport}
\title{
Generate a report on one question.
}
\description{
This function generates a report on the current question, based on the graded tests.
}
\usage{
QReport()
}
\details{
The report contains the following components:
\itemize{
\item{A copy of the question, numbered in the originally specified order, with the correct
answer indicated by an asterisk.}
\item{A table of the counts of responses and the \code{\link{DistractorDiscrimination}} scores.}
\item{Item analysis results for this question, showing \code{\link{DifficultyRating}}, \code{\link{ItemDiscriminator}},
and \code{\link{PointBiserial}} scores for this question.}
\item{An \code{\link{answerPlots}} showing the responses for each option.}
\item{An \code{\link{EmpiricalProbabilityPlot}} showing the relation between 
results on this question and on the test as a whole.}

}
}
\value{
This function is called for the side effect of emitting LaTeX code to display the components described above.
}
\author{
Adam Rahman and Duncan Murdoch
}
