\name{answerCorrelations}
\alias{answerCorrelations}
\title{
Plot correlations of correct answers versus student scores
}
\description{
Compute the total score for each student, and correlate it with their answers on each question, 
to detect errors in the answer key, or bad wording in questions.
}
\usage{
answerCorrelations(gradedTests = GradedTests(), qs)
}
\arguments{
  \item{gradedTests}{
Student results.
}
  \item{qs}{
Which questions to include?  Defaults to all questions.
}
}
\value{
Produces a plot of correlations versus student score, using question number as plotting symbol.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{answerPlots}}
}

