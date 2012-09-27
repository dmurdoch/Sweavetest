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
answerCorrelations(student, CorrectAnswers, qs)
}
\arguments{
  \item{student}{
Student answers in strings.
}
  \item{CorrectAnswers}{
Correct answers in strings, one per student.
}
  \item{qs}{
Which questions to include?
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
\examples{
student <- c("ABCDE", "ABCDE", "ABDC", "BADCA")
correct <- rep("ABCDE", 4)
answerCorrelations(student, correct)
}
