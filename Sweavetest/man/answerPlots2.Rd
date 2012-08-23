\name{answerPlots}
\alias{answerPlots}
\title{
Plot bar charts of answers 
}
\description{
This is designed to plot bar charts of answers in 4-version tests (but will work with 
other small numbers of versions).
}
\usage{
answerPlots(student, correct, version, qs = seq_len(max(nchar(correct))), decreasing = FALSE,
            col.wrong="pink", col.correct="white")
}
\arguments{
  \item{student}{
Student answer strings.
}
  \item{correct}{
Correct answer strings.
}
  \item{version}{
Test version label.  
}
  \item{qs}{
Which questions to plot.
}
  \item{decreasing}{
Order to display questions according to score.  By default, in increasing order;
if \code{TRUE}, decreasing order; if \code{NA}, in original order.
}
  \item{col.wrong, col.correct}{
Colours to use for wrong and right answers respectively.
}
}
\value{
Produces one plot per question.
}
\note{
The code assumes that the correct answers and the version label are in 1-1 correspondence. This version of the answerPlots() function is only used in the creation of the low level report.
}
\author{
Duncan Murdoch
}
\examples{
student <- c("ABCDE", "ABCDE", "ABDC", "BADCA")
correct <- rep("ABCDE", 4)
answerPlots(student, correct, version=c(1,1,2,2))
}
