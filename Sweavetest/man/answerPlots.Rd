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
answerPlots(Questions = qs, col.wrong="pink", col.correct="white")
}
\arguments{
  \item{Questions}{
Which questions to plot.
}
  \item{col.wrong, col.correct}{
Colours to use for wrong and right answers respectively.
}
}
\value{
Produces one plot per question.
}
\note{
The code assumes that the correct answers and the version label are in 1-1 correspondence.
}
\author{
Duncan Murdoch
}
