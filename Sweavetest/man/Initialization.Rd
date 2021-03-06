\name{Initialization}
\alias{Initialization}
\title{
Create all formatting and variable initialization for the test and report creation process
}
\description{
This function declares all variables and pastes all formatting options for the test/report creation process to the main document.
}
\usage{
Initialization <- function(name, version = Version(), scanex, LongAnswers = NA, LQMaxScore = 0, numversions = 4)
}
\arguments{
  \item{name}{The value of \code{\link{TestName}} to use for this test.}
  \item{version}{The value of \code{\link{Version}} to use for this test.}
  \item{scanex}{If \code{version == "Report"}, a filename from which
  to read Scantron results, or the results themselves.}
  \item{numversions}{How many different versions of the test will we produce?}
  \item{LongAnswers}{If \code{version == "Report"}, a filename from which
  to read long answer grades}
  \item{LQMaxScore}{The total number of marks available from long answer questions}
}

\value{
This function is run for the side effect of initializing the run through the test.
}
\author{
Adam Rahman and Duncan Murdoch
}
