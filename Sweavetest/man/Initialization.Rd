\name{Initialization}
\alias{Initialization}
\title{
Create all formatting and variable initialization for the test and report creation process
}
\description{
This function declares all variables and pastes all formatting options for the test/report creation process to the main document.
}
\usage{
Initialization(name, version = Version(), scanex)
}
\arguments{
  \item{name}{The value of \code{\link{TestName}} to use for this test.}
  \item{version}{The value of \code{\link{Version}} to use for this test.}
  \item{scanex}{If \code{version == "Report"}, a filename from which
  to read Scantron results, or the results themselves.}
}

\value{
This function is run for the side effect of initializing the run through the test.
}
\author{
Adam Rahman and Duncan Murdoch
}
