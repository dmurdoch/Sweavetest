\name{globals}
\alias{globals}
\alias{CheckDups}
\alias{GradedTests}
\alias{Index}
\alias{NumVersions}
\alias{QuestionCounter}
\alias{QuestionIndex}
\alias{TestName}
\alias{Version}
\alias{VersionCodes}
\alias{fignum}
\alias{itemlabels}
\alias{randomize}
\alias{testversion}
\alias{versioncode}
\title{
Getter/setter functions for various global variables.
}
\description{
These functions get the current value or set it for a number of 
global variables used during the construction of the test or the report.
}
\usage{
CheckDups(x = TRUE)
GradedTests(x = NULL)
Index(x = NULL)
NumVersions(x = 4)
QuestionCounter(x = 0)
QuestionIndex(x = NULL)
TestName(x = "TestData")
Version(x = "Student")
VersionCodes(x = seq_len(NumVersions()))
fignum(x = 0)
itemlabels(x = paste0("(", LETTERS[1:5], ")"))
randomize(x = TRUE)
testversion(x = 1)
versioncode()
}
\arguments{
  \item{x}{
An optional new value to set the global.
}
}
\details{
The globals have the following meaning and structure:
\describe{
\item{CheckDups}{Should the test building functions \code{\link{horiz}}, \code{\link{enumerate}}, etc.
check for duplicate answers?}
\item{GradedTests}{A dataframe of test results produced by \code{\link{grades}}.}
\item{Index}{A dataframe of test result encodings, produced by reading the 
index output when producing the student versions of the tests.}
\item{QuestionCounter}{The current question number.}
\item{QuestionIndex}{The complete list of question numbers.}
\item{TestName}{A name to use for the test and as part of created filenames.  (The \code{\link{make.names}} function
is used to sanitize the name.)}
\item{Version}{The version or mode of this run through the test: one of \code{c("Student", "Teacher", "Report")}}
\item{VersionCodes}{Vector of 3 exam codes for all exam versions.}
\item{fignum}{The current figure number when producing a report.}
\item{itemlabels}{The labels to use in \code{\link{horiz}} and \code{\link{answerPlots}}.}
\item{randomize}{Whether to randomize the answer order.}
\item{testversion}{The test version number from 1 to \code{NumVersions()}, or 0 for the master.}
\item{versioncode}{A read-only display value of the exam code for this test version.}
}
}
\value{
These functions return the value of the global (after setting it).
}
\author{
Duncan Murdoch
}
