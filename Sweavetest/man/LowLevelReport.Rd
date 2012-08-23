\name{LowLevelReport}
\alias{LowLevelReport}
\title{
Create a Low Level Report using Exam Data
}
\description{
This function is to be used by instructors who have not created a test in Sweave, but wish to create a report using their exam results. It produces many of the same statistics as the full report process, without the ability to reproduce the test questions. One modification is made from the full report, as we are unable to report the frequencies of each question option. Instead, we simply report the frequencies of the letter options by exam code, and leave it up to the instructor to cross reference them with the original test.
}
\usage{
LowLevelReport(scanex)
}

\arguments{
  \item{scanex}{
    The result of the function readScanex(), which can also be found in the Sweavetest package
}
}

\value{
This function returns several statistics calculated from the test results:
\item{Descriptive Statistics}{}
\item{Student Counts}{}
\item{Test Means}{}
\item{Kuder-Richardson 20 and Ferguson's Delta Reliability Statistics}{}
\item{Answer Correlation Plots}{}
\item{Grade Distribution Histogram}{}

Individual question analysis is provided through the following statistics:
\item{Option Frequency}{}
\item{Item Difficulty}{}
\item{Item Discrimination}{}
\item{Point Biserial Correlation}{}
\item{Question Frequency Plots}{}
\item{Empirical Probability Plots}{}
}

\author{
Adam Rahman
}

\seealso{
readScanex()
}
\examples{

library(tables)
setwd("C:/Users/Adam Rahman/Desktop/Summer Project")
scanex <- readScanex("sample.dat")

LowLevelReport(scanex)
}