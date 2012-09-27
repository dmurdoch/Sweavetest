\name{StatisticalOverview}
\alias{StatisticalOverview}
\title{
Create a Single Page Statistical Overview of the Entire Test
}
\description{
This function creates a single page statistical overview of the test, with formatting for use in Latex. Using statistics calculated by other functions in the Sweavetest package, this function creates the tables and appropriate formatting to include a single page overview of the test to the user when a report is desired.
}
\usage{
StatisticalOverview()
}

\value{
Several tables and graphs are produced by this function:
  \item{Student Count Table}{
     A table of the number of students who wrote the test, broken down by class section
  }
  \item{Descriptive Statistics Table}{
    Includes all common statistics broken down by Section (if more than one), and is also aggregated. Includes mean, standard deviation, maximum, minimum, 25th, 50th, and 75th percentiles
  }
  \item{KR20 and Ferguson's Delta}{
    A table containing the calculated Kuder-Richardson 20 and Ferguson's Delta statistic, as well as a warning if either of these statistics are below accepted industry standards
  }
  \item{Percentage Table}{
    A table containing the mean score achieved on each version of the test (if multiple versions are created)
  }
  \item{Score Distribution Plot}{
    A histogram of the score (as a percentage) of all students who wrote the test. Breaks have been placed at 0,10,20. . . representing the percentage score on the test.
  }
  \item{Answer Correlation Plot}{
    Creates a plot measuring the correlation between students answering the question correctly, and their overall score on the test to help the instructor identify poor or potentially incorrect questions
  }

}
\author{
Adam Rahman
}

\seealso{
grades(), KR20(), FergusonsDelta()
}
\examples{
scanex <- readScanex("sample.dat")

GradedTests(grades(scanex))

StatisticalOverview()
}