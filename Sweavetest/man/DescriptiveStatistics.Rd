\name{DescriptiveStatistics}
\alias{DescriptiveStatistics}
\title{
Summary table of grades
}
\description{
Format a LaTeX table summarizing the grades in each section of the course.
}
\usage{
DescriptiveStatistics(Percentage, Section)
}
\arguments{
  \item{Percentage}{
  The value to use as the grade for each student.
}
  \item{Section}{
  The section number for each student.
}
}
\value{
Called for the side effect of printing a table.
}
\author{
Adam Rahman and Duncan Murdoch
}
\seealso{
\code{\link{StatisticalOverview}}
}
\examples{
scanex <- readScanex(system.file("sample/sample.dat", package="Sweavetest"))

GradedTests(grades(scanex)) 

DescriptiveStatistics(GradedTests()$Grade, GradedTests()$Section)
}
