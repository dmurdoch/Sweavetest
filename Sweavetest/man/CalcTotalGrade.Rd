\name{CalcTotalGrade}
\alias{CalcTotalGrade}
\title{
Calculate the total grades on the test
}
\description{
Calculate the total grades of the students, including any and all long answer questions that were included.
}
\usage{
CalcTotalGrade(MCGrades = GradedTests(), LAGrades = LSGrades())
}

\arguments{
  \item{MCGrades}{
Default input is the .dat file corresponding to the multiple choice grades that is read in by the Initialization() function
}
  \item{LAGrades}{
Default input is the .csv file corresponding to the long answer grades that is read in by the Initialization() function
}
}

\value{
Returns the original input from GradedTests(), with two columns appended to the end, pertaining to each student's grade on the individual long answer questions, and their total grade on the test
}

\author{
Adam Rahman
}

\examples{
#This is a default function that is used internally by Sweavetest. There is no need to modify the inputs of this function

CalcTotalGrade(MCGrades = GradedTests(), LAGrades = LSGrades())
}

