\name{DifficultyRating}
\alias{DifficultyRating}
\title{
Determine the Difficulty Rating of Each Test Question
}
\description{
The difficulty rating function determines how many students answered each question correctly, and then dividing by the total number of students to yield a value between 0 and 1, showing the user which questions were the most difficult.
}
\usage{
DifficultyRating(GradedTests, Questions=qs)
}
\arguments{
  \item{GradedTests}{
    The result of running the function grades(), which can be found in the Sweavetest package
}
  \item{Questions}{
    Which questions should be processed?  The default is all questions.
}
}
\value{
The function DifficultyRating returns a data frame containing the difficulty rating of the question.
}
\author{
Adam Rahman
}

\seealso{
The function grades() is used as input to this function. It can be found in the package Sweavetest.
}
\examples{

scanex <- readScanex(system.file("sample/sample.dat", package="Sweavetest"))

GradedTests <- grades(scanex) 

DifficultyRating(GradedTests)
}
