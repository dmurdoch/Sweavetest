\name{Unscramble}
\alias{Unscramble}
\title{
Undo Question Randomization for Use in Low Level Report
}
\description{
Using correspondences provided by the instructor, this function will order all
questions from 1 to the last question, and will export a new updated scanex variable
for use in further analysis.
}
\usage{
Unscramble(scanex, ExamCodes, Orders, Scrambled = FALSE)
}

\arguments{
  \item{scanex}{
The original Scantron file in an unmodified format
}
  \item{ExamCodes}{
A vector containing the Exam Codes of the test.
}
  \item{Orders}{
A matrix containing the order of the questions. These must be in correspondence with the Exam Codes answered above.
}
  \item{Scrambled}{
Scrambled is a indicator variable that will tell the function to execute the unscrambling process based on the specification of the user, or to simply return the unmodified scanex.
}
}
\value{
This function returns the Scantron file back to the user, but with answers in the order specified by the instructor.
}
\author{
Adam Rahman
}

\examples{

scanex <- readScanex(''sample.dat'')
ExamCodes <- c(170,640,880, 990)
Orders <- matrix(c(1,2,3,4,2,3,4,1,3,4,1,2,4,1,2,3), nrow=4,ncol=4, byrow=TRUE)

Unscramble(scanex, ExamCodes, Orders, Scrambled = TRUE)

}
