\name{answerMatrix}
\alias{answerMatrix}
\title{
Convert answer strings to matrix.
}
\description{
Converts answer strings into a matrix, with one answer per cell.
}
\usage{
answerMatrix(answers, qs = seq_len(max(nchar(answers))))
}
\arguments{
  \item{answers}{
Character vector of answers, one string per student.
}
  \item{qs}{
Which questions to include.
}
}
\value{
Matrix with students as rows, questions as columns.
}
\author{
Duncan Murdoch
}
\seealso{
Used by \code{\link{answerCorrelations}} and \code{\link{answerPlots}}.
}
\examples{
student <- c("ABCDE", "ABCDE", "ABDC", "BADCA")
answerMatrix(student)  
}
