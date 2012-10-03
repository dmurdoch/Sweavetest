\name{AnswerCounts}
\alias{AnswerCounts}
\title{
Count occurrences of each answer.
}
\description{
Count the number of occurrences of each answer (or blank, or miscoded answers).
}
\usage{
AnswerCounts(StudentAnswers, qs = seq_len(max(nchar(Answers))))
}
\arguments{
  \item{StudentAnswers}{
A dataframe of answers, such as \code{\link{GradedTests}}.
}
  \item{qs}{
Which questions to process.
}
}
\value{
A 3-way array with dimensions being the exam codes, the questions, and the 
answer counts for \code{c(LETTERS[1:5], "Blank", "Bad")}.

}
\author{
Duncan Murdoch
}
