\name{PermuteResponses}
\alias{PermuteResponses}
\alias{getPerm}
\title{
Generate or return permutations of responses
}
\description{
Functions \code{\link{horiz}}, \code{\link{enumerate}}, etc. each trigger
a call to \code{PermuteResponses} to permute responses and record the permutations.  \code{getPerm}
returns the permutation for a particular question.
}
\usage{
PermuteResponses(len, Correct = 1, KeepLast = 0)
getPerm(na.rm = TRUE, code = versioncode(), q = QuestionCounter(), 
        rand = randomize(), index = Index()) 
  
}
\arguments{
  \item{len}{
How many responses to permute?
}
  \item{Correct}{
Which is the correct response?
}
  \item{KeepLast}{
How many responses should be kept at the end (e.g. \dQuote{None of the above}).
}
  \item{na.rm}{
Whether to remove \code{NA} values for questions with fewer than 5 responses.
}
  \item{code}{
Which exam code to return.
}
  \item{q}{
Which question to return.
}
  \item{rand}{
Whether to show the randomized result.
}
  \item{index}{
Which dataframe to use for the permutation records.
}
}
\value{
\code{PermuteResponses} is called for the side effects of adding to the \code{\link{Index}} dataframe.

\code{getPerm} returns a vector of the requested permutation.
}
\author{
Duncan Murdoch
}
