\name{CreateIndex}
\alias{CreateIndex}
\title{
Create a Table of Counts
}
\description{
This function takes the answers provided by the completed test file, and creates a table of answer counts for each unique exam code that correspond back to the original question. Due to the randomization that occurs with each exam code, it is very difficult to compare which answers are popular amongst students without having each exam available for comparison. This function creates a table of counts that directly corresponds to the original ordering of the question (before randomization) so that the user can instantly see which answer options were the most popular.
}
\usage{
CreateIndex(gradedTests=GradedTests())
}
\arguments{
  \item{gradedTests}{
Dataframe of graded test results.
}
}
\value{
A dataframe is returned with the columns pertaining to the answer choices for each question (i.e. Option 1, Option 2), and rows pertaining to the question number. Each entry in the matrix is the number of responses to each question option.
}
\author{
Adam Rahman
}
\note{
This function works only if a .dat file has been created during the test creation process from the .Rnw file. The default name for this file will be IndexTable.dat. Everything else is handled internally by this function.
}

\examples{
CreateIndex()
}