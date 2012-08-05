\name{mergeLists}
\alias{mergeLists}
\title{
Merge grades or other columns from one dataframe into another.
}
\description{
This function does the kind of merge needed when grading tests.
}
\usage{
mergeLists(master, update, key = "Student ID", full = TRUE)
}
\arguments{
  \item{master}{
  The master list of grades.  All columns will be kept.
}
  \item{update}{
  A list of grades to merge into the \code{master} list.
}
  \item{key}{
  The name of the column to use as the merge key.
}
  \item{full}{
  Whether to do a full merge keeping all rows from both files, or just
  to keep all rows from the \code{master} file.
}
}
\details{
  If \code{key} is not length 1, then the first entry is used for the key from the 
\code{master} file, and the second entry is used as the key from the \code{update} file.
}
\value{
The result of \code{\link{merge}} applied to the two files, with arguments set
as described above.
}
\author{
Duncan Murdoch
}
\keyword{ utils }
