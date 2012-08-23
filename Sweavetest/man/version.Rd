\name{version}
\alias{version}
\title{
Set the Version Type for the Test Creation Process
}
\description{
Allows the user to choose between one of three test versions: Student, Teacher, or Report.
}
\usage{
version(x = "Student")
}

\arguments{
  \item{x}{
    Input from the user. Specifically, one of Student, Teacher, or Report
}
}
\details{
version() uses the function match.arg() to find the closes match from the available choices (Student, Teacher, or Report) to the input provided from the instructor
}
\value{
Sets a global ``Version'' variable to either Student, Teacher, or Report for use in the test creation process.
}
\author{
Adam Rahman
}

\examples{
x <- ``Student''
version(x)
}

