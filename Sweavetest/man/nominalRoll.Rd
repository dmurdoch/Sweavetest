\name{nominalRoll}
\alias{nominalRoll}
\title{
Generate nominal roll sheets for exam
}
\description{
Prints sheets like standard UWO nominal rolls, but with details filled in
}
\usage{
nominalRoll(course, date, rooms, institution = "THE UNIVERSITY OF WESTERN ONTARIO", file = "nominal.tex")
}
\arguments{
  \item{course}{
  Course number
}
  \item{date}{
  Date of test
}
  \item{rooms}{
  Vector listing number of rows in each room, named by the room.
}
  \item{institution}{
  Name of institution.
}
  \item{file}{
  File to write result to.
}
}
\value{
Called for the side effect of writing the .tex file.
}
\author{
Duncan Murdoch
}
\examples{
# Produce one page for each row in WSC 248
nominalRoll("SS 1024A", "October 17, 2011", c("WSC 248"=4))
}
