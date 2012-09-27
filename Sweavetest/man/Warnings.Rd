\name{Warnings}
\alias{Warnings}
\title{
Produce Warnings if Statistics Fall Below Acceptable Standards.
}
\description{
This function will produce warnings if the calculated values of Difficulty Rating, Item Discrimination, or Point Biserial Index are unacceptable
}
\usage{
Warnings(DR, ID, PB)
}
\arguments{
  \item{DR}{
    The result of running the function DifficultyRating()
}
  \item{ID}{
    The result of running the function ItemDiscriminator()
}
  \item{PB}{
    The result of running the function PointBiserial()
}
}
\value{
This function will return a short sentence to the user if the calculated values of Difficulty Rating, Item Discriminator, or Point Biserial Correlation are unacceptable (by industry standards). 

For example, a simple warning such as: ''This question has been identified as being extremely difficult, and should be reviewed for possible errors or exclusion.'', will be issued if the difficulty rating of the question calculated to be less than .2.
}
\author{
Adam Rahman
}
\seealso{
DifficultyRating(), ItemDiscriminator(), PointBiserial()
}
\examples{
scanex <- readScanex("sample.dat")

GradedTests <- grades(scanex)
DR <- DifficultyRating(GradedTests, 1)
ID <- ItemDiscriminator(GradedTests, 1)
PB <- PointBiserial(GradedTests, 1)

Warnings(DR,ID,PB)

}