\name{Conclusion}
\alias{Conclusion}
\title{
Create all closing variables and formatting for the test and report creation process
}
\description{
This is a pseudo-function to declare all needed closing variables and formatting options for the test and report creation process
}
\usage{
Conclusion(END = TRUE)
}
\arguments{
  \item{END}{
    A pseudo variable used only to initialize the Conclusion() function. It is set as TRUE by default so the user need not  change it.
}
}
\value{
This function returns all variables and formatting for the test and report creation process so that the user need not worry about it. It also calls the Appendix() and answerkey() functions when they are required in the creation of the test report or teacher version of the test.
}
\author{
Adam Rahman
}
