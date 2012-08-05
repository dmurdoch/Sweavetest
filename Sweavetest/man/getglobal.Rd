\name{getglobal}
\alias{getglobal}
\title{
Get a global value if an argument is missing.
}
\description{
This takes a missing argument to a function, and gets the global value
of the same name, or a default value.
}
\usage{
getglobal(var, default)
}
\arguments{
  \item{var}{ Which variable to use.
}
  \item{default}{
A default value.
}
}
\value{
The value of \code{var}, or if it is missing, the value of a global of the same name,
or if none, the \code{default} value.
}
\examples{
f <- function(x) {
   x <- getglobal(x, 10)
   x
}
f(1)
f()
x <- 123
f()
}

\keyword{ utils }% __ONLY ONE__ keyword per line
