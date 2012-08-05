\name{clean}
\alias{clean}
\title{
Collapse into multi-line string, and strip leading blanks.
}
\description{
Collapse into multi-line string, and strip leading blanks.
}
\usage{
clean(lines, stripleading = TRUE)
}
\arguments{
  \item{lines}{
Input strings.
}
  \item{stripleading}{
Whether to do anything.
}
}
\value{
The same lines collapsed into a multi-line string, with leading blanks removed if requested.
}

\examples{
cat(clean(letters[1:4]))
}
\keyword{ utils }
