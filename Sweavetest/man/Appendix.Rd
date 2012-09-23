\name{Appendix}
\alias{Appendix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create an Appendix During the Report Process
}
\description{
If a report is ordered by the instructor, creates an appendix explaining the statistical concepts used throughout the report
}
\usage{
Appendix(Version)
}
\arguments{
  \item{Version}{
    What sort of report is wanted?  May be \code{"Report"}, \code{"Low Level"}, \code{"Student"}.
}
}
\value{
This function produces an explanation for each of the statistical concepts used in the report, so that the instructor may look up the significance of one of the statistics if they are unsure of its meaning. All output is completed with Latex formatting for the report process.
}
\author{
Adam Rahman
}

\examples{
Version <- "Report"
Appendix(Version)
}

