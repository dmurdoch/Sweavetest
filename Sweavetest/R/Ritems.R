Ritems <-
function(..., Correct=1, KeepLast=0, report=FALSE) {

  if (report) 
    stop("Ritems() cannot produce a report; you need to call QReport()")

  x <- unlist(list(...))
  items(paste("\\begin{Schunk}\n\\begin{Soutput}\n", x, 
              "\n\\end{Soutput}\n\\end{Schunk}\n", sep=""),
        Correct=Correct, KeepLast=KeepLast)
}

Renumerate <- function(..., Correct = 1, KeepLast = 0, report=TRUE) {
  cat("\\begin{enumerate}\n")
  Ritems(..., Correct=Correct, KeepLast=KeepLast)
  cat("\\end{enumerate}\n")
  
  if (report)
    QReport()
}