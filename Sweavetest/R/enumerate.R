enumerate <- function(..., Correct = 1, KeepLast = 0, report = TRUE) {
  cat("\\begin{enumerate}\n")
  items(..., Correct=Correct, KeepLast=KeepLast)
  cat("\\end{enumerate}\n")

  if (report)
    QReport()
}