Ritems <-
function(..., Correct=1, KeepLast=0) {
  x <- unlist(list(...))
  items(paste("\\begin{Schunk}\n\\begin{Soutput}\n", x, 
              "\n\\end{Soutput}\n\\end{Schunk}\n", sep=""),
        Correct=Correct, KeepLast=KeepLast)
}

Renumerate <- function(..., Correct = 1, KeepLast = 0) {
  cat("\\begin{enumerate}\n")
  Ritems(..., Correct=Correct, KeepLast=KeepLast)
  cat("\\end{enumerate}\n")
}