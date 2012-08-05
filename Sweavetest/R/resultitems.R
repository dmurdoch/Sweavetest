resultitems <-
function(..., Correct=1, KeepLast=0, strip=TRUE) {
  x <- list(...)
  n <- length(x)
  vals <- character(n)
  for (i in 1:n) 
    vals[i] <- clean(capture.output(print(x[[i]])), stripleading=strip)
  Ritems(vals, Correct=Correct, KeepLast=KeepLast)
}

resultenumerate <- 
function(..., Correct=1, KeepLast=0, strip=TRUE) {
  cat("\\begin{enumerate}\n")
  resultitems(..., Correct=Correct, KeepLast=KeepLast, strip=strip)
  cat("\\end{enumerate}\n")
}