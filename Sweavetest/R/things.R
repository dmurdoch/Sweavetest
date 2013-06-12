things <-
function(..., Correct=NA, KeepLast=0, report=FALSE) {
  if (report)
    stop("things() cannot produce a report; you need to call QReport()")
  x <- unlist(list(...))
  
  QuestionCounter(QuestionCounter() + 1)
  
  PermuteResponses(length(x), Correct, KeepLast)
  
  x <- x[getPerm()]
  
  cat(x, collapse="\n")
}

