items <-
function(..., Correct=1, KeepLast=0, report=FALSE) {
  
  if (report) 
    stop("items() cannot produce a report; you need to call QReport()")
    
  QuestionCounter(QuestionCounter() + 1)
  
  x <- unlist(list(...))
  if (CheckDups() && any(duplicated(format(x))))  
    stop("Duplicated answers in Q", QuestionCounter(), ": ", 
         paste(format(x), collapse=" "))
  
  PermuteResponses(length(x), Correct, KeepLast)
  
  if (!is.na(Correct)) 
    x[Correct] <- paste("\\Correct", x[Correct])
  
  x <- x[getPerm()]		 				  
  
  y <- paste("\\item",x,"\n", sep=" ")
  cat(y)
}
