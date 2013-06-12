horiz <-
function(..., Correct=1, KeepLast=0, report=TRUE) {
  
  QuestionCounter(QuestionCounter()+1)
  
  x <- unlist(list(...))
  if (CheckDups() && any(duplicated(format(x))))  
    stop("Duplicated answers in Q", QuestionCounter(), ": ", 
       paste(format(x), collapse=" ")) 

  PermuteResponses(length(x), Correct, KeepLast)
  
  if (!is.na(Correct)) 
    x[Correct] <- paste("\\Correct", x[Correct])
	 
  labels <- itemlabels()[seq_along(x)]
  x <- x[getPerm()]		 				  
				  
  cat( paste(labels, "\\hspace{1ex}", x, "\\hfill") )	 
 
  if (report)
    QReport()
}
