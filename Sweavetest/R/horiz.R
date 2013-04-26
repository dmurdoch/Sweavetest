horiz <-
function(..., Correct=1, KeepLast=0, report=TRUE) {
  
  QuestionCounter(QuestionCounter()+1)
  
  
  x <- unlist(list(...))
  if (CheckDups() && any(duplicated(format(x))))  
    stop("Duplicated answers in Q", length(Answers())+1, ": ", 
       paste(format(x), collapse=" ")) 
  n <- length(x)-KeepLast  
  rand <- sample(n)  # Leave this here in case randomization is only temporarily off
  if (randomize()) 
    indices <- c(if (n) perms[[n]][[testversion()]][rand], n+seq_len(KeepLast))
  else
    indices <- seq_along(x)
  if (!is.na(Correct)) {
    correct(c(correct(), Correct))
    x[Correct] <- paste("\\Correct", x[Correct])
    Answers(c(Answers(), which(indices == Correct)))
  }		 				  
	 
  labels <- itemlabels()[1:(n+KeepLast)]		 	 
  x <- x[indices]		 				  
				  
  QuestionIndex(c(QuestionIndex(),QuestionCounter()))	 
				  
  cat( paste(labels, "\\hspace{1ex}", x, "\\hfill") )	 
 
  if(length(indices)<5){		 			 
    indices <- c(indices,rep(NA,5-length(indices)))						 
  }		 				  

  LastIndices(indices)
  
  if (Version() != "Report" && !is.na(Correct))
    Index(rbind(Index(),data.frame(Question=QuestionCounter(), 
  			       ExamCode=versioncode(),
  			       Correct=Correct,
  			       A=indices[1],
  			       B=indices[2],
  			       C=indices[3],
  			       D=indices[4],
  			       E=indices[5])))	
  
  if (report)
    QReport()
}
