items <-
function(..., Correct=1, KeepLast=0, report=FALSE) {
  
  if (report) 
    stop("items() cannot produce a report; you need to call QReport()")
    
  QuestionCounter(QuestionCounter() + 1)
  
  x <- unlist(list(...))
  if (CheckDups() && any(duplicated(format(x))))  
    stop("Duplicated answers in Q", length(Answers())+1, ": ", 
         paste(format(x), collapse=" "))
  
  n <- length(x) - KeepLast
  rand <- sample(n) # Leave this here in case randomization is only temporarily off
  
  if (!randomize()) rand <- 1:n  
  indices <- c(if (n) perms[[n]][[testversion()]][rand], n+seq_len(KeepLast))
  
  if (!is.na(Correct)) {
    correct(c(correct(), Correct))
    x[Correct] <- paste("\\Correct", x[Correct])
    Answers(c(Answers(), which(indices == Correct)))
  }
  
  x <- x[indices]
  QuestionIndex(c(QuestionIndex(),QuestionCounter()))
  
  y <- paste("\\item",x,"\n", sep=" ")
  cat(y)
  
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
}
