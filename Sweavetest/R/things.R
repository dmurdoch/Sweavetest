things <-
function(..., Correct=NA, KeepLast=0, report=FALSE) {
  if (report)
    stop("things() cannot produce a report; you need to call QReport()")
  x <- unlist(list(...))
  n <- length(x) - KeepLast
  rand <- sample(n)  # Leave this here in case randomization is only temporarily off
  if (!randomize()) rand <- 1:n  
  indices <- c(if (n) perms[[n]][[testversion()]][rand], n+seq_len(KeepLast))
  if (!is.na(Correct)) {
#    x[Correct] <- paste("\\Correct", x[Correct])
    Answers(c(Answers(), which(indices == Correct)))
  }
  
  x <- x[indices]
  
  QuestionCounter(QuestionCounter() + 1)
  QuestionIndex(c(QuestionIndex(),QuestionCounter()))
  
  cat(x, collapse="\n")
  
  if(length(indices)<5){
    indices <- c(indices,rep(NA,5-length(indices)))
  }
  
  Index(rbind(Index(),data.frame(Question=QuestionCounter(), 
  			       ExamCode=versioncode(),
  			       Correct=Correct,
  			       A=indices[1],
  			       B=indices[2],
  			       C=indices[3],
  			       D=indices[4],
  			       E=indices[5])))	
}

