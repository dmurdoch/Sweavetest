horiz <-
function(..., Correct=1, KeepLast=0, CheckDups, randomize, itemlabels, Answers, Index, QuestionIndex,DR,ID,PB,AnswerCountMatrix, GradedTests, fignum, correct) {
  
  randomize <- getglobal(randomize, FALSE)
  itemlabels <- getglobal(itemlabels, paste("(", letters, ")", sep=""))
  Answers <- getglobal(Answers, c())
  CheckDups <- getglobal(CheckDups, TRUE)
  Index <- getglobal(Index, c())
  QuestionIndex <- getglobal(QuestionIndex,c())
  correct <- getglobal(correct,c())
  QuestionCounter(QuestionCounter()+1)
  
  correct <- c(correct, Correct)
  .STEnv$correct <- correct
  
  x <- unlist(list(...))
  if (CheckDups && any(duplicated(format(x))))  
    stop("Duplicated answers in Q", length(Answers)+1, ": ", 
       paste(format(x), collapse=" ")) 
  n <- length(x)-KeepLast  
  rand <- sample(n)  
  if (!randomize) rand <- 1:n  
  indices <- c(if (n) perms[[n]][[testversion()]][rand], n+seq_len(KeepLast))
  if (!is.na(Correct)) {
    x[Correct] <- paste("\\Correct", x[Correct])
    .STEnv$Answers <- c(Answers, which(indices == Correct))
  }		 				  
	 
  .STEnv$LastIndices <- indices		 		 
		 
  labels <- itemlabels[1:(n+KeepLast)]		 	 
  x <- x[indices]		 				  
				  
  .STEnv$QuestionIndex <- c(QuestionIndex,QuestionCounter())	 
				  
  cat( paste(labels, "\\hspace{1ex}", x, "\\hfill") )	 
 
  if(length(indices)<5){		 			 
    indices <- c(indices,rep(NA,5-length(indices)))						 
  }		 				  
		 
  .STEnv$Index <- rbind(Index,indices)	
  
  QReport()
}
