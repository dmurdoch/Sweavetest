things <-
function(..., Correct=NA, KeepLast=0, randomize, Index) {
  
  randomize <- getglobal(randomize, FALSE)
  Index <- getglobal(Index,c())
    
  x <- unlist(list(...))
  n <- length(x) - KeepLast
  rand <- sample(n)
  if (!randomize) rand <- 1:n  
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
  
  .STEnv$Index <- rbind(Index, indices)
}

