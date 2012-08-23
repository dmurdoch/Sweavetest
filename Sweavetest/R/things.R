getglobal <- function(var, default) {
  if (missing(var)) {
	name <- deparse(substitute(var))
	if (exists(name, globalenv())) var <- get(name, globalenv())
	else var <- default
  }
  var
}

things <-
function(..., Correct=NA, KeepLast=0, testversion, randomize, Answers,Index, QuestionCounter,QuestionIndex) {
  
  testversion <- getglobal(testversion, 1)
  randomize <- getglobal(randomize, FALSE)
  Answers <- getglobal(Answers, c())
  Index <- getglobal(Index,c())
  QuestionCounter <- getglobal(QuestionCounter,0)
  QuestionIndex <- getglobal(QuestionIndex,c())
    
  x <- unlist(list(...))
  n <- length(x) - KeepLast
  rand <- sample(n)
  if (!randomize) rand <- 1:n  
  indices <- c(if (n) perms[[n]][[testversion]][rand], n+seq_len(KeepLast))
  if (!is.na(Correct)) {
#    x[Correct] <- paste("\\Correct", x[Correct])
    .GlobalEnv$Answers <- c(Answers, which(indices == Correct))
  }
  .GlobalEnv$LastIndices <- indices
  
  x <- x[indices]
  
  QuestionCounter <- QuestionCounter + 1
  QuestionCounter <<- QuestionCounter
  QuestionIndex <<- c(QuestionIndex,QuestionCounter)
  
  cat(x, collapse="\n")
  
  if(length(indices)<5){
    indices <- c(indices,rep(NA,5-length(indices)))
  }
  
  Index <<- rbind(Index, indices)
}

