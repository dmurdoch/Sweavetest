horiz <-
function(..., Correct=1, KeepLast=0, CheckDups, testversion, randomize, itemlabels, Answers) {
  
  testversion <- getglobal(testversion, 1)
  randomize <- getglobal(randomize, FALSE)
  itemlabels <- getglobal(itemlabels, paste("(", letters, ")", sep=""))
  Answers <- getglobal(Answers, c())
  CheckDups <- getglobal(CheckDups, TRUE)
  
  x <- unlist(list(...))
  if (CheckDups && any(duplicated(format(x))))  
    stop("Duplicated answers in Q", length(Answers)+1, ": ", 
         paste(format(x), collapse=" ")) 
  n <- length(x)-KeepLast  
  rand <- sample(n)  
  if (!randomize) rand <- 1:n  
  indices <- c(if (n) perms[[n]][[testversion]][rand], n+seq_len(KeepLast))
  if (!is.na(Correct)) {
    x[Correct] <- paste("\\Correct", x[Correct])
    Answers <<- c(Answers, which(indices == Correct))
  }
  LastIndices <<- indices
  labels <- itemlabels[1:(n+KeepLast)]
  x <- x[indices]
  cat( paste(labels, "\\hspace{1ex}", x, "\\hfill") )
}
