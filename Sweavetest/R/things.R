getglobal <- function(var, default) {
  if (missing(var)) {
	name <- deparse(substitute(var))
	if (exists(name, globalenv())) var <- get(name, globalenv())
	else var <- default
  }
  var
}

things <-
function(..., Correct=NA, KeepLast=0, testversion, randomize, Answers) {
  
  testversion <- getglobal(testversion, 1)
  randomize <- getglobal(randomize, FALSE)
  Answers <- getglobal(Answers, c())
  
  x <- unlist(list(...))
  n <- length(x) - KeepLast
  rand <- sample(n)
  if (!randomize) rand <- 1:n  
  indices <- c(if (n) perms[[n]][[testversion]][rand], n+seq_len(KeepLast))
  if (!is.na(Correct)) {
#    x[Correct] <- paste("\\Correct", x[Correct])
    Answers <<- c(Answers, which(indices == Correct))
  }
  LastIndices <<- indices
  x <- x[indices]
  cat(x, collapse="\n")
}

