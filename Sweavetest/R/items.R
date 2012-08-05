items <-
function(..., Correct=1, KeepLast=0, CheckDups, Answers) {
  Answers <- getglobal(Answers, c())
  CheckDups <- getglobal(CheckDups, TRUE)
  x <- unlist(list(...))
  if (CheckDups && any(duplicated(format(x))))  
    stop("Duplicated answers in Q", length(Answers)+1, ": ", 
         paste(format(x), collapse=" "))   
  if (!is.na(Correct))
    x[Correct] <- paste("\\Correct", x[Correct])
  things(paste("\\item", x, "\n"), KeepLast=KeepLast, Correct=Correct, 
         Answers=Answers)
}

enumerate <- function(..., Correct = 1, KeepLast = 0) {
  cat("\\begin{enumerate}\n")
  items(..., Correct=Correct, KeepLast=KeepLast)
  cat("\\end{enumerate}\n")
}