answerkey <- function(symbols=letters, group=5, perline=6) {
  code <- versioncode()
  master <- code == "MASTER"
  if (master)
    code <- Index()[1,"ExamCode"]
  index <- Index()
  index <- index[index$ExamCode == code,]
  n <- nrow(index)
  answers <- numeric(n)
  for (i in seq_len(n))
    if (master)
      answers[i] <- index[i, "Correct"]
    else
      answers[i] <- which( index[i, LETTERS[1:5]] == index[i, "Correct"] )
    
  line <- 0:(n - 1) %/% (group*perline) + 1
  gp <- 0:(length(line) - 1) %/% group + 1
  pos <- 0:(length(line) - 1) %% group + 1

  cat("\\begin{tabular}{", paste(rep("l", min(max(gp),perline)), collapse=""), "}\n", sep="")
  for (i in seq_len(max(line))) {
     keep <- line == i
     key <- matrix(" ", max(gp[keep]), group)
     key[cbind(gp[keep], pos[keep])] <- symbols[answers[keep]]     
     cat(paste(((min(gp[keep])-1):(max(gp[keep])-1))*group + 1, collapse=" & "), "\\\\\n", sep="")
     cat(paste( apply(key[min(gp[keep]):max(gp[keep]),,drop=FALSE], 1, paste, collapse=""), collapse=" & "), "\\\\\n", sep="")
  }
  cat("\\end{tabular}\n")
}