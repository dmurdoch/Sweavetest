answerkey <- function(Answers, symbols=letters, group=5, perline=6) {
  Answers <- getglobal(Answers, numeric(0))
  line <- 0:(length(Answers) - 1) %/% (group*perline) + 1
  gp <- 0:(length(Answers) - 1) %/% group + 1
  pos <- 0:(length(Answers) - 1) %% group + 1

  cat("\\begin{tabular}{", paste(rep("l", min(max(gp),perline)), collapse=""), "}\n", sep="")
  for (i in seq_len(max(line))) {
     keep <- line == i
     key <- matrix(" ", max(gp[keep]), group)
     key[cbind(gp[keep], pos[keep])] <- symbols[Answers[keep]]     
     cat(paste(((min(gp[keep])-1):(max(gp[keep])-1))*group + 1, collapse=" & "), "\\\\\n", sep="")
     cat(paste( apply(key[min(gp[keep]):max(gp[keep]),,drop=FALSE], 1, paste, collapse=""), collapse=" & "), "\\\\\n", sep="")
  }
  cat("\\end{tabular}\n")
}