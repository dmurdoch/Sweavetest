answerMatrix <- function(answers, qs=seq_len(max(nchar(answers)))) {
  answerS <- strsplit(answers, "")
  len <- max(qs)
  answerS <- lapply(answerS,  
                function(x) {
                  if (length(x) < len) 
                    x <- c(x, rep(" ", len-length(x)))
                  x[qs]
                }
             )       
  do.call(rbind, answerS)
}

answerCorrelations <- function(student, correct, qs=seq_len(max(nchar(correct)))) {
  student <- answerMatrix(student, qs)
  correct <- answerMatrix(correct, qs)
  scores <- student == correct
  studentTotal <- rowSums(scores)
  questionRate <- colSums(scores)/nrow(scores)
  correlations <- apply(scores, 2, function(x) if (sd(x) > 0) cor(x, studentTotal) else 0)
  plot(questionRate, correlations, type="n", 
       xlab="Student Score", ylab="Correlation")
  text(questionRate, correlations, label = qs)
}

answerPlots <- function(student, correct, version, 
                        qs=seq_len(max(nchar(correct))),
                        decreasing=FALSE,
                        col.wrong="pink", col.correct="white"){
  ns <- length(student)
  student <- answerMatrix(student, qs)
  correct <- answerMatrix(correct, qs)
  
  scores <- student == correct
  if (is.na(decreasing))
    o <- seq_len(ncol(scores))
  else
    o <- order(colSums(scores), decreasing=decreasing)
  if (missing(version)) {
    versions <- unique(correct)
    version <- correct
  } else
    versions <- unique(version)
  
  for (i in o) {
    for (j in seq_len(length(versions))) {
      v <- versions[j]
      sub <- v == version
      bar <- c(A=0, B=0, C=0, D=0, E=0)
      col <- rep(col.wrong, 5)
      names(col) <- names(bar)
      col[unique(correct[sub,i])] <- col.correct
      for (a in names(bar)) 
      	bar[a] <- sum(student[sub,i] == a)/sum(sub)
      if (v == versions[[1]]) {
        title <- paste("Q", qs[i], " ", round(100*sum(scores[,i])/ns), "% correct",
                       sep="")
      	centres <- barplot(bar, ylim=c(0, length(versions)), main=title, 
      	                   col=col, axes=FALSE)
      	axis(1, at=centres, LETTERS[1:5])
      	axis(2, at=1:length(versions) - 1, versions, las=2)
      } else
        barplot(bar, add=TRUE, offset=j-1, col=col, axes=FALSE)
      
    }
  }
}
