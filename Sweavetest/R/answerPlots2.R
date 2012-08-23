answerPlots2 <- function(student, correct, version, Question,
                        qs=seq_len(max(nchar(correct))),
                        decreasing=FALSE,
                        col.wrong="pink", col.correct="white"){
  ns <- length(student)
  student <- answerMatrix(student, qs)
  correct <- answerMatrix(correct, qs)
  
  scores <- student == correct
  o <- Question
  
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
