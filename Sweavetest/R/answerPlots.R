answerCorrelations <- function(gradedTests = GradedTests(), qs=seq_len(max(nchar(gradedTests$Correct)))) {
  student <- answerMatrix(gradedTests$Answers, qs)
  answers <- answerMatrix(gradedTests$Correct, qs)
  scores <- student == answers
  studentTotal <- gradedTests$Grade
  questionRate <- colSums(scores)/nrow(scores)
  correlations <- apply(scores, 2, function(x) if (sd(x) > 0) cor(x, studentTotal) else 0)
  plot(questionRate, correlations, type="n", 
       xlab="Student Score", ylab="Correlation", main="Answer Correlations")
  text(questionRate, correlations, label = qs)
}

answerPlots <- function(Questions=qs,
                        col.wrong="pink", col.correct="white"){
                        
  # GradedTests$Answers is vector of student answers, e.g. c("DBDCBDACCBAABBCBCCABBDAACAAACC", ...
  # GradedTests$Correct is vector of correct answers, e.g. c("ABDCBDACCBAACACBACBDBAEACAAACE", ...
  # Index is dataframe giving coding for tests, with columns Question, ExamCode, Correct, A, B, C, D, E
  # GradedTests is a dataframe with results in columns "Student ID", Section, ExamCode, Sheet, Scantron,
  #   Answers, Correct, Grade

  qs <- as.numeric(unique(Index()$Question))
  
  versions <- as.character(unique(Index()$ExamCode))
  NumV <- length(versions)
  
  summary <- CreateIndex()
  for (i in Questions) {
    Qsummary <- with(summary, summary[Question == i,])
    if (all(is.na(Qsummary$A5))) NumResp <- 4
    else NumResp <- 5
    names <- paste0("A", 1:NumResp)
    if (any(Qsummary$Blank > 0))
    	names <- c(names, "Blank")
    if (any(Qsummary$Bad > 0))
    	names <- c(names, "Bad")
    max <- max(Qsummary[, names])
    score <- sum(Qsummary[, "Score"])
    NumS <- sum(Qsummary[, names])
    
    for (j in seq_len(NumV)) {
      v <- versions[j]
      Vsummary <- with(Qsummary, Qsummary[ExamCode == v,])
      bar <- 0.75*as.numeric(Vsummary[,names]/max)
      names(bar) <- c("A1"="1", "A2"="2", "A3"="3", "A4"="4", "A5"="5",
                    "Blank"="Blank", "Bad"="Bad")[names]
      col <- rep(col.wrong, length(bar))      
      names(col) <- names(bar)
      col[Vsummary$Correct] <- col.correct
      
      if (j == 1) {
        title <- paste("Q", qs[i], " ", round(100*score/NumS), "% correct",
                       sep="")
      	centres <- barplot(bar, ylim=c(0, length(versions)), main=title, 
      	                   col=col, axes=FALSE)
      
      	axis(2, at=1:length(versions) - 1, versions, las=2)
      } else
        barplot(bar, add=TRUE, offset=j-1, col=col, axes=FALSE)
        
      text(x=centres[1:NumResp], y=rep(j-1, NumResp),adj=c(0.5, 1.3),
           labels=Vsummary[1,paste0("R", 1:NumResp)], xpd=NA)
      
    }
  }
}
