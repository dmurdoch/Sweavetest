answerCorrelations <- function(student, correct, qs=seq_len(max(nchar(correct)))) {
  student <- answerMatrix(student, qs)
  correct <- answerMatrix(correct, qs)
  scores <- student == correct
  studentTotal <- rowSums(scores)
  questionRate <- colSums(scores)/nrow(scores)
  correlations <- apply(scores, 2, function(x) if (sd(x) > 0) cor(x, studentTotal) else 0)
  plot(questionRate, correlations, type="n", 
       xlab="Student Score", ylab="Correlation", main="Answer Correlations")
  text(questionRate, correlations, label = qs)
}

answerPlots <- function(Questions=seq_len(NumQ),
                        qs=seq_len(max(nchar(Correct))),
                        decreasing=FALSE,
                        col.wrong="pink", col.correct="white"){
                        
  # GradedTests$Answers is vector of student answers, e.g. c("DBDCBDACCBAABBCBCCABBDAACAAACC", ...
  # student splits it out into matrix with questions as columns, students as rows
  # GradedTests$Correct is vector of correct answers, e.g. c("ABDCBDACCBAACACBACBDBAEACAAACE", ...
  # correct splits it out into matrix
  # version is vector of test versions, e.g. c("840", "840", "170", ...
  # QuestionCount is ??
  # Index is dataframe giving coding for tests, with columns Question, ExamCode, A, B, C, D, E
  # CorrectIndex is similar indicating the right answer, columns ExamCode, Question, Correct
  #   the Correct column gives the answer number on the original prerandomization test
  # GradedTests is a dataframe with results in columns "Student ID", Section, ExamCode, Sheet, Scantron,
  #   Answers, Correct, Grade

  Index <- getglobal(Index, c())
  CorrectIndex <- getglobal(CorrectIndex, c())
  GradedTests <- getglobal(GradedTests,c())
  NumS <- nrow(GradedTests)
  qs <- as.numeric(unique(Index$Question))
  NumQ <- length(qs)
  
  student <- answerMatrix(GradedTests$Answers, qs)
  correct <- answerMatrix(GradedTests$Correct, qs)
  
  versions <- as.character(unique(Index$ExamCode))
  NumV <- length(versions)
  
  Counts <- AnswerCounts(GradedTests, qs)
  
  scores <- student == correct

  
  for (i in Questions) {
    for (j in seq_len(NumV)) {
      v <- versions[j]
      sub <- v == version
      
      WhichIndex <- which(AnswerCountMatrix[,1] == v)
      AnswerCountIndex <- AnswerCountMatrix[WhichIndex,]
      AnswerCounts <- AnswerCountIndex[i,]
      AnswerCounts <- AnswerCounts[-1]
      drop <- which(is.na(AnswerCounts))
      if(length(drop)>0) AnswerCounts <- AnswerCounts[-drop]
      CorrectPosition <- CorrectIndex[ExamCode == v,"Correct"]
      
      
      RightIndex <- which(Index[,1]==v)
      EIndex <- Index[RightIndex,]
      QIndex <- EIndex[o,]
      QIndex <- QIndex[-1]
  
      if(is.na(QIndex[5]) == FALSE){
      bar <- c("1"=0, "2"=0, "3"=0, "4"=0, "5"=0)
      col <- rep(col.wrong, 5)
      }
  
      if(is.na(QIndex[5]) == TRUE){
        QIndex <- QIndex[-5]
        bar <- c("1"=0, "2"=0, "3"=0, "4"=0)
        col <- rep(col.wrong, 4)
      }
      
      names(QIndex) <- names(bar)
      names(col) <- names(bar)
      col[CorrectPosition] <- col.correct

  
      bar <- AnswerCounts/sum(sub)  
      
      if (v == versions[[1]]) {
        title <- paste("Q", qs[i], " ", round(100*sum(scores[,i])/NumS), "% correct",
                       sep="")
      	centres <- barplot(bar, ylim=c(0, length(versions)), main=title, 
      	                   col=col, axes=FALSE)
      
      	axis(2, at=1:length(versions) - 1, versions, las=2)
      } else
        barplot(bar, add=TRUE, offset=j-1, col=col, axes=FALSE)
        
      text(x=centres, y=rep(j-1, length(QIndex)),adj=c(0.5, 1.3),labels=LETTERS[order(QIndex)], xpd=NA)
      
    }
  }
}
