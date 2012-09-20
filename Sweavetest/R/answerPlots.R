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
       xlab="Student Score", ylab="Correlation", main="Answer Correlations")
  text(questionRate, correlations, label = qs)
}

answerPlots <- function(Student, Correct, version, QuestionCount,
                        qs=seq_len(max(nchar(Correct))),
                        decreasing=FALSE,
                        col.wrong="pink", col.correct="white"){
  ns <- length(Student)
  student <- answerMatrix(Student, qs)
  correct <- answerMatrix(Correct, qs)
  Index <- getglobal(Index, c())
  CorrectIndex <- getglobal(CorrectIndex, c())
  GradedTests <- getglobal(GradedTests,c())
  
  qs <- unique(Index$Question)
  versions <- unique(Index$ExamCode)
  
  StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
  NumQ <- length(qs)
  
  MatrixSize <- 5 * nrow(Index)
  
  AnswerCountMatrix <- matrix(0, nrow=nrow(Index), ncol=5)
  
  for(i in seq_along(versions)){
    for(j in seq_along(qs)){
      Q <- Index[Index$Question == qs[j] & Index$ExamCode == versions[i],]
      ExamCode <- Q$ExamCode
      NumOpt <- 5
      if(is.na(Q$E)){
        NumOpt <- NumOpt-1
      }
      for(k in 1:NumOpt){
        Position <- which(Q == k)
        if (!length(Position)) browser()
        
        Position <- Position - 1
        
        if(Position == 1){
          CorrectNum <- "A"
        }
        
        if(Position == 2){
          CorrectNum <- "B"
        }
        
        if(Position == 3){
          CorrectNum <- "C"
        }
        
        if(Position == 4){
          CorrectNum <- "D"
        }
        
        if(Position == 5){
          CorrectNum <- "E"
        }
        StudentsInVersion <- which(GradedTests[,3]==ExamCode)
        StudentVersionAnswers <- StudentAnswers[StudentsInVersion,j]
        NumCorrAns <- length(which(StudentVersionAnswers == CorrectNum))
        AnswerCountMatrix[j+NumQ*(i-1),k] <- NumCorrAns
      }
      if(NumOpt < 5){
        AnswerCountMatrix[j+NumQ*(i-1),5] <- NA
      }
    }
  }
  
  AnswerCountMatrix <- cbind(Index[,1], AnswerCountMatrix)
  
  scores <- student == correct

  if (missing(version)) {
    versions <- unique(correct)
    version <- correct
  } else
    versions <- unique(version)
  
  o <- QuestionCount
  
  for (i in o) {
    for (j in seq_len(length(versions))) {
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
        title <- paste("Q", qs[i], " ", round(100*sum(scores[,i])/ns), "% correct",
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
