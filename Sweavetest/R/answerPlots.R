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
  Index <- Index[,-c(1,2)]
  CorrectIndex <- getglobal(CorrectIndex, c())
  CorrectIndex <- CorrectIndex[,-1]
  GradedTests <- getglobal(GradedTests,c())
  
  qs <- seq_len(max(nchar(GradedTests$Correct)))
  StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
  NumQ <- max(qs)
  
  MatrixSize <- (ncol(Index)-1) * nrow(Index)
  
  Index <- as.matrix(Index)
  
  AnswerCountMatrix <- matrix(rep(0,MatrixSize), nrow=nrow(Index), ncol=ncol(Index)-1)
  
  for(i in 1:NumberOfVersions){
    for(j in 1:NumQ){
      Q <- Index[j+NumQ*(i-1),]
      ExamCode <- Q[1]
      NumOpt <- ncol(Index)-1
      if(is.na(Q[6])){
        NumOpt <- NumOpt-1
      }
      for(k in 1:NumOpt){
        Position <- which(Q == k)
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
      WhichCorrect <- which(CorrectIndex == v)
      CorrectPosition <- CorrectIndex[WhichCorrect + i]
      
      
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
      
        if(length(QIndex) == 5){
          axis(1, at=centres, c(1:5))
      	}
        
        if(length(QIndex) == 4){
          axis(1, at=centres, c(1:4))
        }
        
      	axis(2, at=1:length(versions) - 1, versions, las=2)
      } else
        barplot(bar, add=TRUE, offset=j-1, col=col, axes=FALSE)
      
    }
  }
}
