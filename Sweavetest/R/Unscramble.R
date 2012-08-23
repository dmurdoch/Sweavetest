Unscramble <- function(scanex, ExamCodes = unique(scanex$ExamCode), Orders, Scrambled = FALSE){
  
  if(Scrambled != FALSE){
    OrderMatrix <- cbind(ExamCodes, Orders)
    GradedTests <- grades(scanex)
    qs <- seq_len(max(nchar(GradedTests$Correct[1])))
    Keys <- which(scanex$"Student ID" == "999999999")
    StudentScanex <- scanex[-Keys,]
    CorrectScanex <- scanex[Keys,]
    StudentAnswers <- answerMatrix(StudentScanex$Answers, qs)
    CorrectAnswers <- answerMatrix(CorrectScanex$Answers, qs)
    RightAnswerOrder <- rep(0,max(qs))
    RightCorrectOrder <- rep(0, max(qs))
    CorrectedScanex <- c()
    
    for(i in ExamCodes){
      RightStudentAnswers <- StudentAnswers[which(StudentScanex$ExamCode == i),]
      RightCorrectAnswers <- CorrectAnswers[which(CorrectScanex$ExamCode == i),]
      WhichExam <- which(OrderMatrix[,1] == i)
      RightExamOrder <- OrderMatrix[WhichExam,]
      RightExamOrder <- RightExamOrder[-1]
      RightAnswerOrder <- matrix(rep(0, max(qs)*nrow(RightStudentAnswers)), ncol=max(qs), nrow=nrow(RightStudentAnswers))
      
    for(j in 1:length(RightExamOrder)){
        RightAnswerOrder[,j] <- RightStudentAnswers[,which(RightExamOrder == j)]
        RightCorrectOrder[j] <- RightCorrectAnswers[which(RightExamOrder == j)]
      }
      
      CorrectedScanex <- c()
      
      for(k in 1:nrow(RightAnswerOrder)){
        ProperAnswers <- paste(RightAnswerOrder[k,], collapse="")
        CorrectedScanex <- rbind(CorrectedScanex,ProperAnswers)
      }
      RightCorrectedOrder <- paste(RightCorrectOrder, collapse="")
      row.names(CorrectedScanex) <- NULL
      WhichStudentScanex <- which(StudentScanex$ExamCode == i)
      WhichCorrectScanex <- which(CorrectScanex$ExamCode == i)
      StudentScanex$Answers[WhichStudentScanex] <- CorrectedScanex
      CorrectScanex$Answers[WhichCorrectScanex] <- RightCorrectedOrder
      
    }
    
    NewAnswers <- rbind(CorrectScanex,StudentScanex)
    scanex$Answers <- NewAnswers
  }
  return(scanex)
}