Unscramble <- function(scanex, Orders){
  
  ExamCodes <- colnames(Orders)
  
  qs <- seq_along(Orders[,1])
  Answers <- answerMatrix(scanex$Answers, qs)
  
  for(i in seq_along(ExamCodes)){
    ThisCode <- which(scanex$ExamCode == ExamCodes[i])
    subset <- Answers[ThisCode,,drop=FALSE]
    ThisOrder <- Orders[,i]
    Answers[ThisCode,] <- subset[,ThisOrder]
  }  
  
  scanex$Answers <- apply(Answers, 1, function(row) paste(row, collapse=""))
  
  scanex
}