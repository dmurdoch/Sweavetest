Conclusion <- function(END = TRUE, Version){
  
  Version <- getglobal(Version, "Student")
  
  if(END == TRUE){
    
  if(Version == "Teacher"){
    cat("Answer key for version", ExamNum[testversion], ":")
    cat("\\\\")
    answerkey(symbols=LETTERS)
  }
  
  Appendix(Version)
  
  if(Version == "Student") {
    with(.STEnv, {
      QuestionIndex <- paste("Q", QuestionIndex, sep="")
      Code <- strsplit(versioncodes[testversion], " ")
      Code <- Code[[1]][2]
      ExamCode <- rep(Code, QuestionCounter)
      CorrectIndex <- correct
      Index <- cbind(QuestionIndex,ExamCode,Index)
    })  
    write.table(.STEnv$CorrectIndex, "CorrectIndex.dat", append=TRUE, col.names=FALSE)
    write.table(.STEnv$Index,"TestIndex.dat", append=TRUE,col.names=FALSE)
  }
  }
}