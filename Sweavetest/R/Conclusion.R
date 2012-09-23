Conclusion <- function(END = TRUE, Version = getglobal(Version, "Student")){
  
  if(END == TRUE){
    
  if(Version == "Teacher"){
    cat("Answer key for version", ExamNum[testversion], ":")
    cat("\\\\")
    answerkey(symbols=LETTERS)
  }
  
  Appendix(Version)
  
  if(Version == "Student") {
    with(.STEnv, {
      Code <- strsplit(versioncodes[testversion], " ")
      Code <- as.integer(Code[[1]][2])
      ExamCode <- rep(Code, QuestionCounter)
      Correct <- correct
      Index <- cbind(QuestionIndex,ExamCode,Correct,Index)
      if(testversion == 1){
        colnames(Index) <- c("Question", "ExamCode", "Correct", "A", "B", "C", "D", "E")
        write.table(Index,"TestIndex.csv", append=TRUE, row.names = FALSE, col.names = TRUE)
      }
      else{write.table(Index,"TestIndex.csv", append=TRUE, row.names = FALSE, col.names = FALSE)}
    })      
  }
  }
}