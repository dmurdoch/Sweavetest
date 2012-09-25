Conclusion <- function(END = TRUE){
  
  if(END == TRUE){
    if(Version() == "Teacher"){
      cat("Answer key for version", versioncode(), ":")
      cat("\\\\")
      answerkey(symbols=LETTERS)
    }
    
    Appendix()
    
    if(Version() == "Student") {
      with(.STEnv, {
        Code <- as.integer(versioncode())
        ExamCode <- rep(Code, QuestionCounter)
        Correct <- correct
        Index <- cbind(QuestionIndex,ExamCode,Correct,Index)
        if(testversion() == 1){
          colnames(Index) <- c("Question", "ExamCode", "Correct", "A", "B", "C", "D", "E")
          write.table(Index,"TestIndex.csv", append=TRUE, row.names = FALSE, col.names = TRUE)
        }
        else{write.table(Index,"TestIndex.csv", append=TRUE, row.names = FALSE, col.names = FALSE)}
      })      
    }
  }
}