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
        Correct <- correct()
        if(testversion() == 1)
          write.csv(Index(),"TestIndex.csv", row.names = FALSE)
        else
          write.table(Index(),"TestIndex.csv", append=TRUE, row.names = FALSE, 
            col.names = FALSE, sep=",", dec=".", qmethod="double")
      })      
    }
  }
}