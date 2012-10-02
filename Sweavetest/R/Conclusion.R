Conclusion <- function(){
  
    if(Version() == "Teacher"){
      cat("\\newpage\n")
      cat("Answer key for version", versioncode(), ":")
      cat("\\\\")
      answerkey(symbols=LETTERS)
    }
    
    Appendix()
    
    if(Version() == "Student") {
        if(testversion() == 1)
          write.csv(Index(),paste0(TestName(), "Index.csv"), row.names = FALSE)
        else
          write.table(Index(),paste0(TestName(), "Index.csv"), append=TRUE, row.names = FALSE, 
            col.names = FALSE, sep=",", dec=".", qmethod="double")
    }
}