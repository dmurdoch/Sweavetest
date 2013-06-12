Conclusion <- function(){
  
    if(Version() == "Teacher"){
      cat("\\newpage\n")
      oldtv <- testversion()
      for (tv in seq_len(NumVersions()+1)-1) {
        testversion(tv)
        cat("Answer key for version", versioncode(), ":")
        cat("\\\\")
        answerkey(symbols=LETTERS)
        cat("\n")
      }
      testversion(oldtv)
    }
    
    Appendix()
    
    if(Version() == "Student") 
      write.csv(Index(),paste0(TestName(), "Index.csv"), row.names = FALSE)
}