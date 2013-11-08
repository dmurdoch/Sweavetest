CalcTotalGrade <- function(MCGrades = GradedTests(), LAGrades = LSGrades()){
  
  LAGrade <- rep(0, nrow(MCGrades))
  TotalGrade <- rep(0,nrow(MCGrades))
  
  for(i in 1:nrow(MCGrades)){
    Index <- which(LAGrades[,1] == MCGrades[i,1])
    LAGrade[i] <- sum(LAGrades[Index,-1])
    TotalGrade[i] <- MCGrades[i,8] + LAGrade[i]
  }
  
  GradedTests(cbind(GradedTests(),LAGrade,TotalGrade))
  
}