DistractorDiscrimination <- function(
   GradedTests = getglobal(GradedTests, NULL),
   QuestionCounter = getglobal(QuestionCounter, 0),
   Index = getglobal(Index, NULL)){
  
  Index <- Index[,LETTERS[1:5]]
  qs <- seq_len(max(nchar(GradedTests$Correct[1])))
  Student <- answerMatrix(GradedTests$Answers, qs)
  
  Quantile75 <- quantile(GradedTests$Grade, .75)
  Quantile25 <- quantile(GradedTests$Grade, .25)
  Bottom25 <- which(GradedTests$Grade <= Quantile25)
  Top25 <- which(GradedTests$Grade >= Quantile75)
  NumBottom25 <- length(Bottom25)
  NumTop25 <- length(Top25)
  
  Qlength <- length(Index[QuestionCounter,]) - 1 
  
  if(is.na(Index[QuestionCounter,6]) == TRUE){
    Qlength <- Qlength - 1
  }
  
  Top25Counter <- rep(0,Qlength)
  Bottom25Counter <- rep(0,Qlength)
  
  NewIndex <- matrix(rep(0, ncol(Index)*nrow(Index)), nrow = nrow(Index), ncol=ncol(Index))
  NewIndex[,1] <- Index[,1]
  
  for(i in 1:nrow(Index)){
    for(j in 2:ncol(Index)){
      if(is.na(Index[i,j]) == FALSE){
        if(Index[i,j] == "1") {NewIndex[i,2] <- LETTERS[j-1]}
        if(Index[i,j] == "2") {NewIndex[i,3] <- LETTERS[j-1]}
        if(Index[i,j] == "3") {NewIndex[i,4] <- LETTERS[j-1]}
        if(Index[i,j] == "4") {NewIndex[i,5] <- LETTERS[j-1]}
        if(Index[i,j] == "5") {NewIndex[i,6] <- LETTERS[j-1]}
      }
      if(is.na(Index[i,j])) {Index[i,j] <- NA}
    }
  }
  
  for(i in 1:NumTop25){
    CurrentTop25 <- Top25[i]
    StudentExamCode <- GradedTests$ExamCode[CurrentTop25]
    StudentIndex <- which(NewIndex[,1] == StudentExamCode)
    QuestionIndex <- NewIndex[StudentIndex,]
    QuestionIndex <- QuestionIndex[QuestionCounter,]
    drop <- which(is.na(QuestionIndex))
    
    if(length(drop)>0){
      QuestionIndex <- QuestionIndex[-drop]
    }
    
    QuestionIndex <- QuestionIndex[-1]
    
    Count <- which(QuestionIndex == Student[CurrentTop25,QuestionCounter])
    Top25Counter[Count] <- Top25Counter[Count]+1
  }
  
    for(j in 1:NumBottom25){
      CurrentBottom25 <- Bottom25[j]
      StudentExamCode <- GradedTests$ExamCode[CurrentBottom25]
      StudentIndex <- which(NewIndex[,1] == StudentExamCode)
      QuestionIndex <- NewIndex[StudentIndex,]
      QuestionIndex <- QuestionIndex[QuestionCounter,]
      drop <- which(is.na(QuestionIndex))
      
      if(length(drop)>0){
        QuestionIndex <- QuestionIndex[-drop]
      }
      
      QuestionIndex <- QuestionIndex[-1]
      
      Count <- which(QuestionIndex == Student[CurrentBottom25,QuestionCounter])
      Bottom25Counter[Count] <- Bottom25Counter[Count]+1
    
  }
  
  Top25Percentage <- Top25Counter/NumTop25
  Bottom25Percentage <- Bottom25Counter/NumBottom25
    
  Distractor <- Top25Percentage-Bottom25Percentage
  return(Distractor)
}