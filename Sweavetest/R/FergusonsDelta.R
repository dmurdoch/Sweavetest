FergusonsDelta <- function(GradedTests){

  qs<-seq_len(max(nchar(GradedTests$Correct)))
  StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
  CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)
  QuestionScores <- StudentAnswers == CorrectAnswers
  StudentScores <- rowSums(QuestionScores)
  
  GradeCount <- rep(0,ncol(QuestionScores))
  
  for(i in 0:ncol(QuestionScores)){
  
    GradeCount[i+1] <- sum(StudentScores == i)
  }
  
  N <- nrow(QuestionScores)
  K <- ncol(QuestionScores)
  
  FD <- (N^2 - sum(GradeCount^2))/(N^2 - N^2/(K+1))
  
  return(FD)
}
