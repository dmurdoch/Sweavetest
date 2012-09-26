DifficultyRating <- function(GradedTests, Questions = qs){

  qs<-seq_len(max(nchar(GradedTests$Correct)))
  StudentAnswers <- answerMatrix(GradedTests$Answers,qs)[,Questions, drop=FALSE]
  CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)[,Questions, drop=FALSE]
  QuestionScores <- StudentAnswers == CorrectAnswers

  DiffRate <- 0

  for(i in 1:ncol(QuestionScores)){

    DiffRate[i] <- sum(QuestionScores[,i])/nrow(QuestionScores)

  } 

  return(DiffRate)

}
